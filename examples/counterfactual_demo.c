/**
 * @file counterfactual_demo.c
 * @brief 反事实推理实例演示
 *
 * 反事实推理（Counterfactual Reasoning）核心问题：
 *   "若异常 A 未发生，失败 F 是否仍会发生？"
 *
 * 若移除 A 后无法找到因果链 → A 对 F 是"因果必要"的，支持根因结论。
 *
 * 本演示构造两个场景：
 * 1. 单一根因：温度异常是唯一解释 → 反事实（无该异常）无链 → 异常必要
 * 2. 多因可选：温度与压力异常都可解释 → 反事实（无温度）仍有链 → 温度非严格必要
 *
 * 运行后生成 counterfactual_demo.html 可视化网页。
 */

#include "../include/kos_core.h"
#include "../include/kos_kernel.h"
#include "../include/kos_manufacturing.h"
#include "../include/kos_causal_trace.h"
#include "../include/kos_knowledge_base.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

#define TIME_BASE 1738195200ULL  /* 2026-01-30 00:00:00 UTC */

/* ========== 场景 1：单一根因（温度异常是唯一解释） ========== */

static kos_knowledge_base_t* scenario_single_root_cause(const char** out_cf_result) {
    printf("\n========== 场景 1：单一根因 ==========\n");
    printf("知识：工艺步骤、温度异常、失败事件。温度异常是唯一可解释失败的原因。\n\n");

    kos_term* initial_K = kos_mk_prop("InitialKB");
    kos_state_t* sigma = kos_state_create(initial_K);
    kos_term_free(initial_K);
    if (!sigma) return NULL;

    kos_knowledge_base_t* kb = kos_kb_create();
    if (!kb) { kos_state_free(sigma); return NULL; }
    kos_state_set_kb(sigma, kb);

    BatchID batch = {.batch_id = "Batch_26_01-00001"};
    Machine machine = {.machine_id = "HeatTreatment_05", .line_id = "Line2"};
    TimeRange step_dur = {.start = TIME_BASE - 3600, .end = TIME_BASE - 60};
    Param param_temp = {.param_name = "temperature"};
    ParamValue pv_temp = {.param = param_temp, .value = 185.2};
    Time anomaly_time = TIME_BASE - 1800;
    ErrorCode err = {.code = "UNQUALIFIED"};

    /* 添加工艺步骤、温度异常、失败事件到 K */
    kos_term* proc = kos_mk_proc_step(batch, machine, step_dur);
    kos_term* anom = kos_mk_anomaly(machine, param_temp, pv_temp, anomaly_time);
    kos_term* fail = kos_mk_fail_event(batch, err, TIME_BASE);

    if (proc) { sigma->K = kos_update_knowledge(sigma->K, kos_term_copy(proc)); kos_term_free(proc); }
    if (anom) { sigma->K = kos_update_knowledge(sigma->K, kos_term_copy(anom)); kos_term_free(anom); }
    if (fail) { sigma->K = kos_update_knowledge(sigma->K, kos_term_copy(fail)); kos_term_free(fail); }

    /* 构建知识库用于可视化：FailEvt → ProcStep → Anomaly */
    int ts = 0;
    kos_term* proc_t = kos_mk_proc_step(batch, machine, step_dur);
    kos_term* anom_t = kos_mk_anomaly(machine, param_temp, pv_temp, anomaly_time);
    kos_term* fail_t = kos_mk_fail_event(batch, err, TIME_BASE);
    if (fail_t) {
        kos_term* type_f = kos_mk_prop("FailEvt");
        kos_kb_add_item(kb, "fail_evt", fail_t->kind == KOS_PAIR ? fail_t->data.pair.data : fail_t,
                        type_f, ts++, KOS_KB_SOURCE_BOOTSTRAP);
        kos_term_free(fail_t);
        kos_term_free(type_f);
    }
    if (proc_t) {
        kos_term* type_p = kos_mk_prop("ProcStep");
        kos_kb_add_item(kb, "proc_step", proc_t->kind == KOS_PAIR ? proc_t->data.pair.data : proc_t,
                        type_p, ts++, KOS_KB_SOURCE_BOOTSTRAP);
        kos_term_free(proc_t);
        kos_term_free(type_p);
    }
    if (anom_t) {
        kos_term* type_a = kos_mk_prop("Anomaly");
        kos_kb_add_item(kb, "temp_anomaly", anom_t, type_a, ts++, KOS_KB_SOURCE_BOOTSTRAP);
        kos_term_free(anom_t);
        kos_term_free(type_a);
    }
    kos_kb_add_dependency_typed(kb, "fail_evt", "proc_step", KOS_KB_DEP_TYPE_CAUSAL);
    kos_kb_add_dependency_typed(kb, "proc_step", "temp_anomaly", KOS_KB_DEP_TYPE_CAUSAL);

    FailEvt fail_evt = {.batch = batch, .error = err, .time = TIME_BASE, .qa_shift_proof = NULL};

    RootCauseReport* rep = kos_analyze_quality_traceability(fail_evt, sigma);
    if (rep) {
        printf("  [事实] 根因追溯: 温度异常 (%.2f°C) @ %s\n",
               rep->anomaly.value.value, rep->anomaly.machine.machine_id);

        bool cf_necessary = kos_counterfactual_test(rep->anomaly, fail_evt, sigma);
        *out_cf_result = cf_necessary
            ? "否，无法找到因果链 → 温度异常是因果必要的（支持根因结论）"
            : "是，仍能找到其他解释 → 温度异常非严格必要";
        printf("  [反事实] 若温度异常未发生，能否解释失败？\n");
        printf("           → %s\n", *out_cf_result);

        kos_root_cause_report_free(rep);
    } else {
        *out_cf_result = "未找到因果链";
    }

    kos_state_set_kb(sigma, NULL);  /* 避免 free 时释放 kb */
    kos_state_free(sigma);
    return kb;
}

/* ========== 场景 2：多因可选（温度与压力异常都可解释） ========== */

static kos_knowledge_base_t* scenario_multiple_causes(const char** out_cf_result) {
    printf("\n========== 场景 2：多因可选 ==========\n");
    printf("知识：工艺步骤、温度异常、压力异常、失败事件。两者都可解释失败。\n\n");

    kos_term* initial_K = kos_mk_prop("InitialKB");
    kos_state_t* sigma = kos_state_create(initial_K);
    kos_term_free(initial_K);
    if (!sigma) return NULL;

    kos_knowledge_base_t* kb = kos_kb_create();
    if (!kb) { kos_state_free(sigma); return NULL; }
    kos_state_set_kb(sigma, kb);

    BatchID batch = {.batch_id = "Batch_26_01-00002"};
    Machine machine = {.machine_id = "Quench_03", .line_id = "Line2"};
    TimeRange step_dur = {.start = TIME_BASE - 3600, .end = TIME_BASE - 60};
    Param param_temp = {.param_name = "temperature"};
    Param param_press = {.param_name = "pressure"};
    ParamValue pv_temp = {.param = param_temp, .value = 192.0};
    ParamValue pv_press = {.param = param_press, .value = 0.15};
    Time t_temp = TIME_BASE - 2400, t_press = TIME_BASE - 1200;
    ErrorCode err = {.code = "HARDNESS_ISSUE"};

    /* 添加工艺步骤、温度异常、压力异常、失败事件到 K */
    kos_term* proc = kos_mk_proc_step(batch, machine, step_dur);
    kos_term* anom_temp = kos_mk_anomaly(machine, param_temp, pv_temp, t_temp);
    kos_term* anom_press = kos_mk_anomaly(machine, param_press, pv_press, t_press);
    kos_term* fail = kos_mk_fail_event(batch, err, TIME_BASE);

    if (proc) { sigma->K = kos_update_knowledge(sigma->K, kos_term_copy(proc)); kos_term_free(proc); }
    if (anom_temp) { sigma->K = kos_update_knowledge(sigma->K, kos_term_copy(anom_temp)); kos_term_free(anom_temp); }
    if (anom_press) { sigma->K = kos_update_knowledge(sigma->K, kos_term_copy(anom_press)); kos_term_free(anom_press); }
    if (fail) { sigma->K = kos_update_knowledge(sigma->K, kos_term_copy(fail)); kos_term_free(fail); }

    /* 构建知识库用于可视化 */
    int ts = 0;
    kos_term* proc_t = kos_mk_proc_step(batch, machine, step_dur);
    kos_term* anom_t = kos_mk_anomaly(machine, param_temp, pv_temp, t_temp);
    kos_term* anom_p = kos_mk_anomaly(machine, param_press, pv_press, t_press);
    kos_term* fail_t = kos_mk_fail_event(batch, err, TIME_BASE);
    if (fail_t) {
        kos_term* type_f = kos_mk_prop("FailEvt");
        kos_kb_add_item(kb, "fail_evt", fail_t->kind == KOS_PAIR ? fail_t->data.pair.data : fail_t,
                        type_f, ts++, KOS_KB_SOURCE_BOOTSTRAP);
        kos_term_free(fail_t);
        kos_term_free(type_f);
    }
    if (proc_t) {
        kos_term* type_p = kos_mk_prop("ProcStep");
        kos_kb_add_item(kb, "proc_step", proc_t->kind == KOS_PAIR ? proc_t->data.pair.data : proc_t,
                        type_p, ts++, KOS_KB_SOURCE_BOOTSTRAP);
        kos_term_free(proc_t);
        kos_term_free(type_p);
    }
    if (anom_t) {
        kos_term* type_a = kos_mk_prop("Anomaly");
        kos_kb_add_item(kb, "temp_anomaly", anom_t, type_a, ts++, KOS_KB_SOURCE_BOOTSTRAP);
        kos_term_free(anom_t);
        kos_term_free(type_a);
    }
    if (anom_p) {
        kos_term* type_a = kos_mk_prop("Anomaly");
        kos_kb_add_item(kb, "press_anomaly", anom_p, type_a, ts++, KOS_KB_SOURCE_BOOTSTRAP);
        kos_term_free(anom_p);
        kos_term_free(type_a);
    }
    kos_kb_add_dependency_typed(kb, "fail_evt", "proc_step", KOS_KB_DEP_TYPE_CAUSAL);
    kos_kb_add_dependency_typed(kb, "proc_step", "temp_anomaly", KOS_KB_DEP_TYPE_CAUSAL);
    kos_kb_add_dependency_typed(kb, "proc_step", "press_anomaly", KOS_KB_DEP_TYPE_CAUSAL);

    FailEvt fail_evt = {.batch = batch, .error = err, .time = TIME_BASE, .qa_shift_proof = NULL};
    Anomaly anomaly_temp = {.machine = machine, .param = param_temp, .value = pv_temp, .time = t_temp};

    RootCauseReport* rep = kos_analyze_quality_traceability(fail_evt, sigma);
    if (rep) {
        printf("  [事实] 根因追溯: %s 异常 (%.2f) @ %s\n",
               rep->anomaly.param.param_name, rep->anomaly.value.value, rep->anomaly.machine.machine_id);

        bool cf_temp_necessary = kos_counterfactual_test(anomaly_temp, fail_evt, sigma);
        *out_cf_result = cf_temp_necessary
            ? "否，无法找到因果链 → 温度异常是因果必要的"
            : "是，压力异常仍可解释 → 温度异常非严格必要（存在替代原因）";
        printf("  [反事实] 若温度异常未发生，能否解释失败？\n");
        printf("           → %s\n", *out_cf_result);

        kos_root_cause_report_free(rep);
    } else {
        *out_cf_result = "未找到因果链";
    }

    kos_state_set_kb(sigma, NULL);
    kos_state_free(sigma);
    return kb;
}

/* ========== 主流程 ========== */

int main(void) {
    printf("============================================================\n");
    printf("KOS-TL 反事实推理实例演示\n");
    printf("============================================================\n");
    printf("\n反事实推理：\"若 X 未发生，Y 是否仍会发生？\"\n");
    printf("用于验证根因的因果必要性。\n");

    const char* cf_result1 = NULL;
    const char* cf_result2 = NULL;
    kos_knowledge_base_t* kb1 = scenario_single_root_cause(&cf_result1);
    kos_knowledge_base_t* kb2 = scenario_multiple_causes(&cf_result2);

    /* 导出可视化网页 */
    if (kb1 && kb2) {
        char* html = kos_export_counterfactual_demo_html(kb1, kb2, cf_result1, cf_result2);
        if (html) {
            FILE* f = fopen("counterfactual_demo.html", "w");
            if (f) {
                fputs(html, f);
                fclose(f);
                printf("\n✓ 可视化网页已写入 counterfactual_demo.html\n");
            }
            free(html);
        }
    }

    if (kb1) kos_kb_free(kb1);
    if (kb2) kos_kb_free(kb2);

    printf("\n============================================================\n");
    printf("演示完成。\n");
    printf("============================================================\n");
    return 0;
}
