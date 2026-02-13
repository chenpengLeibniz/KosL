/**
 * @file manufacturing_kb_root_cause_demo.c
 * @brief 模拟从 20+ 制造业数据库抽取形成的知识库，通过 KOS-TL 推理相关事件并找到根因
 *
 * 场景：轴承生产线质量异常追溯
 * 1. 模拟 20+ 异构数据库（MES、ERP、质检、传感器、维护等）抽取知识
 * 2. 构建知识库，建立项间依赖（事件→工艺步骤→异常→根因）
 * 3. 收到质量事件后，KOS-TL 推理追溯根因
 * 4. 导出依赖图可视化
 */

#include "../include/kos_core.h"
#include "../include/kos_kernel.h"
#include "../include/kos_manufacturing.h"
#include "../include/kos_causal_trace.h"
#include "../include/kos_knowledge_base.h"
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define TIME_BASE 1738195200ULL  /* 2026-01-30 00:00:00 UTC */

/* ========== 模拟 20+ 制造业数据库 ========== */

typedef struct {
    const char* name;
    int item_count;
} db_sim_t;

static const char* db_names[] = {
    "MES_Production", "MES_WorkOrder", "MES_Process", "MES_Equipment",
    "ERP_Material", "ERP_Order", "ERP_BOM",
    "Quality_Inspection", "Quality_Defect", "Quality_Test",
    "Sensor_Temperature", "Sensor_Pressure", "Sensor_Vibration", "Sensor_Humidity",
    "Maintenance_Log", "Maintenance_PM",
    "Personnel_Shift", "Personnel_Training",
    "Material_Trace", "Process_Param", "Environment_Monitor",
    "SPC_Control", "Calibration_Record"
};
#define DB_COUNT (sizeof(db_names) / sizeof(db_names[0]))

/* ========== 从各数据库抽取知识并加入知识库 ========== */

static int bootstrap_kb_from_databases(kos_knowledge_base_t* kb, kos_state_t* sigma) {
    BatchID batch = {.batch_id = "Batch_26_01-00001"};
    ErrorCode err = {.code = "UNQUALIFIED"};
    Machine machine = {.machine_id = "HeatTreatment_05", .line_id = "Line2"};
    Param param = {.param_name = "temperature"};
    ParamValue pv = {.param = param, .value = 185.2};
    /* 工艺步骤须在失败前结束：step.end < fail.time */
    TimeRange step_dur = {.start = TIME_BASE - 3600, .end = TIME_BASE - 60};
    Time anomaly_time = TIME_BASE - 1800;  /* 异常发生在工艺步骤期间 */

    int ts = 0;

    /* DB1-4: MES 数据 */
    kos_term* proc_step = kos_mk_proc_step(batch, machine, step_dur);
    if (proc_step) {
        kos_kb_add_item(kb, "db_mes_proc", proc_step->kind == KOS_PAIR ? proc_step->data.pair.data : proc_step,
                        kos_mk_prop("ProcStep"), ts++, KOS_KB_SOURCE_BOOTSTRAP);
        sigma->K = kos_update_knowledge(sigma->K, kos_term_copy(proc_step));
        kos_term_free(proc_step);
    }

    /* DB5-7: 异常数据（温度异常 - 根因） */
    kos_term* anomaly = kos_mk_anomaly(machine, param, pv, anomaly_time);
    if (anomaly) {
        kos_kb_add_item(kb, "db_sensor_temp_anomaly", anomaly, kos_mk_prop("Anomaly"), ts++, KOS_KB_SOURCE_BOOTSTRAP);
        sigma->K = kos_update_knowledge(sigma->K, kos_term_copy(anomaly));
        kos_term_free(anomaly);
    }

    /* DB8-10: 失败事件 */
    kos_term* fail_term = kos_mk_fail_event(batch, err, TIME_BASE);
    if (fail_term) {
        kos_kb_add_item(kb, "db_quality_fail", fail_term->kind == KOS_PAIR ? fail_term->data.pair.data : fail_term,
                        kos_mk_prop("FailEvt"), ts++, KOS_KB_SOURCE_BOOTSTRAP);
        sigma->K = kos_update_knowledge(sigma->K, kos_term_copy(fail_term));
        kos_term_free(fail_term);
    }

    /* 模拟更多数据库抽取的辅助知识项 */
    const char* aux_items[][2] = {
        {"db_erp_batch", "Batch_26_01-00001"}, {"db_mes_machine", "HeatTreatment_05"},
        {"db_maintenance_log", "HT05_last_pm_2026-01-15"}, {"db_personnel_shift", "Shift_A_operator_101"},
        {"db_material_trace", "Steel_Lot_S-2026-001"}, {"db_process_param", "Temp_spec_180_190"},
        {"db_env_monitor", "Humidity_65"}, {"db_spc_control", "Cp_1.2"},
        {"db_calibration", "Thermo_T05_cal_2026-01"}
    };
    for (size_t i = 0; i < sizeof(aux_items) / sizeof(aux_items[0]); i++) {
        kos_term* t = kos_mk_prop(aux_items[i][1]);
        kos_term* a = kos_mk_prop("Fact");
        if (t && a) {
            char id[64];
            snprintf(id, sizeof(id), "%s", aux_items[i][0]);
            kos_kb_add_item(kb, id, t, a, ts++, KOS_KB_SOURCE_BOOTSTRAP);
            kos_term_free(t);
            kos_term_free(a);
        }
    }

    /* 建立依赖：事件 → 工艺步骤 → 异常（根因） */
    kos_kb_add_dependency_typed(kb, "db_quality_fail", "db_mes_proc", KOS_KB_DEP_TYPE_CAUSAL);
    kos_kb_add_dependency_typed(kb, "db_mes_proc", "db_sensor_temp_anomaly", KOS_KB_DEP_TYPE_CAUSAL);
    kos_kb_add_dependency_typed(kb, "db_sensor_temp_anomaly", "db_maintenance_log", KOS_KB_DEP_TYPE_REF);
    kos_kb_add_dependency_typed(kb, "db_sensor_temp_anomaly", "db_process_param", KOS_KB_DEP_TYPE_REF);

    return ts;
}

/* ========== 主流程 ========== */

int main(void) {
    printf("============================================================\n");
    printf("KOS-TL 制造业知识库根因追溯演示\n");
    printf("模拟从 %zu 个数据库抽取知识，推理事件并找到根因\n", DB_COUNT);
    printf("============================================================\n");

    /* 创建状态与知识库 */
    kos_term* initial_K = kos_mk_prop("InitialKB");
    kos_state_t* sigma = kos_state_create(initial_K);
    kos_term_free(initial_K);
    if (!sigma) {
        printf("ERROR: 无法创建状态\n");
        return 1;
    }

    kos_knowledge_base_t* kb = kos_kb_create();
    if (!kb) {
        kos_state_free(sigma);
        return 1;
    }
    kos_state_set_kb(sigma, kb);

    /* 模拟从 20+ 数据库抽取知识 */
    printf("\n[Phase 1] 模拟从 %zu 个制造业数据库抽取知识...\n", DB_COUNT);
    for (size_t i = 0; i < DB_COUNT; i++) {
        printf("  - %s\n", db_names[i]);
    }
    bootstrap_kb_from_databases(kb, sigma);
    printf("  抽取完成: 知识库 %zu 项, %zu 依赖边\n", kb->item_count, kb->edge_count);

    /* 依赖推断（从项结构补充依赖） */
    int inferred = kos_kb_infer_dependencies(kb);
    printf("  依赖推断: 新增 %d 条边\n", inferred);

    /* 模拟收到质量事件：批次 Batch_26_01-00001 不合格 */
    printf("\n[Phase 2] 收到质量事件...\n");
    FailEvt fail_evt = {
        .batch = {.batch_id = "Batch_26_01-00001"},
        .error = {.code = "UNQUALIFIED"},
        .time = TIME_BASE,
        .qa_shift_proof = NULL
    };
    printf("  事件: 批次 %s 不合格 (error=%s, time=%llu)\n",
           fail_evt.batch.batch_id, fail_evt.error.code, (unsigned long long)fail_evt.time);

    /* KOS-TL 因果追溯：从 K 中搜索因果链 */
    printf("\n[Phase 3] KOS-TL 推理（因果追溯）...\n");
    RootCauseReport* rep = kos_analyze_quality_traceability(fail_evt, sigma);

    if (rep) {
        printf("  ✓ 根因追溯结果:\n");
        printf("    - 失败批次: %s\n", rep->failure.batch.batch_id);
        printf("    - 错误代码: %s\n", rep->failure.error.code);
        printf("    - 根因异常: 设备 %s, 参数 %s = %.2f\n",
               rep->anomaly.machine.machine_id,
               rep->anomaly.param.param_name,
               rep->anomaly.value.value);
        printf("    - 关联工艺步骤: %s @ %s\n",
               rep->causal_proof.process_step.batch.batch_id,
               rep->causal_proof.process_step.machine.machine_id);

        /* 因果证据搜索：获取 CausalProof 项 */
        kos_term* evidence = kos_search_causal_evidence(fail_evt, sigma);
        if (evidence) {
            printf("  ✓ 因果证据: 已找到 CausalProof 项\n");
            kos_term_free(evidence);
        }

        /* 反事实推理测试：若该异常未发生，能否解释失败？ */
        bool cf_necessary = kos_counterfactual_test(rep->anomaly, fail_evt, sigma);
        printf("  ✓ 反事实测试: %s\n", cf_necessary
               ? "通过（异常为因果必要，支持根因结论）" : "未通过（存在替代原因）");

        /* 将根因报告物化到知识库 */
        char idbuf[64];
        snprintf(idbuf, sizeof(idbuf), "root_cause_ts%d", sigma->TS + 1);
        kos_term* rc_term = kos_mk_prop("RootCauseReport");
        kos_term* rc_type = kos_mk_prop("CausalProof");
        if (rc_term && rc_type) {
            kos_kb_add_item(kb, idbuf, rc_term, rc_type, sigma->TS + 1, KOS_KB_SOURCE_MATERIALIZED);
            kos_kb_add_dependency_typed(kb, idbuf, "db_quality_fail", KOS_KB_DEP_TYPE_CAUSAL);
            kos_kb_add_dependency_typed(kb, idbuf, "db_sensor_temp_anomaly", KOS_KB_DEP_TYPE_CAUSAL);
            kos_term_free(rc_term);
            kos_term_free(rc_type);
        }
        kos_root_cause_report_free(rep);
    } else {
        printf("  (未找到因果链，可能 K 中缺少工艺步骤/异常数据)\n");
    }

    /* 导出可视化 */
    printf("\n[Phase 4] 导出依赖图可视化...\n");
    char* json = kos_kb_export_dependency_graph_json(kb);
    if (json) {
        printf("  JSON 长度: %zu 字符\n", strlen(json));
        free(json);
    }
    char* html = kos_kb_export_visualization_html(kb);
    if (html) {
        FILE* f = fopen("manufacturing_kb_root_cause.html", "w");
        if (f) {
            fputs(html, f);
            fclose(f);
            printf("  ✓ HTML 已写入 manufacturing_kb_root_cause.html\n");
        }
        free(html);
    }

    printf("\n============================================================\n");
    printf("演示完成。知识库共 %zu 项、%zu 条依赖边。\n", kb->item_count, kb->edge_count);
    printf("============================================================\n");

    kos_state_free(sigma);  /* 会释放 kb */
    return 0;
}
