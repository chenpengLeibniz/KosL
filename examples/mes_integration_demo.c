/**
 * @file mes_integration_demo.c
 * @brief MES/QMS 对接适配器示例
 *
 * 演示如何将 MES、QMS 等制造业系统的数据映射为 KOS-TL 类型，
 * 并接入因果追溯流程。
 *
 * 补齐路线：见 docs/KOS_TL_APPLICATION_VALUE_ANALYSIS.md Phase 1.3
 */

#include "../include/kos_core.h"
#include "../include/kos_kernel.h"
#include "../include/kos_manufacturing.h"
#include "../include/kos_causal_trace.h"
#include "../include/kos_knowledge_base.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define TIME_BASE 1738195200ULL  /* 2026-01-30 00:00:00 UTC */

/* ========== 模拟 MES API 返回的原始数据结构 ========== */

typedef struct {
    char work_order_id[64];
    char batch_code[64];
    char product_code[32];
    uint64_t planned_start;
    uint64_t planned_end;
} mes_work_order_t;

typedef struct {
    char operation_id[64];
    char machine_code[64];
    char line_code[64];
    uint64_t actual_start;
    uint64_t actual_end;
    char status[16];  /* "RUNNING", "COMPLETED", "ABORTED" */
} mes_process_record_t;

typedef struct {
    char sensor_id[64];
    char param_name[64];
    double value;
    double spec_min;
    double spec_max;
    uint64_t timestamp;
    char severity[16];  /* "WARNING", "ALARM", "CRITICAL" */
} mes_sensor_anomaly_t;

typedef struct {
    char defect_code[32];
    char batch_code[64];
    uint64_t detect_time;
    char station_id[64];
} mes_quality_defect_t;

/* ========== MES 适配器：将 MES 数据映射为 KOS 类型 ========== */

typedef struct {
    const char* mes_endpoint;
    const char* api_key;
    int timeout_ms;
} kos_mes_adapter_config_t;

/* 从 MES 工艺记录映射为 ProcStep */
static ProcStep mes_to_proc_step(const mes_work_order_t* wo,
                                 const mes_process_record_t* rec) {
    ProcStep step = {0};
    strncpy(step.batch.batch_id, wo->batch_code, sizeof(step.batch.batch_id) - 1);
    strncpy(step.machine.machine_id, rec->machine_code, sizeof(step.machine.machine_id) - 1);
    strncpy(step.machine.line_id, rec->line_code, sizeof(step.machine.line_id) - 1);
    step.duration.start = rec->actual_start;
    step.duration.end = rec->actual_end;
    step.route_proof = kos_mk_prop("Proof(InRoute_from_MES)");
    return step;
}

/* 从 MES 传感器异常映射为 Anomaly */
static Anomaly mes_to_anomaly(const mes_sensor_anomaly_t* sa,
                              const mes_process_record_t* rec) {
    Anomaly a = {0};
    strncpy(a.machine.machine_id, rec->machine_code, sizeof(a.machine.machine_id) - 1);
    strncpy(a.machine.line_id, rec->line_code, sizeof(a.machine.line_id) - 1);
    strncpy(a.param.param_name, sa->param_name, sizeof(a.param.param_name) - 1);
    a.value.param = a.param;
    a.value.value = sa->value;
    a.time = sa->timestamp;
    return a;
}

/* 从 MES 质量缺陷映射为 FailEvt */
static FailEvt mes_to_fail_evt(const mes_quality_defect_t* qd) {
    FailEvt f = {0};
    strncpy(f.batch.batch_id, qd->batch_code, sizeof(f.batch.batch_id) - 1);
    strncpy(f.error.code, qd->defect_code, sizeof(f.error.code) - 1);
    f.time = qd->detect_time;
    f.qa_shift_proof = kos_mk_prop("Proof(QAShift_from_QMS)");
    return f;
}

/* ========== 模拟 MES API 拉取（实际项目中替换为 HTTP/OPC-UA 等） ========== */

static int mes_adapter_fetch_work_orders(const kos_mes_adapter_config_t* config,
                                         mes_work_order_t* out, int max_count) {
    (void)config;
    /* 模拟：返回一条工单 */
    if (max_count < 1) return 0;
    strcpy(out[0].work_order_id, "WO-2026-001");
    strcpy(out[0].batch_code, "Batch_26_01-00001");
    strcpy(out[0].product_code, "Bearing-6205");
    out[0].planned_start = TIME_BASE - 7200;
    out[0].planned_end = TIME_BASE;
    return 1;
}

static int mes_adapter_fetch_process_records(const kos_mes_adapter_config_t* config,
                                             const char* batch_code,
                                             mes_process_record_t* out, int max_count) {
    (void)config;
    (void)batch_code;
    if (max_count < 1) return 0;
    strcpy(out[0].operation_id, "OP-HeatTreat-05");
    strcpy(out[0].machine_code, "HeatTreatment_05");
    strcpy(out[0].line_code, "Line2");
    out[0].actual_start = TIME_BASE - 3600;
    out[0].actual_end = TIME_BASE - 60;  /* 工艺须在失败前结束 */
    strcpy(out[0].status, "COMPLETED");
    return 1;
}

static int mes_adapter_fetch_sensor_anomalies(const kos_mes_adapter_config_t* config,
                                             const char* machine_code,
                                             uint64_t ts_start, uint64_t ts_end,
                                             mes_sensor_anomaly_t* out, int max_count) {
    (void)config;
    (void)machine_code;
    (void)ts_start;
    (void)ts_end;
    if (max_count < 1) return 0;
    strcpy(out[0].sensor_id, "T05-Thermo-01");
    strcpy(out[0].param_name, "temperature");
    out[0].value = 185.2;
    out[0].spec_min = 180.0;
    out[0].spec_max = 190.0;
    out[0].timestamp = TIME_BASE - 1800;
    strcpy(out[0].severity, "ALARM");
    return 1;
}

static int mes_adapter_fetch_quality_defects(const kos_mes_adapter_config_t* config,
                                            uint64_t ts_start, uint64_t ts_end,
                                            mes_quality_defect_t* out, int max_count) {
    (void)config;
    (void)ts_start;
    (void)ts_end;
    if (max_count < 1) return 0;
    strcpy(out[0].defect_code, "UNQUALIFIED");
    strcpy(out[0].batch_code, "Batch_26_01-00001");
    out[0].detect_time = TIME_BASE;
    strcpy(out[0].station_id, "QA-Final-01");
    return 1;
}

/* ========== 主流程：MES 数据 → KOS 知识库 → 因果追溯 ========== */

int main(void) {
    printf("============================================================\n");
    printf("KOS-TL MES/QMS 对接适配器演示\n");
    printf("模拟 MES API 拉取 → 映射为 KOS 类型 → 因果追溯\n");
    printf("============================================================\n");

    kos_mes_adapter_config_t mes_config = {
        .mes_endpoint = "http://mes.example.com/api",
        .api_key = "demo_key",
        .timeout_ms = 5000
    };

    /* 1. 从 MES 拉取原始数据 */
    printf("\n[Phase 1] 从 MES 拉取数据...\n");
    mes_work_order_t wo;
    mes_process_record_t proc;
    mes_sensor_anomaly_t sensor_anomaly;
    mes_quality_defect_t defect;

    int n_wo = mes_adapter_fetch_work_orders(&mes_config, &wo, 1);
    int n_proc = mes_adapter_fetch_process_records(&mes_config, wo.batch_code, &proc, 1);
    int n_anom = mes_adapter_fetch_sensor_anomalies(&mes_config, proc.machine_code,
                                                    proc.actual_start, proc.actual_end,
                                                    &sensor_anomaly, 1);
    int n_def = mes_adapter_fetch_quality_defects(&mes_config,
                                                  TIME_BASE - 86400, TIME_BASE,
                                                  &defect, 1);

    printf("  工单: %d 条, 工艺记录: %d 条, 传感器异常: %d 条, 质量缺陷: %d 条\n",
           n_wo, n_proc, n_anom, n_def);

    /* 2. 映射为 KOS 类型并构建状态 */
    printf("\n[Phase 2] 映射为 KOS 类型...\n");
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

    int ts = 0;
    if (n_proc > 0) {
        ProcStep step = mes_to_proc_step(&wo, &proc);
        kos_term* proc_term = kos_mk_proc_step(step.batch, step.machine, step.duration);
        if (proc_term) {
            kos_kb_add_item(kb, "mes_proc_step", proc_term->kind == KOS_PAIR ? proc_term->data.pair.data : proc_term,
                            kos_mk_prop("ProcStep"), ts++, KOS_KB_SOURCE_BOOTSTRAP);
            sigma->K = kos_update_knowledge(sigma->K, kos_term_copy(proc_term));
            kos_term_free(proc_term);
        }
        if (step.route_proof) kos_term_free(step.route_proof);
    }

    if (n_anom > 0) {
        Anomaly a = mes_to_anomaly(&sensor_anomaly, &proc);
        ParamValue pv = {.param = a.param, .value = a.value.value};
        kos_term* anom_term = kos_mk_anomaly(a.machine, a.param, pv, a.time);
        if (anom_term) {
            kos_kb_add_item(kb, "mes_sensor_anomaly", anom_term,
                            kos_mk_prop("Anomaly"), ts++, KOS_KB_SOURCE_BOOTSTRAP);
            sigma->K = kos_update_knowledge(sigma->K, kos_term_copy(anom_term));
            kos_term_free(anom_term);
        }
    }

    if (n_def > 0) {
        FailEvt f = mes_to_fail_evt(&defect);
        kos_term* fail_term = kos_mk_fail_event(f.batch, f.error, f.time);
        if (fail_term) {
            kos_kb_add_item(kb, "qms_fail_evt", fail_term->kind == KOS_PAIR ? fail_term->data.pair.data : fail_term,
                            kos_mk_prop("FailEvt"), ts++, KOS_KB_SOURCE_BOOTSTRAP);
            sigma->K = kos_update_knowledge(sigma->K, kos_term_copy(fail_term));
            kos_term_free(fail_term);
        }
        if (f.qa_shift_proof) kos_term_free(f.qa_shift_proof);
    }

    /* 建立依赖 */
    kos_kb_add_dependency_typed(kb, "qms_fail_evt", "mes_proc_step", KOS_KB_DEP_TYPE_CAUSAL);
    kos_kb_add_dependency_typed(kb, "mes_proc_step", "mes_sensor_anomaly", KOS_KB_DEP_TYPE_CAUSAL);

    printf("  知识库: %zu 项, %zu 依赖边\n", kb->item_count, kb->edge_count);

    /* 3. 因果追溯 */
    printf("\n[Phase 3] KOS-TL 因果追溯...\n");
    FailEvt fail_evt = mes_to_fail_evt(&defect);
    RootCauseReport* rep = kos_analyze_quality_traceability(fail_evt, sigma);

    if (rep) {
        printf("  ✓ 根因追溯结果:\n");
        printf("    - 失败批次: %s\n", rep->failure.batch.batch_id);
        printf("    - 错误代码: %s\n", rep->failure.error.code);
        printf("    - 根因异常: 设备 %s, 参数 %s = %.2f\n",
               rep->anomaly.machine.machine_id,
               rep->anomaly.param.param_name,
               rep->anomaly.value.value);
        kos_root_cause_report_free(rep);
    } else {
        printf("  (未找到因果链)\n");
    }
    if (fail_evt.qa_shift_proof) kos_term_free(fail_evt.qa_shift_proof);

    printf("\n============================================================\n");
    printf("MES 对接演示完成。适配器模式可扩展到真实 MES/QMS API。\n");
    printf("============================================================\n");

    kos_state_free(sigma);
    return 0;
}
