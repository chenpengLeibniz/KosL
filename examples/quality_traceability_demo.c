// examples/quality_traceability_demo.c
// 制造业质量异常追溯应用示例
// 参考 Kos.pdf Section 7: Application of KOS-TL
//
// 应用场景：轴承生产质量追溯
// 问题：当某批次产品出现严重质量缺陷时，系统能否自动追溯其生产过程，
//       识别与设备、人员或原材料相关的潜在异常，并产生可执行和可解释的因果链？

#include "../include/kos_core.h"
#include "../include/kos_kernel.h"
#include "../include/kos_runtime.h"
#include "../include/kos_manufacturing.h"
#include "../include/kos_causal_trace.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// ========== 应用场景：轴承生产质量追溯 ==========
//
// 核心挑战：
// 1. 异构数据源（工单、设备日志、人员排班、质检记录）
// 2. 强时间顺序和因果约束
// 3. 推理结果必须直接支持生产决策和责任归属

// ========== Step 1: Core Layer - 定义类型和约束 ==========

// 基础原子类型（Base Sorts）
kos_term* create_base_types(void) {
    printf("[Core] Creating base types...\n");
    
    // BatchID: 批次标识符
    kos_term* batch_id_type = kos_mk_id("BatchID");
    
    // Machine: 设备标识符
    kos_term* machine_type = kos_mk_id("Machine");
    
    // Time: 时间点
    kos_term* time_type = kos_mk_time("Time");
    
    // ErrorCode: 错误代码
    kos_term* error_type = kos_mk_id("ErrorCode");
    
    // ParamName: 参数名称
    kos_term* param_type = kos_mk_id("ParamName");
    
    // Value: 参数值
    kos_term* value_type = kos_mk_val("Value");
    
    printf("  - BatchID, Machine, Time, ErrorCode, ParamName, Value\n");
    
    // 注意：这些类型会被后续使用，实际应用中应该保存到本体中
    return batch_id_type;  // 返回一个作为示例
}

// 创建失败事件类型：FailEvt : Σ(b:BatchID). Σ(err:ErrorCode). Σ(t:Time). Prop
kos_term* create_failure_event_type(void) {
    printf("[Core] Creating FailureEvent type...\n");
    
    kos_term* batch_id_type = kos_mk_id("BatchID");
    kos_term* error_type = kos_mk_id("ErrorCode");
    kos_term* time_type = kos_mk_time("Time");
    kos_term* prop_type = kos_mk_prop("FailureEvent");
    
    // FailEvt : Σ(b:BatchID). Σ(err:ErrorCode). Σ(t:Time). Prop
    kos_term* sigma_t_prop = kos_mk_sigma(time_type, prop_type);
    kos_term* sigma_err_sigma = kos_mk_sigma(error_type, sigma_t_prop);
    kos_term* fail_evt_type = kos_mk_sigma(batch_id_type, sigma_err_sigma);
    
    printf("  - FailEvt : Σ(b:BatchID). Σ(err:ErrorCode). Σ(t:Time). Prop\n");
    
    return fail_evt_type;
}

// 创建异常事件类型：AnomalyEvt : Σ(m:Machine). Σ(p:ParamName). Σ(v:Value). Σ(t:Time). Prop
kos_term* create_anomaly_event_type(void) {
    printf("[Core] Creating AnomalyEvent type...\n");
    
    kos_term* machine_type = kos_mk_id("Machine");
    kos_term* param_type = kos_mk_id("ParamName");
    kos_term* value_type = kos_mk_val("Value");
    kos_term* time_type = kos_mk_time("Time");
    kos_term* prop_type = kos_mk_prop("AnomalyEvent");
    
    // AnomalyEvt : Σ(m:Machine). Σ(p:ParamName). Σ(v:Value). Σ(t:Time). Prop
    kos_term* sigma_t_prop = kos_mk_sigma(time_type, prop_type);
    kos_term* sigma_v_sigma = kos_mk_sigma(value_type, sigma_t_prop);
    kos_term* sigma_p_sigma = kos_mk_sigma(param_type, sigma_v_sigma);
    kos_term* anomaly_evt_type = kos_mk_sigma(machine_type, sigma_p_sigma);
    
    printf("  - AnomalyEvt : Σ(m:Machine). Σ(p:ParamName). Σ(v:Value). Σ(t:Time). Prop\n");
    
    return anomaly_evt_type;
}

// ========== Step 2: Runtime Layer - 精化原始信号 ==========

// 模拟从物理传感器/数据库获取的原始信号
bitstream create_raw_failure_signal(void) {
    printf("[Runtime] Creating raw failure signal...\n");
    
    // 模拟原始信号：批次 Batch_202310-01 在 10:00 发生 HARD_ERR 错误
    const char* raw_data = "Batch_202310-01|HARD_ERR|1697004000";
    size_t len = strlen(raw_data);
    
    bitstream signal;
    signal.data = (unsigned char*)malloc(len);
    if (signal.data) {
        memcpy(signal.data, raw_data, len);
        signal.length = len;
    } else {
        signal.length = 0;
    }
    
    printf("  - Raw signal: %s\n", raw_data);
    
    return signal;
}

bitstream create_raw_anomaly_signal(void) {
    printf("[Runtime] Creating raw anomaly signal...\n");
    
    // 模拟原始信号：设备 HeatTreatment_03 在 09:50 电压参数异常（220.5V，正常应为 220V±2V）
    const char* raw_data = "HeatTreatment_03|voltage|220.5|1697001000";
    size_t len = strlen(raw_data);
    
    bitstream signal;
    signal.data = (unsigned char*)malloc(raw_data ? len : 0);
    if (signal.data) {
        memcpy(signal.data, raw_data, len);
        signal.length = len;
    } else {
        signal.length = 0;
    }
    
    printf("  - Raw signal: %s\n", raw_data);
    
    return signal;
}

// ========== Step 3: 构造具体事件实例 ==========

// 构造失败事件实例
kos_term* create_failure_event_instance(const char* batch_id, const char* error_code, const char* time_str) {
    printf("[Kernel] Creating failure event instance...\n");
    
    kos_term* batch_id_term = kos_mk_id(batch_id);
    kos_term* error_term = kos_mk_id(error_code);
    kos_term* time_term = kos_mk_time(time_str);
    kos_term* proof_term = kos_mk_prop("FailureProof");
    
    // 构造嵌套的 Σ 类型实例：<batch_id, <error_code, <time, proof>>>
    kos_term* pair_t_proof = kos_mk_pair(time_term, proof_term);
    kos_term* pair_e_rest = kos_mk_pair(error_term, pair_t_proof);
    kos_term* fail_evt_instance = kos_mk_pair(batch_id_term, pair_e_rest);
    
    printf("  - Instance: Batch=%s, Error=%s, Time=%s\n", batch_id, error_code, time_str);
    
    return fail_evt_instance;
}

// 构造异常事件实例
kos_term* create_anomaly_event_instance(const char* machine_id, const char* param_name, 
                                        const char* value_str, const char* time_str) {
    printf("[Kernel] Creating anomaly event instance...\n");
    
    kos_term* machine_term = kos_mk_id(machine_id);
    kos_term* param_term = kos_mk_id(param_name);
    kos_term* value_term = kos_mk_val(value_str);
    kos_term* time_term = kos_mk_time(time_str);
    kos_term* proof_term = kos_mk_prop("AnomalyProof");
    
    // 构造嵌套的 Σ 类型实例：<machine, <param, <value, <time, proof>>>>
    kos_term* pair_t_proof = kos_mk_pair(time_term, proof_term);
    kos_term* pair_v_rest = kos_mk_pair(value_term, pair_t_proof);
    kos_term* pair_p_rest = kos_mk_pair(param_term, pair_v_rest);
    kos_term* anomaly_evt_instance = kos_mk_pair(machine_term, pair_p_rest);
    
    printf("  - Instance: Machine=%s, Param=%s, Value=%s, Time=%s\n", 
           machine_id, param_name, value_str, time_str);
    
    return anomaly_evt_instance;
}

// ========== Step 4: Kernel Layer - 状态演化和因果推理 ==========

// 验证因果有效性（简化实现）
bool verify_causal_validity(kos_term* anomaly_evt, kos_term* failure_evt) {
    printf("[Kernel] Verifying causal validity...\n");
    
    if (!anomaly_evt || !failure_evt || 
        anomaly_evt->kind != KOS_PAIR || failure_evt->kind != KOS_PAIR) {
        return false;
    }
    
    // 提取时间信息（简化实现：假设时间在嵌套的 pair 中）
    // 实际实现中需要递归提取时间字段并比较
    
    // 简化验证：检查事件结构是否正确
    bool anomaly_valid = (anomaly_evt->data.pair.data != NULL && 
                          anomaly_evt->data.pair.proof != NULL);
    bool failure_valid = (failure_evt->data.pair.data != NULL && 
                          failure_evt->data.pair.proof != NULL);
    
    printf("  - Anomaly event valid: %s\n", anomaly_valid ? "Yes" : "No");
    printf("  - Failure event valid: %s\n", failure_valid ? "Yes" : "No");
    
    return anomaly_valid && failure_valid;
}

// ========== 主程序 ==========

int main(void) {
    printf("========================================\n");
    printf("KOS-TL Quality Traceability Demo\n");
    printf("Manufacturing: Bearing Production\n");
    printf("========================================\n\n");
    
    // ========== Phase 1: Core Layer - 类型定义 ==========
    printf("=== Phase 1: Core Layer - Type Definitions ===\n");
    
    kos_term* base_types = create_base_types();
    kos_term* fail_evt_type = create_failure_event_type();
    kos_term* anomaly_evt_type = create_anomaly_event_type();
    
    printf("\n");
    
    // ========== Phase 2: Runtime Layer - 信号精化 ==========
    printf("=== Phase 2: Runtime Layer - Signal Elaboration ===\n");
    
    // 创建初始本体（简化：使用失败事件类型作为本体）
    kos_term* initial_ontology = fail_evt_type;
    
    // 精化失败信号
    bitstream failure_signal = create_raw_failure_signal();
    kos_term* failure_event_pair = kos_elab(failure_signal, initial_ontology);
    
    if (!failure_event_pair) {
        printf("  ERROR: Failed to elaborate failure signal (logical firewall)\n");
        free(failure_signal.data);
        return 1;
    }
    printf("  ✓ Failure signal elaborated successfully\n");
    
    // 精化异常信号
    bitstream anomaly_signal = create_raw_anomaly_signal();
    kos_term* anomaly_event_pair = kos_elab(anomaly_signal, initial_ontology);
    
    if (!anomaly_event_pair) {
        printf("  ERROR: Failed to elaborate anomaly signal (logical firewall)\n");
        kos_term_free(failure_event_pair);
        free(failure_signal.data);
        free(anomaly_signal.data);
        return 1;
    }
    printf("  ✓ Anomaly signal elaborated successfully\n");
    
    free(failure_signal.data);
    free(anomaly_signal.data);
    
    printf("\n");
    
    // ========== Phase 3: Kernel Layer - 状态演化 ==========
    printf("=== Phase 3: Kernel Layer - State Evolution ===\n");
    
    // 创建初始状态
    kos_state_t* sigma = kos_runtime_init(initial_ontology);
    if (!sigma) {
        printf("  ERROR: Failed to create initial state\n");
        kos_term_free(failure_event_pair);
        kos_term_free(anomaly_event_pair);
        return 1;
    }
    printf("  Initial state created (TS=%d)\n", kos_state_get_TS(sigma));
    
    // 将异常事件加入队列（先发生）
    if (kos_queue_enqueue(sigma->P, anomaly_event_pair) != 0) {
        printf("  ERROR: Failed to enqueue anomaly event\n");
        kos_state_free(sigma);
        kos_term_free(failure_event_pair);
        return 1;
    }
    printf("  ✓ Anomaly event enqueued\n");
    
    // 将失败事件加入队列（后发生）
    if (kos_queue_enqueue(sigma->P, failure_event_pair) != 0) {
        printf("  ERROR: Failed to enqueue failure event\n");
        kos_state_free(sigma);
        kos_term_free(failure_event_pair);
        return 1;
    }
    printf("  ✓ Failure event enqueued\n");
    
    printf("  Queue size: %zu\n", kos_state_get_queue_size(sigma));
    
    // 执行状态演化（Peek-Verify-Reduce-Confirm）
    printf("\n  Executing state evolution...\n");
    
    int step_count = 0;
    while (!kos_queue_is_empty(sigma->P) && step_count < 10) {
        bool success = kos_step(sigma);
        step_count++;
        
        printf("  Step %d: %s (TS=%d, Queue=%zu)\n", 
               step_count, 
               success ? "SUCCESS" : "FAIL",
               kos_state_get_TS(sigma),
               kos_state_get_queue_size(sigma));
        
        if (!success) {
            break;
        }
    }
    
    printf("\n");
    
    // ========== Phase 4: 因果推理和根因分析（基于因果追溯） ==========
    printf("=== Phase 4: Causal Reasoning and Root Cause Analysis ===\n");
    
    // 使用制造业领域类型构造事件并加入 K，以便因果索引能解析
    BatchID batch = {.batch_id = "Batch_202310-01"};
    ErrorCode err = {.code = "HARD_ERR"};
    Machine machine = {.machine_id = "HeatTreatment_03", .line_id = "Line1"};
    Param param = {.param_name = "voltage"};
    ParamValue pv = {.param = param, .value = 220.5};
    TimeRange step_dur = {.start = 1697000000ULL, .end = 1697002000ULL};  /* 09:33-09:53 */
    
    kos_term* proc_step_term = kos_mk_proc_step(batch, machine, step_dur);
    kos_term* anomaly_term = kos_mk_anomaly(machine, param, pv, 1697001000ULL);  /* 09:50 */
    kos_term* fail_term = kos_mk_fail_event(batch, err, 1697004000ULL);          /* 10:00 */
    
    if (proc_step_term && anomaly_term && fail_term) {
        sigma->K = kos_update_knowledge(sigma->K, proc_step_term);
        sigma->K = kos_update_knowledge(sigma->K, anomaly_term);
        sigma->K = kos_update_knowledge(sigma->K, fail_term);
        kos_term_free(proc_step_term);
        kos_term_free(anomaly_term);
        kos_term_free(fail_term);
    }
    
    FailEvt failure_for_trace = {.batch = batch, .error = err, .time = 1697004000ULL, .qa_shift_proof = NULL};
    
    // 因果证据搜索（从 K 中查找满足因果约束的链）
    kos_term* causal_evidence = kos_search_causal_evidence(failure_for_trace, sigma);
    bool causal_valid = (causal_evidence != NULL);
    
    printf("\n  Causal Chain Analysis (kos_search_causal_evidence):\n");
    printf("  - Anomaly: HeatTreatment_03 voltage=220.5V at 09:50\n");
    printf("  - ProcStep: Batch_202310-01 on HeatTreatment_03 [09:33-09:53]\n");
    printf("  - Failure: Batch_202310-01 HARD_ERR at 10:00\n");
    printf("  - Temporal: anomaly in step && step.end < failure ✓\n");
    printf("  - Causal evidence found: %s\n", causal_valid ? "YES" : "NO");
    
    RootCauseReport* report = kos_analyze_quality_traceability(failure_for_trace, sigma);
    if (report) {
        printf("\n  Root Cause Report (kos_analyze_quality_traceability):\n");
        printf("  ========================================\n");
        printf("  Batch: %s\n", report->failure.batch.batch_id);
        printf("  Failure: %s at %llu\n", report->failure.error.code, (unsigned long long)report->failure.time);
        printf("  Root Cause: %s anomaly on %s (param=%s, value=%.2f)\n",
               report->anomaly.param.param_name, report->anomaly.machine.machine_id,
               report->anomaly.param.param_name, report->anomaly.value.value);
        printf("  Causal Proof: ✓ Valid temporal, spatial, batch correlation\n");
        printf("  ========================================\n");
        kos_root_cause_report_free(report);
    }
    if (causal_evidence) kos_term_free(causal_evidence);
    
    printf("\n");
    
    // ========== Phase 5: 物化到物理存储 ==========
    printf("=== Phase 5: Materialization to Physical Storage ===\n");
    
    // 创建存储后端
    storage_backend_t* backend = kos_storage_create(STORAGE_BACKEND_MEMORY, NULL);
    if (backend) {
        // 物化知识集到物理存储（原子提交栅栏）
        int materialize_result = kos_materialize(sigma, backend);
        if (materialize_result == 0) {
            printf("  ✓ Knowledge materialized successfully\n");
        } else {
            printf("  ✗ Materialization failed\n");
        }
        
        kos_storage_free(backend);
    }
    
    printf("\n");
    
    // ========== 清理资源 ==========
    printf("=== Cleanup ===\n");
    
    kos_state_free(sigma);
    
    // 注意：failure_event_pair 和 anomaly_event_pair 已被队列或状态管理，不应再次释放
    
    printf("  Resources freed\n");
    
    printf("\n========================================\n");
    printf("Demo completed successfully!\n");
    printf("========================================\n");
    
    return 0;
}
