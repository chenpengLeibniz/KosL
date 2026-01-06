// src/domain/manufacturing/types.c
// 制造业领域类型构建器
// 注意：类型构建现在基于内存中的类型本体，而不是硬编码

#include "../../../include/kos_manufacturing.h"
#include "../../../include/kos_ontology.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

// 全局类型本体（从文件加载）
static TypeOntology* g_manufacturing_ontology = NULL;

// 前向声明
static bool kos_check_causal_validity_simple(Anomaly* anomaly, FailEvt* failure, ProcStep* step);

// 获取或初始化制造业类型本体
static TypeOntology* get_manufacturing_ontology(void) {
    if (!g_manufacturing_ontology) {
        extern TypeOntology* kos_manufacturing_ontology_init(void);
        g_manufacturing_ontology = kos_manufacturing_ontology_init();
    }
    return g_manufacturing_ontology;
}

// 创建 BatchID（基于类型本体）
kos_term* kos_mk_batch_id(const char* batch_id) {
    if (!batch_id) {
        return NULL;
    }
    
    TypeOntology* ontology = get_manufacturing_ontology();
    if (ontology) {
        // 构造BatchID实例（基于ID类型）
        kos_term* batch_id_term = kos_mk_id(batch_id);
        // 使用本体构建类型实例（包含类型检查）
        return kos_ontology_mk_type_instance(ontology, "BatchID", batch_id_term, NULL);
    }
    
    // 回退到直接构建
    return kos_mk_prop(batch_id);
}

// 创建 Machine
kos_term* kos_mk_machine(const char* machine_id, const char* line_id) {
    if (!machine_id || !line_id) {
        return NULL;
    }
    
    char buffer[256];
    snprintf(buffer, sizeof(buffer), "Machine(%s, %s)", machine_id, line_id);
    return kos_mk_prop(buffer);
}

// 创建失败事件
kos_term* kos_mk_fail_event(BatchID batch, ErrorCode error, Time time) {
    char buffer[512];
    snprintf(buffer, sizeof(buffer), 
             "FailEvt(batch=%s, error=%s, time=%I64u)",
             batch.batch_id, error.code, (unsigned long long)time);
    
    kos_term* event = kos_mk_prop(buffer);
    if (!event) {
        return NULL;
    }
    
    // 创建QA班次证明（简化实现）
    kos_term* qa_proof = kos_mk_prop("Proof(QAShift)");
    
    // 构造 Σ-Type 对
    return kos_mk_pair(event, qa_proof);
}

// 创建工艺步骤
kos_term* kos_mk_proc_step(BatchID batch, Machine machine, TimeRange duration) {
    char buffer[512];
    snprintf(buffer, sizeof(buffer),
             "ProcStep(batch=%s, machine=%s, start=%I64u, end=%I64u)",
             batch.batch_id, machine.machine_id,
             (unsigned long long)duration.start, (unsigned long long)duration.end);
    
    kos_term* step = kos_mk_prop(buffer);
    if (!step) {
        return NULL;
    }
    
    // 创建工艺路线证明（简化实现）
    kos_term* route_proof = kos_mk_prop("Proof(InRoute)");
    
    return kos_mk_pair(step, route_proof);
}

// 创建异常
kos_term* kos_mk_anomaly(Machine machine, Param param, ParamValue value, Time time) {
    char buffer[512];
    snprintf(buffer, sizeof(buffer),
             "Anomaly(machine=%s, param=%s, value=%.2f, time=%I64u)",
             machine.machine_id, param.param_name, value.value,
             (unsigned long long)time);
    
    return kos_mk_prop(buffer);
}

// 创建因果证明
kos_term* kos_mk_causal_proof(Anomaly anomaly, FailEvt failure, ProcStep step) {
    // 验证因果有效性
    if (!kos_check_causal_validity_simple(&anomaly, &failure, &step)) {
        return NULL;
    }
    
    char buffer[512];
    snprintf(buffer, sizeof(buffer),
             "CausalProof(anomaly_time=%I64u, failure_time=%I64u, step_end=%I64u)",
             (unsigned long long)anomaly.time,
             (unsigned long long)failure.time,
             (unsigned long long)step.duration.end);
    
    kos_term* proof = kos_mk_prop(buffer);
    if (!proof) {
        return NULL;
    }
    
    // 创建时间逻辑证明
    kos_term* temporal = kos_mk_prop("Proof(temporal: anomaly in step && step < failure)");
    
    // 创建空间逻辑证明
    kos_term* spatial = kos_mk_prop("Proof(spatial: anomaly.machine == step.machine)");
    
    // 创建批次一致性证明
    kos_term* batch = kos_mk_prop("Proof(batch: step.batch == failure.batch)");
    
    // 组合所有证明
    kos_term* combined = kos_mk_pair(temporal, spatial);
    kos_term* final_proof = kos_mk_pair(combined, batch);
    
    return kos_mk_pair(proof, final_proof);
}

// 创建根因报告
kos_term* kos_mk_root_cause_report(FailEvt failure, Anomaly anomaly, CausalProof proof) {
    char buffer[512];
    snprintf(buffer, sizeof(buffer),
             "RootCauseReport(batch=%s, error=%s, machine=%s, param=%s)",
             failure.batch.batch_id, failure.error.code,
             anomaly.machine.machine_id, anomaly.param.param_name);
    
    kos_term* report = kos_mk_prop(buffer);
    if (!report) {
        return NULL;
    }
    
    // 创建完整的证明链
    kos_term* causal_proof_term = kos_mk_causal_proof(anomaly, failure, proof.process_step);
    if (!causal_proof_term) {
        kos_term_free(report);
        return NULL;
    }
    
    return kos_mk_pair(report, causal_proof_term);
}

// 辅助函数：简化版因果有效性检查
static bool kos_check_causal_validity_simple(Anomaly* anomaly, FailEvt* failure, ProcStep* step) {
    if (!anomaly || !failure || !step) {
        return false;
    }
    
    // 时间逻辑：异常时间在步骤时间内，且步骤结束时间 < 失败时间
    bool temporal_ok = (anomaly->time >= step->duration.start) &&
                       (anomaly->time <= step->duration.end) &&
                       (step->duration.end < failure->time);
    
    // 空间逻辑：异常机器 = 步骤机器
    bool spatial_ok = (strcmp(anomaly->machine.machine_id, step->machine.machine_id) == 0);
    
    // 批次一致性：步骤批次 = 失败批次
    bool batch_ok = (strcmp(step->batch.batch_id, failure->batch.batch_id) == 0);
    
    return temporal_ok && spatial_ok && batch_ok;
}

