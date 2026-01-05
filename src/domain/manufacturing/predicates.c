// src/domain/manufacturing/predicates.c
// 制造业领域谓词验证

#include "../../../include/kos_manufacturing.h"
#include <string.h>

// 检查批次是否在工艺路线中
bool kos_check_in_route(BatchID batch, Machine machine, kos_term* K) {
    // 简化实现：从知识集K中查找工艺路线信息
    // 实际实现应该查询 ProcessRoute 表
    (void)batch;
    (void)machine;
    (void)K;
    
    // TODO: 实现真正的工艺路线检查
    // 从K中查找 ProcessRoute(Model, StepName, StepOrder, TargetLineType)
    return true; // 占位符
}

// 检查时间点是否在时间区间内
bool kos_check_overlap(Time time_point, TimeRange duration) {
    return (time_point >= duration.start) && (time_point <= duration.end);
}

// 检查因果有效性
bool kos_check_causal_validity(CausalProof* proof) {
    if (!proof) {
        return false;
    }
    
    // 时间逻辑验证
    bool temporal = kos_check_overlap(proof->anomaly.time, proof->process_step.duration) &&
                    (proof->process_step.duration.end < proof->failure.time);
    
    // 空间逻辑验证
    bool spatial = (strcmp(proof->anomaly.machine.machine_id, 
                          proof->process_step.machine.machine_id) == 0);
    
    // 批次一致性验证
    bool batch = (strcmp(proof->process_step.batch.batch_id,
                        proof->failure.batch.batch_id) == 0);
    
    return temporal && spatial && batch;
}










