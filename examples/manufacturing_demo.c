// examples/manufacturing_demo.c
// 制造业质量异常追溯演示

#include "../include/kos_manufacturing.h"
#include "../include/kos_ontology.h"
#include "../include/kos_runtime.h"
#include <stdio.h>
#include <string.h>

// 声明本体初始化函数
extern TypeOntology* kos_manufacturing_ontology_init(void);

int main() {
    printf("=== Manufacturing Quality Traceability Demo ===\n\n");
    
    // 1. 初始化类型本体（从文件加载或创建）
    printf("1. Initializing Type Ontology:\n");
    TypeOntology* ontology = kos_manufacturing_ontology_init();
    if (!ontology) {
        printf("   ERROR: Failed to initialize ontology\n");
        return 1;
    }
    printf("   Domain: %s\n", ontology->domain_name);
    printf("   Atomic types: %zu\n", ontology->atomic_count);
    printf("   Predicate types: %zu\n", ontology->predicate_count);
    printf("   Event types: %zu\n\n", ontology->event_count);
    
    // 2. 初始化系统
    printf("2. Initializing System:\n");
    kos_state_t sigma = init_system();
    printf("   System initialized (TS=%d)\n\n", sigma.TS);
    
    // 3. 创建失败事件
    printf("3. Creating Failure Event:\n");
    FailEvt failure;
    strcpy(failure.batch.batch_id, "Batch_202310-01");
    strcpy(failure.error.code, "HARD_ERR");
    failure.time = 1697004000; // 2023-10-10 10:00:00
    
    kos_term* fail_event = kos_mk_fail_event(failure.batch, failure.error, failure.time);
    printf("   Failure: Batch=%s, Error=%s, Time=%I64u\n",
           failure.batch.batch_id, failure.error.code, (unsigned long long)failure.time);
    
    // 4. 创建异常事件
    printf("\n4. Creating Anomaly Event:\n");
    Machine machine;
    strcpy(machine.machine_id, "HeatTreatment_03");
    strcpy(machine.line_id, "Line_A");
    
    Param param;
    strcpy(param.param_name, "voltage");
    
    ParamValue value;
    value.param = param;
    value.value = 220.5;
    
    Anomaly anomaly;
    anomaly.machine = machine;
    anomaly.param = param;
    anomaly.value = value;
    anomaly.time = 1697001000; // 2023-10-10 09:50:00 (在失败之前)
    
    kos_term* anomaly_term = kos_mk_anomaly(anomaly.machine, anomaly.param,
                                            anomaly.value, anomaly.time);
    printf("   Anomaly: Machine=%s, Param=%s, Value=%.2f, Time=%I64u\n",
           anomaly.machine.machine_id, anomaly.param.param_name,
           anomaly.value.value, (unsigned long long)anomaly.time);
    
    // 5. 创建工艺步骤
    printf("\n5. Creating Process Step:\n");
    ProcStep step;
    strcpy(step.batch.batch_id, "Batch_202310-01");
    step.machine = machine;
    step.duration.start = 1697000000; // 09:46:40
    step.duration.end = 1697002000;   // 09:53:20
    
    kos_term* step_term = kos_mk_proc_step(step.batch, step.machine, step.duration);
    printf("   Step: Batch=%s, Machine=%s, Duration=[%I64u, %I64u]\n",
           step.batch.batch_id, step.machine.machine_id,
           (unsigned long long)step.duration.start,
           (unsigned long long)step.duration.end);
    
    // 6. 构造因果证明
    printf("\n6. Constructing Causal Proof:\n");
    CausalProof causal_proof;
    causal_proof.anomaly = anomaly;
    causal_proof.failure = failure;
    causal_proof.process_step = step;
    
    bool valid = kos_check_causal_validity(&causal_proof);
    printf("   Causal validity: %s\n", valid ? "VALID" : "INVALID");
    
    if (valid) {
        printf("   - Temporal: Anomaly in step && Step end < Failure time\n");
        printf("   - Spatial: Anomaly.machine == Step.machine\n");
        printf("   - Batch: Step.batch == Failure.batch\n");
    }
    
    // 7. 构造根因报告
    printf("\n7. Generating Root Cause Report:\n");
    kos_term* report = kos_mk_root_cause_report(failure, anomaly, causal_proof);
    if (report) {
        printf("   Root cause report generated successfully\n");
        
        // 序列化报告
        kos_serialized* serialized = kos_term_serialize(report);
        if (serialized) {
            printf("   Serialized report (length=%I64u):\n", (unsigned long long)serialized->length);
            printf("   %s\n", serialized->data);
            kos_serialized_free(serialized);
        }
        
        kos_term_free(report);
    }
    
    // 8. 反事实推理测试
    printf("\n8. Counterfactual Reasoning Test:\n");
    bool is_necessary = kos_counterfactual_test(anomaly, failure, &sigma);
    printf("   Is anomaly necessary for failure? %s\n",
           is_necessary ? "YES" : "NO");
    
    // 9. 保存本体（如果修改了）
    printf("\n9. Saving Ontology:\n");
    extern int kos_manufacturing_ontology_save(TypeOntology* ontology);
    if (kos_manufacturing_ontology_save(ontology) == 0) {
        printf("   Ontology saved successfully\n");
    }
    
    // 清理
    if (fail_event) kos_term_free(fail_event);
    if (anomaly_term) kos_term_free(anomaly_term);
    if (step_term) kos_term_free(step_term);
    if (ontology) kos_ontology_free(ontology);
    
    printf("\n=== Demo Complete ===\n");
    return 0;
}

