// src/domain/manufacturing/ontology_setup.c
// 制造业领域类型本体初始化
// 从文件加载或创建制造业领域的类型本体

#include "../../../include/kos_ontology.h"
#include "../../../include/kos_manufacturing.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

// 制造业领域类型本体文件路径
#define MANUFACTURING_ONTOLOGY_FILE "manufacturing_ontology.json"

// 初始化制造业领域类型本体
// 如果文件存在则加载，否则创建默认本体
TypeOntology* kos_manufacturing_ontology_init(void) {
    // 尝试从文件加载
    TypeOntology* ontology = kos_ontology_load_from_file(MANUFACTURING_ONTOLOGY_FILE);
    
    if (ontology) {
        printf("[Manufacturing] Loaded ontology from file: %s\n", MANUFACTURING_ONTOLOGY_FILE);
        return ontology;
    }
    
    // 文件不存在，创建默认本体
    printf("[Manufacturing] Creating default ontology...\n");
    ontology = kos_ontology_create("manufacturing");
    if (!ontology) {
        return NULL;
    }
    
    // 添加原子类型
    kos_ontology_add_atomic_type(ontology, "BatchID", "String", NULL);
    kos_ontology_add_atomic_type(ontology, "Machine", "String", NULL);
    kos_ontology_add_atomic_type(ontology, "Time", "UInt64", NULL);
    kos_ontology_add_atomic_type(ontology, "ErrorCode", "String", NULL);
    kos_ontology_add_atomic_type(ontology, "Param", "String", NULL);
    kos_ontology_add_atomic_type(ontology, "ParamValue", "Float", NULL);
    
    // 添加谓词类型
    const char* in_route_params[] = {"BatchID", "Machine"};
    kos_ontology_add_predicate_type(ontology, "InRoute", in_route_params, 2, NULL);
    
    const char* overlap_params[] = {"Time", "TimeRange"};
    kos_ontology_add_predicate_type(ontology, "Overlap", overlap_params, 2, NULL);
    
    // 添加事件类型
    const char* fail_evt_fields[] = {"batch", "error", "time"};
    const char* fail_evt_types[] = {"BatchID", "ErrorCode", "Time"};
    kos_ontology_add_event_type(ontology, "FailEvt", fail_evt_fields, fail_evt_types, 3, NULL);
    
    const char* proc_step_fields[] = {"batch", "machine", "duration"};
    const char* proc_step_types[] = {"BatchID", "Machine", "TimeRange"};
    kos_ontology_add_event_type(ontology, "ProcStep", proc_step_fields, proc_step_types, 3, NULL);
    
    const char* anomaly_fields[] = {"machine", "param", "value", "time"};
    const char* anomaly_types[] = {"Machine", "Param", "ParamValue", "Time"};
    kos_ontology_add_event_type(ontology, "Anomaly", anomaly_fields, anomaly_types, 4, NULL);
    
    // 保存到文件
    kos_ontology_save_to_file(ontology, MANUFACTURING_ONTOLOGY_FILE);
    printf("[Manufacturing] Saved default ontology to: %s\n", MANUFACTURING_ONTOLOGY_FILE);
    
    return ontology;
}

// 更新本体并保存
int kos_manufacturing_ontology_save(TypeOntology* ontology) {
    if (!ontology) {
        return -1;
    }
    
    return kos_ontology_save_to_file(ontology, MANUFACTURING_ONTOLOGY_FILE);
}










