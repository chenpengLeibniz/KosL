// src/domain/manufacturing/ontology_setup.c
// 制造业领域类型本体初始化（基于类型构造）
// 使用类型构造器（Π、Σ、Sum等）构造所有类型定义

#include "../../../include/kos_ontology.h"
#include "../../../include/kos_core.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

// 制造业领域类型本体文件路径
#define MANUFACTURING_ONTOLOGY_FILE "manufacturing_ontology.json"

// 初始化制造业领域类型本体
// 如果文件存在则加载，否则创建默认本体（使用类型构造器）
TypeOntology* kos_manufacturing_ontology_init(void) {
    // 尝试从文件加载
    TypeOntology* ontology = kos_ontology_load_from_file(MANUFACTURING_ONTOLOGY_FILE);
    
    if (ontology) {
        printf("[Manufacturing] Loaded ontology from file: %s\n", MANUFACTURING_ONTOLOGY_FILE);
        return ontology;
    }
    
    // 文件不存在，创建默认本体
    printf("[Manufacturing] Creating default ontology using type constructors...\n");
    ontology = kos_ontology_create("manufacturing");
    if (!ontology) {
        return NULL;
    }
    
    // ========== 基础类型定义（使用基础Sort） ==========
    
    // BatchID类型：基于ID基础Sort
    kos_term* batch_id_type = kos_mk_id("BatchID");
    kos_ontology_add_type_definition(ontology, "BatchID", batch_id_type, NULL);
    
    // Machine类型：基于ID基础Sort（简化处理，实际可以是更复杂的类型）
    kos_term* machine_type = kos_mk_id("Machine");
    kos_ontology_add_type_definition(ontology, "Machine", machine_type, NULL);
    
    // Time类型：基于Time基础Sort
    kos_term* time_type = kos_mk_time("Time");
    kos_ontology_add_type_definition(ontology, "Time", time_type, NULL);
    
    // ErrorCode类型：基于Prop（简化处理）
    kos_term* error_code_type = kos_mk_prop("ErrorCode");
    kos_ontology_add_type_definition(ontology, "ErrorCode", error_code_type, NULL);
    
    // Param类型：基于Prop（简化处理）
    kos_term* param_type = kos_mk_prop("Param");
    kos_ontology_add_type_definition(ontology, "Param", param_type, NULL);
    
    // ParamValue类型：基于Prop（简化处理）
    kos_term* param_value_type = kos_mk_prop("ParamValue");
    kos_ontology_add_type_definition(ontology, "ParamValue", param_value_type, NULL);
    
    // ========== 事件类型定义（使用Σ类型） ==========
    
    // FailEvt类型：Σ(b: BatchID). Σ(err: ErrorCode). Σ(t: Time). Prop
    kos_term* prop_type = kos_mk_prop("Prop");
    
    kos_term* time_prop_sigma = kos_mk_sigma(time_type, prop_type);
    kos_term* error_time_prop_sigma = kos_mk_sigma(error_code_type, time_prop_sigma);
    kos_term* fail_evt_type = kos_mk_sigma(batch_id_type, error_time_prop_sigma);
    kos_ontology_add_type_definition(ontology, "FailEvt", fail_evt_type, NULL);
    
    // ProcStep类型：Σ(b: BatchID). Σ(m: Machine). Σ(dur: TimeRange). Prop
    // 注意：TimeRange需要先定义，这里简化处理
    kos_term* time_range_type = kos_mk_prop("TimeRange");
    kos_term* time_range_prop_sigma = kos_mk_sigma(time_range_type, prop_type);
    kos_term* machine_time_range_prop_sigma = kos_mk_sigma(machine_type, time_range_prop_sigma);
    kos_term* proc_step_type = kos_mk_sigma(batch_id_type, machine_time_range_prop_sigma);
    kos_ontology_add_type_definition(ontology, "ProcStep", proc_step_type, NULL);
    
    // Anomaly类型：Σ(m: Machine). Σ(p: Param). Σ(v: ParamValue). Σ(t: Time). Prop
    kos_term* time_prop_sigma_anomaly = kos_mk_sigma(time_type, prop_type);
    kos_term* param_value_time_prop_sigma = kos_mk_sigma(param_value_type, time_prop_sigma_anomaly);
    kos_term* param_param_value_time_prop_sigma = kos_mk_sigma(param_type, param_value_time_prop_sigma);
    kos_term* anomaly_type = kos_mk_sigma(machine_type, param_param_value_time_prop_sigma);
    kos_ontology_add_type_definition(ontology, "Anomaly", anomaly_type, NULL);
    
    // ========== 谓词类型定义（使用Π类型） ==========
    
    // InRoute类型：Π(b: BatchID). Π(m: Machine). Prop
    kos_term* machine_prop_pi = kos_mk_pi(machine_type, prop_type);
    kos_term* in_route_type = kos_mk_pi(batch_id_type, machine_prop_pi);
    kos_ontology_add_type_definition(ontology, "InRoute", in_route_type, NULL);
    
    // Overlap类型：Π(t: Time). Π(dur: TimeRange). Prop
    kos_term* time_range_prop_pi = kos_mk_pi(time_range_type, prop_type);
    kos_term* overlap_type = kos_mk_pi(time_type, time_range_prop_pi);
    kos_ontology_add_type_definition(ontology, "Overlap", overlap_type, NULL);
    
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
















