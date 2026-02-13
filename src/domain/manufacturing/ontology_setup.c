// src/domain/manufacturing/ontology_setup.c
// 制造业领域类型本体初始化（基于类型构造）
// 优先使用 kos-core 形式化校验，非法类型不能创建

#include "../../../include/kos_ontology.h"
#include "../../../include/kos_core.h"
#include "../../../include/kos_core_bridge.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

// 制造业领域类型本体文件路径
#define MANUFACTURING_ONTOLOGY_FILE "manufacturing_ontology.json"

// 从 .kos 添加类型（kos-core 可用时）；否则回退到 kos_mk_* + add_type_definition
static int add_type(TypeOntology* ontology, const char* name, const char* kos_expr,
                    kos_term* fallback_term, char* err, size_t err_size) {
    if (kos_core_bridge_available()) {
        return kos_ontology_add_type_from_kos(ontology, name, kos_expr, NULL, err, err_size);
    }
    if (fallback_term && kos_type_wellformed(fallback_term)) {
        return kos_ontology_add_type_definition(ontology, name, fallback_term, NULL);
    }
    return -1;
}

// 初始化制造业领域类型本体
// 如果文件存在则加载，否则创建默认本体（优先经 kos-core 校验）
TypeOntology* kos_manufacturing_ontology_init(void) {
    // 尝试从文件加载
    TypeOntology* ontology = kos_ontology_load_from_file(MANUFACTURING_ONTOLOGY_FILE);
    
    if (ontology) {
        printf("[Manufacturing] Loaded ontology from file: %s\n", MANUFACTURING_ONTOLOGY_FILE);
        return ontology;
    }
    
    // 文件不存在，创建默认本体
    printf("[Manufacturing] Creating default ontology (kos-core validated when available)...\n");
    ontology = kos_ontology_create("manufacturing");
    if (!ontology) {
        return NULL;
    }
    
    char err[512];
    
    // ========== 基础类型定义 ==========
    
    add_type(ontology, "BatchID", "Prop BatchID", kos_mk_prop("BatchID"), err, sizeof(err));
    add_type(ontology, "Machine", "Prop Machine", kos_mk_prop("Machine"), err, sizeof(err));
    add_type(ontology, "Time", "Prop Time", kos_mk_prop("Time"), err, sizeof(err));
    add_type(ontology, "ErrorCode", "Prop ErrorCode", kos_mk_prop("ErrorCode"), err, sizeof(err));
    add_type(ontology, "Param", "Prop Param", kos_mk_prop("Param"), err, sizeof(err));
    add_type(ontology, "ParamValue", "Prop ParamValue", kos_mk_prop("ParamValue"), err, sizeof(err));
    add_type(ontology, "TimeRange", "Prop TimeRange", kos_mk_prop("TimeRange"), err, sizeof(err));
    
    // ========== 事件类型定义（使用Σ类型） ==========
    
    if (kos_core_bridge_available()) {
        add_type(ontology, "FailEvt",
                 "Sigma(b: Prop BatchID). Sigma(err: Prop ErrorCode). Sigma(t: Prop Time). Prop P",
                 NULL, err, sizeof(err));
        add_type(ontology, "ProcStep",
                 "Sigma(b: Prop BatchID). Sigma(m: Prop Machine). Sigma(dur: Prop TimeRange). Prop P",
                 NULL, err, sizeof(err));
        add_type(ontology, "Anomaly",
                 "Sigma(m: Prop Machine). Sigma(p: Prop Param). Sigma(v: Prop ParamValue). Sigma(t: Prop Time). Prop P",
                 NULL, err, sizeof(err));
        add_type(ontology, "InRoute",
                 "Pi(b: Prop BatchID). Pi(m: Prop Machine). Prop P",
                 NULL, err, sizeof(err));
        add_type(ontology, "Overlap",
                 "Pi(t: Prop Time). Pi(dur: Prop TimeRange). Prop P",
                 NULL, err, sizeof(err));
    } else {
        /* 回退：使用 kos_mk_* 构造（需良构类型） */
        kos_term* batch_id_type = kos_ontology_find_type_definition(ontology, "BatchID");
        kos_term* machine_type = kos_ontology_find_type_definition(ontology, "Machine");
        kos_term* time_type = kos_ontology_find_type_definition(ontology, "Time");
        kos_term* error_code_type = kos_ontology_find_type_definition(ontology, "ErrorCode");
        kos_term* param_type = kos_ontology_find_type_definition(ontology, "Param");
        kos_term* param_value_type = kos_ontology_find_type_definition(ontology, "ParamValue");
        kos_term* time_range_type = kos_ontology_find_type_definition(ontology, "TimeRange");
        if (batch_id_type && error_code_type && time_type) {
            kos_term* p = kos_mk_prop("Prop");
            kos_term* t1 = p ? kos_mk_sigma(time_type, p) : NULL;
            kos_term* t2 = t1 ? kos_mk_sigma(error_code_type, t1) : NULL;
            kos_term* fail_evt = t2 ? kos_mk_sigma(batch_id_type, t2) : NULL;
            if (fail_evt) { kos_ontology_add_type_definition(ontology, "FailEvt", fail_evt, NULL); kos_term_free(fail_evt); }
        }
        if (batch_id_type && machine_type && time_range_type) {
            kos_term* p = kos_mk_prop("Prop");
            kos_term* t1 = p ? kos_mk_sigma(time_range_type, p) : NULL;
            kos_term* t2 = t1 ? kos_mk_sigma(machine_type, t1) : NULL;
            kos_term* proc_step = t2 ? kos_mk_sigma(batch_id_type, t2) : NULL;
            if (proc_step) { kos_ontology_add_type_definition(ontology, "ProcStep", proc_step, NULL); kos_term_free(proc_step); }
        }
        if (machine_type && param_type && param_value_type && time_type) {
            kos_term* p = kos_mk_prop("Prop");
            kos_term* t1 = p ? kos_mk_sigma(time_type, p) : NULL;
            kos_term* t2 = t1 ? kos_mk_sigma(param_value_type, t1) : NULL;
            kos_term* t3 = t2 ? kos_mk_sigma(param_type, t2) : NULL;
            kos_term* anomaly = t3 ? kos_mk_sigma(machine_type, t3) : NULL;
            if (anomaly) { kos_ontology_add_type_definition(ontology, "Anomaly", anomaly, NULL); kos_term_free(anomaly); }
        }
        if (batch_id_type && machine_type) {
            kos_term* p = kos_mk_prop("Prop");
            kos_term* in_route = p ? kos_mk_pi(batch_id_type, kos_mk_pi(machine_type, p)) : NULL;
            if (in_route) { kos_ontology_add_type_definition(ontology, "InRoute", in_route, NULL); kos_term_free(in_route); }
        }
        if (time_type && time_range_type) {
            kos_term* p = kos_mk_prop("Prop");
            kos_term* overlap = p ? kos_mk_pi(time_type, kos_mk_pi(time_range_type, p)) : NULL;
            if (overlap) { kos_ontology_add_type_definition(ontology, "Overlap", overlap, NULL); kos_term_free(overlap); }
        }
    }
    
    // 添加扩展类型定义
    extern int kos_manufacturing_ontology_add_generated_types(TypeOntology* ontology);
    kos_manufacturing_ontology_add_generated_types(ontology);
    
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




