// src/domain/manufacturing/ontology_crud.c
// 制造业领域类型本体的CRUD操作接口
// 注意：此文件需要迁移到新的基于类型构造的API
// 当前暂时禁用，等待完整迁移

#include "../../../include/kos_ontology.h"
#include "../../../include/kos_manufacturing.h"
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

// 获取制造业类型本体（单例模式）
static TypeOntology* get_ontology(void) {
    static TypeOntology* g_ontology = NULL;
    if (!g_ontology) {
        extern TypeOntology* kos_manufacturing_ontology_init(void);
        g_ontology = kos_manufacturing_ontology_init();
    }
    return g_ontology;
}

// ========== 类型本体CRUD操作 ==========
// 注意：以下函数需要迁移到新的基于类型构造的API
// 新API使用 kos_ontology_add_type_definition, kos_ontology_find_type_definition 等函数

// TODO: 迁移所有函数以使用新的类型定义API

int kos_manufacturing_add_atomic_type(const char* name, const char* base_type) {
    // TODO: 迁移到新API
    // 新方式：使用类型构造器创建类型定义，然后调用 kos_ontology_add_type_definition
    fprintf(stderr, "[WARNING] kos_manufacturing_add_atomic_type is deprecated. Use kos_ontology_add_type_definition instead.\n");
    return -1;
}

void* kos_manufacturing_get_atomic_type(const char* name) {
    // TODO: 迁移到新API
    // 新方式：使用 kos_ontology_find_type_definition
    fprintf(stderr, "[WARNING] kos_manufacturing_get_atomic_type is deprecated. Use kos_ontology_find_type_definition instead.\n");
    return NULL;
}

void kos_manufacturing_list_atomic_types(void) {
    // TODO: 迁移到新API
    // 新方式：遍历 TypeOntology->type_definitions 数组
    fprintf(stderr, "[WARNING] kos_manufacturing_list_atomic_types is deprecated.\n");
}

int kos_manufacturing_remove_atomic_type(const char* name) {
    // TODO: 迁移到新API
    // 新方式：使用 kos_ontology_remove_type_definition
    fprintf(stderr, "[WARNING] kos_manufacturing_remove_atomic_type is deprecated. Use kos_ontology_remove_type_definition instead.\n");
    return -1;
}

int kos_manufacturing_add_predicate_type(const char* name, const char** param_types, int param_count) {
    // TODO: 迁移到新API
    // 新方式：使用类型构造器（Π类型）创建类型定义，然后调用 kos_ontology_add_type_definition
    fprintf(stderr, "[WARNING] kos_manufacturing_add_predicate_type is deprecated. Use kos_ontology_add_type_definition with Π types instead.\n");
    return -1;
}

void* kos_manufacturing_get_predicate_type(const char* name) {
    // TODO: 迁移到新API
    // 新方式：使用 kos_ontology_find_type_definition
    fprintf(stderr, "[WARNING] kos_manufacturing_get_predicate_type is deprecated. Use kos_ontology_find_type_definition instead.\n");
    return NULL;
}

void kos_manufacturing_list_predicate_types(void) {
    // TODO: 迁移到新API
    // 新方式：遍历 TypeOntology->type_definitions 数组，筛选出Π类型
    fprintf(stderr, "[WARNING] kos_manufacturing_list_predicate_types is deprecated.\n");
}

int kos_manufacturing_add_event_type(const char* name, const char** field_names, 
                                    const char** field_types, int field_count) {
    // TODO: 迁移到新API
    // 新方式：使用类型构造器（Σ类型）创建类型定义，然后调用 kos_ontology_add_type_definition
    fprintf(stderr, "[WARNING] kos_manufacturing_add_event_type is deprecated. Use kos_ontology_add_type_definition with Σ types instead.\n");
    return -1;
}

void* kos_manufacturing_get_event_type(const char* name) {
    // TODO: 迁移到新API
    // 新方式：使用 kos_ontology_find_type_definition
    fprintf(stderr, "[WARNING] kos_manufacturing_get_event_type is deprecated. Use kos_ontology_find_type_definition instead.\n");
    return NULL;
}

void kos_manufacturing_list_event_types(void) {
    // TODO: 迁移到新API
    // 新方式：遍历 TypeOntology->type_definitions 数组，筛选出Σ类型
    fprintf(stderr, "[WARNING] kos_manufacturing_list_event_types is deprecated.\n");
}

int kos_manufacturing_remove_event_type(const char* name) {
    // TODO: 迁移到新API
    // 新方式：使用 kos_ontology_remove_type_definition
    fprintf(stderr, "[WARNING] kos_manufacturing_remove_event_type is deprecated. Use kos_ontology_remove_type_definition instead.\n");
    return -1;
}
