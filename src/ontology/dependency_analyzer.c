// src/ontology/dependency_analyzer.c
// Phase 2 Enhancement: Advanced Dependency Analysis
// 增强的依赖分析：递归遍历类型结构，精确检测依赖关系

#include "../../include/kos_ontology_version.h"
#include "../../include/kos_ontology.h"
#include "../../include/kos_core.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

// ========== 类型引用检测 ==========

// 检查 kos_term 中是否包含对指定类型名的引用
static bool term_contains_type_reference(kos_term* term, const char* type_name) {
    if (!term || !type_name) {
        return false;
    }
    
    switch (term->kind) {
        case KOS_SIGMA: {
            // Σ类型：检查字段类型
            if (term->data.sigma.fields) {
                for (size_t i = 0; i < term->data.sigma.field_count; i++) {
                    kos_sigma_field_t* field = &term->data.sigma.fields[i];
                    if (field->type && term_contains_type_reference(field->type, type_name)) {
                        return true;
                    }
                }
            }
            break;
        }
        case KOS_PI: {
            // Π类型：检查参数类型和返回类型
            if (term->data.pi.param_type && 
                term_contains_type_reference(term->data.pi.param_type, type_name)) {
                return true;
            }
            if (term->data.pi.return_type && 
                term_contains_type_reference(term->data.pi.return_type, type_name)) {
                return true;
            }
            break;
        }
        case KOS_ID: {
            // ID类型：检查是否为类型引用
            if (term->data.atomic.val && strcmp(term->data.atomic.val, type_name) == 0) {
                return true;
            }
            break;
        }
        case KOS_APP: {
            // 应用：检查函数和参数
            if (term->data.app.func && 
                term_contains_type_reference(term->data.app.func, type_name)) {
                return true;
            }
            if (term->data.app.arg && 
                term_contains_type_reference(term->data.app.arg, type_name)) {
                return true;
            }
            break;
        }
        case KOS_LAMBDA: {
            // Lambda：检查参数类型和函数体
            if (term->data.lambda.param_type && 
                term_contains_type_reference(term->data.lambda.param_type, type_name)) {
                return true;
            }
            if (term->data.lambda.body && 
                term_contains_type_reference(term->data.lambda.body, type_name)) {
                return true;
            }
            break;
        }
        default:
            break;
    }
    
    return false;
}

// ========== 增强的依赖检测 ==========

// 检测依赖关系（增强版：递归遍历类型结构）
kos_dependency_item_t* kos_ontology_detect_dependencies_enhanced(
    TypeOntology* ontology,
    const char* type_name) {
    if (!ontology || !type_name) {
        return NULL;
    }
    
    kos_dependency_item_t* dependencies = NULL;
    
    // 检查其他类型定义中是否引用了该类型
    for (size_t i = 0; i < ontology->type_count; i++) {
        TypeDefinition* def = &ontology->type_definitions[i];
        
        if (!def->name || strcmp(def->name, type_name) == 0) {
            continue; // 跳过自身
        }
        
        // 递归检查类型定义中是否包含对 type_name 的引用
        bool has_reference = false;
        
        if (def->type_def && term_contains_type_reference(def->type_def, type_name)) {
            has_reference = true;
        }
        
        if (def->ctx && term_contains_type_reference(def->ctx, type_name)) {
            has_reference = true;
        }
        
        if (has_reference) {
            kos_dependency_item_t* item = (kos_dependency_item_t*)calloc(
                1, sizeof(kos_dependency_item_t)
            );
            if (item) {
                item->type = DEPENDENCY_TYPE_DEF;
                item->resource_id = strdup(def->name);
                item->description = (char*)malloc(256);
                if (item->description) {
                    snprintf(item->description, 256, 
                            "Type definition '%s' references '%s'", 
                            def->name, type_name);
                }
                item->next = dependencies;
                dependencies = item;
            }
        }
    }
    
    return dependencies;
}

// ========== 查询依赖检测 ==========

// 检测查询中的依赖（需要查询引擎支持）
kos_dependency_item_t* kos_ontology_detect_query_dependencies(
    TypeOntology* ontology,
    const char* type_name) {
    // TODO: 集成查询引擎，检测哪些查询使用了该类型
    // 当前返回空列表
    return NULL;
}

// ========== 实例依赖检测 ==========

// 检测实例中的依赖（需要存储层支持）
kos_dependency_item_t* kos_ontology_detect_instance_dependencies(
    TypeOntology* ontology,
    const char* type_name) {
    // TODO: 集成存储层，检测哪些实例使用了该类型
    // 当前返回空列表
    return NULL;
}

// ========== 完整的依赖检测 ==========

// 检测所有类型的依赖（类型定义、查询、实例）
kos_dependency_item_t* kos_ontology_detect_all_dependencies(
    TypeOntology* ontology,
    const char* type_name) {
    if (!ontology || !type_name) {
        return NULL;
    }
    
    kos_dependency_item_t* all_deps = NULL;
    
    // 检测类型定义依赖
    kos_dependency_item_t* type_deps = kos_ontology_detect_dependencies_enhanced(ontology, type_name);
    if (type_deps) {
        // 合并到总列表
        kos_dependency_item_t* last = type_deps;
        while (last->next) {
            last = last->next;
        }
        last->next = all_deps;
        all_deps = type_deps;
    }
    
    // 检测查询依赖
    kos_dependency_item_t* query_deps = kos_ontology_detect_query_dependencies(ontology, type_name);
    if (query_deps) {
        kos_dependency_item_t* last = query_deps;
        while (last->next) {
            last = last->next;
        }
        last->next = all_deps;
        all_deps = query_deps;
    }
    
    // 检测实例依赖
    kos_dependency_item_t* instance_deps = kos_ontology_detect_instance_dependencies(ontology, type_name);
    if (instance_deps) {
        kos_dependency_item_t* last = instance_deps;
        while (last->next) {
            last = last->next;
        }
        last->next = all_deps;
        all_deps = instance_deps;
    }
    
    return all_deps;
}
