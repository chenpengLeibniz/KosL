// src/ontology/impact_analysis.c
// KOS Dynamic Ontology Management - Impact Analysis
// 变更影响分析：依赖检测、影响范围分析、迁移脚本生成

#include "../../include/kos_ontology_version.h"
#include "../../include/kos_ontology.h"
#include "../../include/kos_query.h"
#include "../../include/kos_core.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

// ========== 依赖检测 ==========

// 检测依赖关系
kos_dependency_item_t* kos_ontology_detect_dependencies(
    TypeOntology* ontology,
    const char* type_name) {
    if (!ontology || !type_name) {
        return NULL;
    }
    
    kos_dependency_item_t* dependencies = NULL;
    
    // 简化实现：检查其他类型定义中是否引用了该类型
    for (size_t i = 0; i < ontology->type_count; i++) {
        TypeDefinition* def = &ontology->type_definitions[i];
        
        if (!def->name || strcmp(def->name, type_name) == 0) {
            continue; // 跳过自身
        }
        
        // 检查类型定义中是否包含对 type_name 的引用
        // 简化实现：检查类型定义的字符串表示中是否包含类型名
        // 实际应该递归遍历 kos_term 结构
        
        if (def->type_def) {
            // 这里简化处理：假设如果类型定义存在，可能包含引用
            // 实际应该使用更精确的依赖检测算法
            
            kos_dependency_item_t* item = (kos_dependency_item_t*)calloc(
                1, sizeof(kos_dependency_item_t)
            );
            if (item) {
                item->type = DEPENDENCY_TYPE_DEF;
                item->resource_id = strdup(def->name);
                item->description = (char*)malloc(256);
                if (item->description) {
                    snprintf(item->description, 256, 
                            "Type definition '%s' may reference '%s'", 
                            def->name, type_name);
                }
                item->next = dependencies;
                dependencies = item;
            }
        }
    }
    
    return dependencies;
}

// 释放依赖项列表
void kos_dependency_list_free(kos_dependency_item_t* dependencies) {
    kos_dependency_item_t* node = dependencies;
    while (node) {
        kos_dependency_item_t* next = node->next;
        if (node->resource_id) {
            free(node->resource_id);
        }
        if (node->description) {
            free(node->description);
        }
        free(node);
        node = next;
    }
}

// ========== 影响分析 ==========

// 分析类型变更的影响
kos_impact_analysis_t* kos_ontology_analyze_impact(
    TypeOntology* ontology,
    const char* type_name,
    kos_term* new_type_def,
    kos_term* new_ctx) {
    if (!ontology || !type_name || !new_type_def) {
        return NULL;
    }
    
    kos_impact_analysis_t* analysis = (kos_impact_analysis_t*)calloc(
        1, sizeof(kos_impact_analysis_t)
    );
    if (!analysis) {
        return NULL;
    }
    
    analysis->type_name = strdup(type_name);
    analysis->new_type_def = kos_term_copy(new_type_def);
    analysis->new_ctx = new_ctx ? kos_term_copy(new_ctx) : NULL;
    
    // 获取旧类型定义
    TypeDefinition* old_def = kos_ontology_get_type_definition_info(ontology, type_name);
    if (old_def) {
        analysis->old_type_def = old_def->type_def ? kos_term_copy(old_def->type_def) : NULL;
    } else {
        // 新类型，没有旧定义
        analysis->old_type_def = NULL;
    }
    
    // 检测依赖关系
    analysis->dependencies = kos_ontology_detect_dependencies(ontology, type_name);
    
    // 计算受影响的资源数量（简化实现）
    kos_dependency_item_t* dep = analysis->dependencies;
    while (dep) {
        switch (dep->type) {
            case DEPENDENCY_INSTANCE:
                analysis->affected_instance_count++;
                break;
            case DEPENDENCY_QUERY:
                analysis->affected_query_count++;
                break;
            case DEPENDENCY_TYPE_DEF:
                // 类型定义依赖可能影响实例和查询
                analysis->affected_instance_count++;
                analysis->affected_query_count++;
                break;
            default:
                break;
        }
        dep = dep->next;
    }
    
    // 判断是否为破坏性变更（简化实现）
    // 实际应该比较类型定义的兼容性
    analysis->breaking_change = (analysis->old_type_def != NULL && 
                                analysis->affected_instance_count > 0);
    
    // 生成迁移提示
    if (analysis->breaking_change) {
        analysis->migration_hint = (char*)malloc(512);
        if (analysis->migration_hint) {
            snprintf(analysis->migration_hint, 512,
                    "Type '%s' has been modified. %zu instances and %zu queries may be affected. "
                    "Consider running migration script.",
                    type_name, 
                    analysis->affected_instance_count,
                    analysis->affected_query_count);
        }
    } else {
        analysis->migration_hint = strdup("No migration required.");
    }
    
    return analysis;
}

// 分析批量更新的影响
kos_impact_analysis_t** kos_ontology_analyze_batch_impact(
    TypeOntology* ontology,
    kos_ontology_update_t* updates,
    size_t count,
    size_t* analysis_count) {
    if (!ontology || !updates || count == 0 || !analysis_count) {
        return NULL;
    }
    
    kos_impact_analysis_t** analyses = (kos_impact_analysis_t**)malloc(
        count * sizeof(kos_impact_analysis_t*)
    );
    if (!analyses) {
        *analysis_count = 0;
        return NULL;
    }
    
    *analysis_count = 0;
    
    for (size_t i = 0; i < count; i++) {
        if (updates[i].op == UPDATE_MODIFY_TYPE || updates[i].op == UPDATE_ADD_TYPE) {
            analyses[*analysis_count] = kos_ontology_analyze_impact(
                ontology,
                updates[i].type_name,
                updates[i].new_type_def,
                updates[i].new_ctx
            );
            if (analyses[*analysis_count]) {
                (*analysis_count)++;
            }
        }
    }
    
    return analyses;
}

// 释放影响分析结果
void kos_impact_analysis_free(kos_impact_analysis_t* analysis) {
    if (!analysis) {
        return;
    }
    
    if (analysis->type_name) {
        free(analysis->type_name);
    }
    
    if (analysis->old_type_def) {
        kos_term_free(analysis->old_type_def);
    }
    
    if (analysis->new_type_def) {
        kos_term_free(analysis->new_type_def);
    }
    
    if (analysis->old_ctx) {
        kos_term_free(analysis->old_ctx);
    }
    
    if (analysis->new_ctx) {
        kos_term_free(analysis->new_ctx);
    }
    
    if (analysis->dependencies) {
        kos_dependency_list_free(analysis->dependencies);
    }
    
    if (analysis->migration_hint) {
        free(analysis->migration_hint);
    }
    
    free(analysis);
}

// ========== 迁移脚本生成 ==========

// 生成迁移脚本
kos_migration_script_t* kos_ontology_generate_migration(
    kos_impact_analysis_t* impact) {
    if (!impact) {
        return NULL;
    }
    
    kos_migration_script_t* script = (kos_migration_script_t*)calloc(
        1, sizeof(kos_migration_script_t)
    );
    if (!script) {
        return NULL;
    }
    
    script->impact = impact;
    
    // 生成迁移脚本内容（JSON 格式）
    size_t buffer_size = 4096;
    char* buffer = (char*)malloc(buffer_size);
    if (!buffer) {
        free(script);
        return NULL;
    }
    
    int pos = 0;
    pos += snprintf(buffer + pos, buffer_size - pos, "{\n");
    pos += snprintf(buffer + pos, buffer_size - pos, "  \"type_name\": \"%s\",\n", impact->type_name);
    pos += snprintf(buffer + pos, buffer_size - pos, "  \"breaking_change\": %s,\n", 
                   impact->breaking_change ? "true" : "false");
    pos += snprintf(buffer + pos, buffer_size - pos, "  \"affected_instances\": %zu,\n", 
                   impact->affected_instance_count);
    pos += snprintf(buffer + pos, buffer_size - pos, "  \"affected_queries\": %zu,\n", 
                   impact->affected_query_count);
    pos += snprintf(buffer + pos, buffer_size - pos, "  \"migration_hint\": \"%s\",\n", 
                   impact->migration_hint ? impact->migration_hint : "");
    pos += snprintf(buffer + pos, buffer_size - pos, "  \"steps\": [\n");
    
    // 添加迁移步骤（简化实现）
    if (impact->breaking_change) {
        pos += snprintf(buffer + pos, buffer_size - pos, 
                       "    {\"action\": \"validate_instances\", \"type\": \"%s\"},\n",
                       impact->type_name);
        pos += snprintf(buffer + pos, buffer_size - pos, 
                       "    {\"action\": \"update_queries\", \"type\": \"%s\"}\n",
                       impact->type_name);
    } else {
        pos += snprintf(buffer + pos, buffer_size - pos, 
                       "    {\"action\": \"no_migration_needed\"}\n");
    }
    
    pos += snprintf(buffer + pos, buffer_size - pos, "  ]\n");
    pos += snprintf(buffer + pos, buffer_size - pos, "}\n");
    
    script->script_content = buffer;
    script->script_length = pos;
    
    return script;
}

// 生成批量更新的迁移脚本
kos_migration_script_t* kos_ontology_generate_batch_migration(
    kos_impact_analysis_t** analyses,
    size_t count) {
    if (!analyses || count == 0) {
        return NULL;
    }
    
    // 合并所有影响分析生成一个综合迁移脚本
    size_t buffer_size = 8192;
    char* buffer = (char*)malloc(buffer_size);
    if (!buffer) {
        return NULL;
    }
    
    int pos = 0;
    pos += snprintf(buffer + pos, buffer_size - pos, "{\n");
    pos += snprintf(buffer + pos, buffer_size - pos, "  \"batch_migration\": true,\n");
    pos += snprintf(buffer + pos, buffer_size - pos, "  \"type_count\": %zu,\n", count);
    pos += snprintf(buffer + pos, buffer_size - pos, "  \"migrations\": [\n");
    
    for (size_t i = 0; i < count; i++) {
        if (analyses[i]) {
            kos_migration_script_t* script = kos_ontology_generate_migration(analyses[i]);
            if (script && script->script_content) {
                pos += snprintf(buffer + pos, buffer_size - pos, "    %s", script->script_content);
                if (i < count - 1) {
                    pos += snprintf(buffer + pos, buffer_size - pos, ",\n");
                }
                kos_migration_script_free(script);
            }
        }
    }
    
    pos += snprintf(buffer + pos, buffer_size - pos, "\n  ]\n");
    pos += snprintf(buffer + pos, buffer_size - pos, "}\n");
    
    kos_migration_script_t* script = (kos_migration_script_t*)calloc(
        1, sizeof(kos_migration_script_t)
    );
    if (script) {
        script->script_content = buffer;
        script->script_length = pos;
        script->impact = NULL; // 批量迁移不关联单个影响分析
    }
    
    return script;
}

// 执行迁移脚本（应用到知识集 K）
int kos_ontology_apply_migration(
    kos_term** K,
    kos_migration_script_t* script) {
    if (!K || !script || !script->script_content) {
        return -1;
    }
    
    // 简化实现：迁移脚本执行
    // 实际应该解析 JSON 并执行迁移步骤
    
    // 这里只是占位实现
    printf("[Migration] Applying migration script (length=%zu)\n", script->script_length);
    printf("[Migration] Script content:\n%s\n", script->script_content);
    
    return 0;
}

// 释放迁移脚本
void kos_migration_script_free(kos_migration_script_t* script) {
    if (!script) {
        return;
    }
    
    if (script->script_content) {
        free(script->script_content);
    }
    
    // 注意：不释放 impact，因为它可能被其他地方使用
    
    free(script);
}
