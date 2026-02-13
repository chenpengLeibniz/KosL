// src/ontology/change_propagation.c
// Phase 2 Enhancement: Change Propagation System
// 变更传播系统：通知依赖系统、级联更新、变更链追踪

#include "../../include/kos_ontology_version.h"
#include "../../include/kos_ontology.h"
#include "../../include/kos_core.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <time.h>

// ========== 变更传播链 ==========

// 变更传播节点
typedef struct propagation_node {
    char* type_name;                    // 变更的类型名
    update_operation_t operation;       // 操作类型
    kos_term* old_type_def;            // 旧类型定义
    kos_term* new_type_def;            // 新类型定义
    struct propagation_node* next;      // 下一个节点
} propagation_node_t;

// 变更传播链
typedef struct {
    propagation_node_t* head;           // 链头
    propagation_node_t* tail;           // 链尾
    size_t count;                       // 节点数量
} propagation_chain_t;

// 创建传播链
static propagation_chain_t* create_propagation_chain(void) {
    propagation_chain_t* chain = (propagation_chain_t*)calloc(1, sizeof(propagation_chain_t));
    return chain;
}

// 添加传播节点
static void add_propagation_node(
    propagation_chain_t* chain,
    const char* type_name,
    update_operation_t op,
    kos_term* old_def,
    kos_term* new_def) {
    if (!chain || !type_name) {
        return;
    }
    
    propagation_node_t* node = (propagation_node_t*)calloc(1, sizeof(propagation_node_t));
    if (!node) {
        return;
    }
    
    node->type_name = strdup(type_name);
    node->operation = op;
    node->old_type_def = old_def ? kos_term_copy(old_def) : NULL;
    node->new_type_def = new_def ? kos_term_copy(new_def) : NULL;
    node->next = NULL;
    
    if (!chain->head) {
        chain->head = chain->tail = node;
    } else {
        chain->tail->next = node;
        chain->tail = node;
    }
    chain->count++;
}

// 释放传播链
static void free_propagation_chain(propagation_chain_t* chain) {
    if (!chain) {
        return;
    }
    
    propagation_node_t* node = chain->head;
    while (node) {
        propagation_node_t* next = node->next;
        if (node->type_name) {
            free(node->type_name);
        }
        if (node->old_type_def) {
            kos_term_free(node->old_type_def);
        }
        if (node->new_type_def) {
            kos_term_free(node->new_type_def);
        }
        free(node);
        node = next;
    }
    free(chain);
}

// ========== 级联变更检测 ==========

// 检测级联变更（当一个类型变更时，检测哪些依赖类型也需要更新）
propagation_chain_t* kos_ontology_detect_cascade_changes(
    TypeOntology* ontology,
    const char* changed_type_name,
    kos_term* new_type_def) {
    if (!ontology || !changed_type_name) {
        return NULL;
    }
    
    propagation_chain_t* chain = create_propagation_chain();
    if (!chain) {
        return NULL;
    }
    
    // 添加初始变更
    TypeDefinition* old_def = kos_ontology_get_type_definition_info(ontology, changed_type_name);
    add_propagation_node(chain, changed_type_name, UPDATE_MODIFY_TYPE,
                        old_def ? old_def->type_def : NULL, new_type_def);
    
    // 检测依赖该类型的其他类型
    // 简化实现：检查所有类型定义
    for (size_t i = 0; i < ontology->type_count; i++) {
        TypeDefinition* def = &ontology->type_definitions[i];
        
        if (!def->name || strcmp(def->name, changed_type_name) == 0) {
            continue;
        }
        
        // 检查该类型是否依赖 changed_type_name
        // 简化实现：检查类型定义中是否包含引用
        // 实际应该使用依赖分析器
        bool depends_on_changed = false;
        
        if (def->type_def) {
            // TODO: 使用 term_contains_type_reference 检查
            // 这里简化处理
            depends_on_changed = true; // 占位实现
        }
        
        if (depends_on_changed) {
            // 检测该依赖类型是否需要级联更新
            // 简化实现：标记为可能需要更新
            add_propagation_node(chain, def->name, UPDATE_MODIFY_TYPE,
                                def->type_def, def->type_def);
        }
    }
    
    return chain;
}

// ========== 变更传播执行 ==========

// 执行变更传播（应用级联变更）
int kos_ontology_propagate_changes(
    TypeOntology* ontology,
    propagation_chain_t* chain,
    const char* version_name,
    const char* description) {
    if (!ontology || !chain) {
        return -1;
    }
    
    // 开始事务
    kos_ontology_transaction_t* transaction = kos_ontology_begin_transaction(ontology);
    if (!transaction) {
        return -1;
    }
    
    // 应用传播链中的所有变更
    propagation_node_t* node = chain->head;
    while (node) {
        int result = kos_ontology_transaction_add_update(
            transaction,
            node->operation,
            node->type_name,
            node->new_type_def,
            NULL  // 上下文
        );
        
        if (result != 0) {
            // 失败，回滚事务
            kos_ontology_rollback_transaction(ontology, transaction);
            kos_ontology_transaction_free(transaction);
            return -1;
        }
        
        node = node->next;
    }
    
    // 提交事务
    int result = kos_ontology_commit_transaction(ontology, transaction, version_name, description);
    kos_ontology_transaction_free(transaction);
    
    return result;
}

// ========== 变更通知增强 ==========

// 变更通知上下文
typedef struct {
    TypeOntology* ontology;
    propagation_chain_t* propagation_chain;
    kos_impact_analysis_t** impact_analyses;
    size_t impact_count;
} change_notification_context_t;

// 增强的变更通知（包含影响分析）
void kos_ontology_notify_change_with_impact(
    TypeOntology* ontology,
    const char* type_name,
    update_operation_t op,
    kos_term* new_type_def,
    kos_term* new_ctx) {
    if (!ontology || !type_name) {
        return;
    }
    
    // 执行影响分析
    kos_impact_analysis_t* impact = NULL;
    if (op == UPDATE_MODIFY_TYPE || op == UPDATE_ADD_TYPE) {
        impact = kos_ontology_analyze_impact(ontology, type_name, new_type_def, new_ctx);
    }
    
    // 检测级联变更
    propagation_chain_t* cascade = NULL;
    if (op == UPDATE_MODIFY_TYPE && new_type_def) {
        cascade = kos_ontology_detect_cascade_changes(ontology, type_name, new_type_def);
    }
    
    // 触发标准变更通知回调
    // （这里应该调用 runtime_update.c 中的 notify_change_callbacks）
    
    // 清理
    if (impact) {
        kos_impact_analysis_free(impact);
    }
    if (cascade) {
        free_propagation_chain(cascade);
    }
}

// ========== 变更历史追踪 ==========

// 变更历史记录
typedef struct change_history_entry {
    char* type_name;
    update_operation_t operation;
    char* timestamp;
    char* version_name;
    kos_term* type_def_snapshot;
    struct change_history_entry* next;
} change_history_entry_t;

// 变更历史（附加到本体）
static struct change_history_entry* g_change_histories[256] = {0};

// 记录变更历史
void kos_ontology_record_change_history(
    TypeOntology* ontology,
    const char* type_name,
    update_operation_t op,
    const char* version_name) {
    if (!ontology || !type_name) {
        return;
    }
    
    size_t index = 0;
    if (ontology->domain_name) {
        for (const char* p = ontology->domain_name; *p; p++) {
            index = (index * 31 + *p) % 256;
        }
    }
    
    struct change_history_entry* entry = (struct change_history_entry*)calloc(
        1, sizeof(struct change_history_entry)
    );
    if (!entry) {
        return;
    }
    
    entry->type_name = strdup(type_name);
    entry->operation = op;
    entry->version_name = version_name ? strdup(version_name) : NULL;
    
    // 获取时间戳
    time_t now = time(NULL);
    char* timestamp = (char*)malloc(32);
    if (timestamp) {
        strftime(timestamp, 32, "%Y-%m-%d %H:%M:%S", localtime(&now));
        entry->timestamp = timestamp;
    }
    
    // 保存类型定义快照
    TypeDefinition* def = kos_ontology_get_type_definition_info(ontology, type_name);
    if (def && def->type_def) {
        entry->type_def_snapshot = kos_term_copy(def->type_def);
    }
    
    // 添加到历史列表
    entry->next = g_change_histories[index];
    g_change_histories[index] = entry;
}

// 获取变更历史
change_history_entry_t* kos_ontology_get_change_history(
    TypeOntology* ontology,
    const char* type_name) {
    if (!ontology) {
        return NULL;
    }
    
    size_t index = 0;
    if (ontology->domain_name) {
        for (const char* p = ontology->domain_name; *p; p++) {
            index = (index * 31 + *p) % 256;
        }
    }
    
    change_history_entry_t* result = NULL;
    struct change_history_entry* node = g_change_histories[index];
    
    while (node) {
        if (!type_name || strcmp(node->type_name, type_name) == 0) {
            // 复制节点
            change_history_entry_t* copy = (change_history_entry_t*)calloc(
                1, sizeof(change_history_entry_t)
            );
            if (copy) {
                copy->type_name = node->type_name ? strdup(node->type_name) : NULL;
                copy->operation = node->operation;
                copy->timestamp = node->timestamp ? strdup(node->timestamp) : NULL;
                copy->version_name = node->version_name ? strdup(node->version_name) : NULL;
                copy->type_def_snapshot = node->type_def_snapshot ? 
                    kos_term_copy(node->type_def_snapshot) : NULL;
                copy->next = result;
                result = copy;
            }
        }
        node = node->next;
    }
    
    return result;
}

// 释放变更历史
void kos_ontology_free_change_history(change_history_entry_t* history) {
    change_history_entry_t* node = history;
    while (node) {
        change_history_entry_t* next = node->next;
        if (node->type_name) free(node->type_name);
        if (node->timestamp) free(node->timestamp);
        if (node->version_name) free(node->version_name);
        if (node->type_def_snapshot) kos_term_free(node->type_def_snapshot);
        free(node);
        node = next;
    }
}
