// src/ontology/runtime_update.c
// KOS Dynamic Ontology Management - Runtime Update
// 运行时本体更新：原子性更新、批量更新、变更传播

#include "../../include/kos_ontology_version.h"
#include "../../include/kos_ontology.h"
#include "../../include/kos_core.h"
#include "../../include/kos_core_bridge.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

#define INITIAL_TRANSACTION_CAPACITY 8

// ========== 变更通知回调管理 ==========

// 变更通知回调节点
typedef struct callback_node {
    kos_ontology_change_callback_t callback;
    void* user_data;
    struct callback_node* next;
} callback_node_t;

// 回调列表（附加到本体）
static callback_node_t* g_callbacks[256] = {0};

// 获取回调列表索引
static size_t get_callback_index(TypeOntology* ontology) {
    if (!ontology || !ontology->domain_name) {
        return 0;
    }
    
    size_t index = 0;
    for (const char* p = ontology->domain_name; *p; p++) {
        index = (index * 31 + *p) % 256;
    }
    return index;
}

// 触发变更通知
static void notify_change_callbacks(
    TypeOntology* ontology,
    const char* type_name,
    update_operation_t op) {
    size_t index = get_callback_index(ontology);
    callback_node_t* node = g_callbacks[index];
    
    while (node) {
        if (node->callback) {
            node->callback(type_name, op, node->user_data);
        }
        node = node->next;
    }
}

// ========== 事务管理 API ==========

// 开始更新事务
kos_ontology_transaction_t* kos_ontology_begin_transaction(TypeOntology* ontology) {
    if (!ontology) {
        return NULL;
    }
    
    kos_ontology_transaction_t* transaction = (kos_ontology_transaction_t*)calloc(
        1, sizeof(kos_ontology_transaction_t)
    );
    if (!transaction) {
        return NULL;
    }
    
    transaction->capacity = INITIAL_TRANSACTION_CAPACITY;
    transaction->updates = (kos_ontology_update_t*)malloc(
        transaction->capacity * sizeof(kos_ontology_update_t)
    );
    if (!transaction->updates) {
        free(transaction);
        return NULL;
    }
    
    transaction->count = 0;
    transaction->committed = false;
    
    // 保存更新前的版本
    transaction->version_before = kos_ontology_get_current_version(ontology);
    
    return transaction;
}

// 添加更新操作到事务
int kos_ontology_transaction_add_update(
    kos_ontology_transaction_t* transaction,
    update_operation_t op,
    const char* type_name,
    kos_term* new_type_def,
    kos_term* new_ctx) {
    if (!transaction || !type_name) {
        return -1;
    }
    
    if (transaction->committed) {
        return -1; // 事务已提交，不能添加更新
    }
    
    // 扩容
    if (transaction->count >= transaction->capacity) {
        transaction->capacity *= 2;
        transaction->updates = (kos_ontology_update_t*)realloc(
            transaction->updates,
            transaction->capacity * sizeof(kos_ontology_update_t)
        );
        if (!transaction->updates) {
            return -1;
        }
    }
    
    // 添加更新操作
    kos_ontology_update_t* update = &transaction->updates[transaction->count++];
    update->op = op;
    update->type_name = strdup(type_name);
    update->new_type_def = new_type_def ? kos_term_copy(new_type_def) : NULL;
    update->new_ctx = new_ctx ? kos_term_copy(new_ctx) : NULL;
    update->old_type_def = NULL;
    update->old_ctx = NULL;
    
    return 0;
}

// 从 .kos 源添加更新操作（经 kos-core 校验）
int kos_ontology_transaction_add_update_from_kos(
    kos_ontology_transaction_t* transaction,
    update_operation_t op,
    const char* type_name,
    const char* kos_type_expr,
    kos_term* new_ctx,
    char* errmsg,
    size_t errmsg_size) {
    if (!transaction || !type_name || !kos_type_expr) {
        if (errmsg && errmsg_size > 0) snprintf(errmsg, errmsg_size, "Invalid arguments");
        return -1;
    }
    kos_term* type_def = (kos_term*)kos_core_bridge_term_from_kos(kos_type_expr, errmsg, errmsg_size);
    if (!type_def) {
        return -1;
    }
    int r = kos_ontology_transaction_add_update(transaction, op, type_name, type_def, new_ctx);
    kos_term_free(type_def);
    return r;
}

// 提交事务（原子性）
int kos_ontology_commit_transaction(
    TypeOntology* ontology,
    kos_ontology_transaction_t* transaction,
    const char* version_name,
    const char* description) {
    if (!ontology || !transaction || transaction->committed) {
        return -1;
    }
    
    // 保存旧类型定义（用于回滚）
    for (size_t i = 0; i < transaction->count; i++) {
        kos_ontology_update_t* update = &transaction->updates[i];
        
        if (update->op == UPDATE_MODIFY_TYPE || update->op == UPDATE_REMOVE_TYPE) {
            // 查找旧类型定义
            TypeDefinition* old_def = kos_ontology_get_type_definition_info(
                ontology, update->type_name
            );
            if (old_def) {
                update->old_type_def = old_def->type_def ? kos_term_copy(old_def->type_def) : NULL;
                update->old_ctx = old_def->ctx ? kos_term_copy(old_def->ctx) : NULL;
            }
        }
    }
    
    // 执行所有更新操作
    for (size_t i = 0; i < transaction->count; i++) {
        kos_ontology_update_t* update = &transaction->updates[i];
        
        switch (update->op) {
            case UPDATE_ADD_TYPE:
                kos_ontology_add_type_definition(
                    ontology,
                    update->type_name,
                    update->new_type_def,
                    update->new_ctx
                );
                notify_change_callbacks(ontology, update->type_name, UPDATE_ADD_TYPE);
                break;
                
            case UPDATE_REMOVE_TYPE:
                kos_ontology_remove_type_definition(ontology, update->type_name);
                notify_change_callbacks(ontology, update->type_name, UPDATE_REMOVE_TYPE);
                break;
                
            case UPDATE_MODIFY_TYPE:
                kos_ontology_update_type_definition(
                    ontology,
                    update->type_name,
                    update->new_type_def,
                    update->new_ctx
                );
                notify_change_callbacks(ontology, update->type_name, UPDATE_MODIFY_TYPE);
                break;
        }
    }
    
    // 创建新版本
    if (version_name) {
        transaction->version_after = kos_ontology_create_version(
            ontology, version_name, description
        );
    }
    
    transaction->committed = true;
    
    return 0;
}

// 回滚事务
int kos_ontology_rollback_transaction(
    TypeOntology* ontology,
    kos_ontology_transaction_t* transaction) {
    if (!ontology || !transaction || transaction->committed) {
        return -1;
    }
    
    // 反向执行所有更新操作
    for (int i = (int)transaction->count - 1; i >= 0; i--) {
        kos_ontology_update_t* update = &transaction->updates[i];
        
        switch (update->op) {
            case UPDATE_ADD_TYPE:
                // 回滚：删除添加的类型
                kos_ontology_remove_type_definition(ontology, update->type_name);
                break;
                
            case UPDATE_REMOVE_TYPE:
                // 回滚：恢复删除的类型
                if (update->old_type_def) {
                    kos_ontology_add_type_definition(
                        ontology,
                        update->type_name,
                        update->old_type_def,
                        update->old_ctx
                    );
                }
                break;
                
            case UPDATE_MODIFY_TYPE:
                // 回滚：恢复旧类型定义
                if (update->old_type_def) {
                    kos_ontology_update_type_definition(
                        ontology,
                        update->type_name,
                        update->old_type_def,
                        update->old_ctx
                    );
                }
                break;
        }
    }
    
    return 0;
}

// 释放事务对象
void kos_ontology_transaction_free(kos_ontology_transaction_t* transaction) {
    if (!transaction) {
        return;
    }
    
    if (transaction->updates) {
        for (size_t i = 0; i < transaction->count; i++) {
            kos_ontology_update_t* update = &transaction->updates[i];
            if (update->type_name) {
                free(update->type_name);
            }
            if (update->new_type_def) {
                kos_term_free(update->new_type_def);
            }
            if (update->old_type_def) {
                kos_term_free(update->old_type_def);
            }
            if (update->new_ctx) {
                kos_term_free(update->new_ctx);
            }
            if (update->old_ctx) {
                kos_term_free(update->old_ctx);
            }
        }
        free(transaction->updates);
    }
    
    // 注意：不释放 version_before 和 version_after，它们由版本管理器管理
    
    free(transaction);
}

// ========== 批量更新 API ==========

// 批量更新（原子性）
int kos_ontology_batch_update(
    TypeOntology* ontology,
    kos_ontology_update_t* updates,
    size_t count,
    const char* version_name,
    const char* description) {
    if (!ontology || !updates || count == 0) {
        return -1;
    }
    
    // 开始事务
    kos_ontology_transaction_t* transaction = kos_ontology_begin_transaction(ontology);
    if (!transaction) {
        return -1;
    }
    
    // 添加所有更新操作
    for (size_t i = 0; i < count; i++) {
        int result = kos_ontology_transaction_add_update(
            transaction,
            updates[i].op,
            updates[i].type_name,
            updates[i].new_type_def,
            updates[i].new_ctx
        );
        if (result != 0) {
            kos_ontology_rollback_transaction(ontology, transaction);
            kos_ontology_transaction_free(transaction);
            return -1;
        }
    }
    
    // 提交事务
    int result = kos_ontology_commit_transaction(ontology, transaction, version_name, description);
    
    kos_ontology_transaction_free(transaction);
    
    return result;
}

// ========== 原子性更新单个类型 ==========

// 原子性更新类型定义
int kos_ontology_update_atomic(
    TypeOntology* ontology,
    const char* type_name,
    kos_term* new_type_def,
    kos_term* new_ctx,
    const char* version_name,
    const char* description) {
    if (!ontology || !type_name || !new_type_def) {
        return -1;
    }
    
    // 检查类型是否存在
    TypeDefinition* old_def = kos_ontology_get_type_definition_info(ontology, type_name);
    
    update_operation_t op = old_def ? UPDATE_MODIFY_TYPE : UPDATE_ADD_TYPE;
    
    kos_ontology_update_t update;
    update.op = op;
    update.type_name = (char*)type_name; // 注意：这里不复制，因为只是临时使用
    update.new_type_def = new_type_def;
    update.new_ctx = new_ctx;
    
    return kos_ontology_batch_update(ontology, &update, 1, version_name, description);
}

// ========== 变更通知 API ==========

// 注册变更通知回调
int kos_ontology_register_change_callback(
    TypeOntology* ontology,
    kos_ontology_change_callback_t callback,
    void* user_data) {
    if (!ontology || !callback) {
        return -1;
    }
    
    size_t index = get_callback_index(ontology);
    
    callback_node_t* node = (callback_node_t*)malloc(sizeof(callback_node_t));
    if (!node) {
        return -1;
    }
    
    node->callback = callback;
    node->user_data = user_data;
    node->next = g_callbacks[index];
    g_callbacks[index] = node;
    
    return 0;
}

// 取消注册变更通知回调
int kos_ontology_unregister_change_callback(
    TypeOntology* ontology,
    kos_ontology_change_callback_t callback) {
    if (!ontology || !callback) {
        return -1;
    }
    
    size_t index = get_callback_index(ontology);
    callback_node_t* prev = NULL;
    callback_node_t* node = g_callbacks[index];
    
    while (node) {
        if (node->callback == callback) {
            if (prev) {
                prev->next = node->next;
            } else {
                g_callbacks[index] = node->next;
            }
            free(node);
            return 0;
        }
        prev = node;
        node = node->next;
    }
    
    return -1; // 未找到
}
