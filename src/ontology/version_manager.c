// src/ontology/version_manager.c
// KOS Dynamic Ontology Management - Version Manager
// 本体版本管理系统（Git-like 版本控制）

#include "../../include/kos_ontology_version.h"
#include "../../include/kos_ontology.h"
#include "../../include/kos_core.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <time.h>

// ========== 版本存储结构 ==========

// 版本管理器（附加到 TypeOntology）
typedef struct {
    kos_ontology_version_t* versions;      // 版本列表（链表）
    kos_ontology_version_t* current_version; // 当前版本
    size_t version_count;                   // 版本数量
} version_manager_t;

// 获取版本管理器（从本体中）
static version_manager_t* get_version_manager(TypeOntology* ontology) {
    // 简化实现：使用本体的扩展字段存储版本管理器
    // 实际实现中应该在本体中添加版本管理器字段
    // 这里使用静态全局映射（简化实现）
    static version_manager_t* managers[256] = {0};
    static size_t manager_count = 0;
    
    // 简化实现：使用 domain_name 的哈希作为索引
    size_t index = 0;
    if (ontology && ontology->domain_name) {
        for (const char* p = ontology->domain_name; *p; p++) {
            index = (index * 31 + *p) % 256;
        }
    }
    
    if (!managers[index]) {
        managers[index] = (version_manager_t*)calloc(1, sizeof(version_manager_t));
    }
    
    return managers[index];
}

// ========== 辅助函数 ==========

// 生成提交哈希（简化实现：基于时间戳和内容）
static char* generate_commit_hash(TypeOntology* ontology) {
    char hash[33] = {0};
    
    // 简化实现：使用时间戳和类型数量生成哈希
    time_t now = time(NULL);
    snprintf(hash, sizeof(hash), "%08lx%08lx", 
             (unsigned long)now, 
             (unsigned long)(ontology ? ontology->type_count : 0));
    
    return strdup(hash);
}

// 获取当前时间戳字符串
static char* get_current_timestamp(void) {
    time_t now = time(NULL);
    char* timestamp = (char*)malloc(32);
    if (timestamp) {
        strftime(timestamp, 32, "%Y-%m-%d %H:%M:%S", localtime(&now));
    }
    return timestamp;
}

// 深拷贝本体（创建快照）
static TypeOntology* ontology_snapshot(TypeOntology* ontology) {
    if (!ontology) {
        return NULL;
    }
    
    TypeOntology* snapshot = kos_ontology_create(ontology->domain_name);
    if (!snapshot) {
        return NULL;
    }
    
    // 复制所有类型定义
    for (size_t i = 0; i < ontology->type_count; i++) {
        TypeDefinition* def = &ontology->type_definitions[i];
        kos_ontology_add_type_definition(
            snapshot,
            def->name,
            def->type_def ? kos_term_copy(def->type_def) : NULL,
            def->ctx ? kos_term_copy(def->ctx) : NULL
        );
    }
    
    return snapshot;
}

// ========== 版本管理 API ==========

// 创建本体版本（创建快照）
kos_ontology_version_t* kos_ontology_create_version(
    TypeOntology* ontology,
    const char* version_name,
    const char* description) {
    if (!ontology || !version_name) {
        return NULL;
    }
    
    // 创建版本对象
    kos_ontology_version_t* version = (kos_ontology_version_t*)calloc(1, sizeof(kos_ontology_version_t));
    if (!version) {
        return NULL;
    }
    
    version->version_name = strdup(version_name);
    version->description = description ? strdup(description) : NULL;
    version->commit_hash = generate_commit_hash(ontology);
    version->timestamp = get_current_timestamp();
    version->snapshot = ontology_snapshot(ontology);
    
    if (!version->snapshot) {
        free(version->version_name);
        free(version->description);
        free(version->commit_hash);
        free(version->timestamp);
        free(version);
        return NULL;
    }
    
    // 添加到版本管理器
    version_manager_t* vm = get_version_manager(ontology);
    if (vm) {
        version->parent = vm->current_version;
        vm->current_version = version;
        vm->version_count++;
        
        // 添加到版本列表（链表）
        // 简化实现：只维护当前版本
    }
    
    return version;
}

// 获取当前版本
kos_ontology_version_t* kos_ontology_get_current_version(TypeOntology* ontology) {
    if (!ontology) {
        return NULL;
    }
    
    version_manager_t* vm = get_version_manager(ontology);
    return vm ? vm->current_version : NULL;
}

// 查找指定版本
kos_ontology_version_t* kos_ontology_find_version(
    TypeOntology* ontology,
    const char* version_name) {
    if (!ontology || !version_name) {
        return NULL;
    }
    
    version_manager_t* vm = get_version_manager(ontology);
    if (!vm) {
        return NULL;
    }
    
    // 遍历版本链查找
    kos_ontology_version_t* version = vm->current_version;
    while (version) {
        if (version->version_name && strcmp(version->version_name, version_name) == 0) {
            return version;
        }
        version = version->parent;
    }
    
    return NULL;
}

// 回滚到指定版本
int kos_ontology_rollback(
    TypeOntology* ontology,
    const char* version_name) {
    if (!ontology || !version_name) {
        return -1;
    }
    
    // 查找版本
    kos_ontology_version_t* target_version = kos_ontology_find_version(ontology, version_name);
    if (!target_version || !target_version->snapshot) {
        return -1;
    }
    
    // 释放当前本体
    kos_ontology_free(ontology);
    
    // 恢复快照
    TypeOntology* snapshot = target_version->snapshot;
    
    // 复制快照内容到本体
    // 注意：这里简化实现，实际应该深拷贝
    // 由于 TypeOntology 结构，我们需要重新构建
    ontology->type_count = snapshot->type_count;
    ontology->capacity = snapshot->capacity;
    
    // 重新分配类型定义数组
    if (snapshot->type_count > 0) {
        ontology->type_definitions = (TypeDefinition*)malloc(
            snapshot->type_count * sizeof(TypeDefinition)
        );
        if (ontology->type_definitions) {
            for (size_t i = 0; i < snapshot->type_count; i++) {
                TypeDefinition* src_def = &snapshot->type_definitions[i];
                TypeDefinition* dst_def = &ontology->type_definitions[i];
                
                dst_def->name = src_def->name ? strdup(src_def->name) : NULL;
                dst_def->type_def = src_def->type_def ? kos_term_copy(src_def->type_def) : NULL;
                dst_def->ctx = src_def->ctx ? kos_term_copy(src_def->ctx) : NULL;
            }
        }
    }
    
    // 更新版本管理器
    version_manager_t* vm = get_version_manager(ontology);
    if (vm) {
        vm->current_version = target_version;
    }
    
    return 0;
}

// 释放版本对象
void kos_ontology_version_free(kos_ontology_version_t* version) {
    if (!version) {
        return;
    }
    
    if (version->version_name) {
        free(version->version_name);
    }
    
    if (version->description) {
        free(version->description);
    }
    
    if (version->commit_hash) {
        free(version->commit_hash);
    }
    
    if (version->timestamp) {
        free(version->timestamp);
    }
    
    if (version->snapshot) {
        kos_ontology_free(version->snapshot);
    }
    
    // 注意：不释放 parent，因为它可能被其他版本引用
    
    free(version);
}

// ========== 版本比较和差异 ==========

// 比较两个版本
kos_ontology_diff_t* kos_ontology_diff(
    TypeOntology* ontology,
    const char* version1_name,
    const char* version2_name) {
    if (!ontology || !version1_name || !version2_name) {
        return NULL;
    }
    
    kos_ontology_version_t* v1 = kos_ontology_find_version(ontology, version1_name);
    kos_ontology_version_t* v2 = kos_ontology_find_version(ontology, version2_name);
    
    if (!v1 || !v2 || !v1->snapshot || !v2->snapshot) {
        return NULL;
    }
    
    TypeOntology* snap1 = v1->snapshot;
    TypeOntology* snap2 = v2->snapshot;
    
    kos_ontology_diff_t* diff = (kos_ontology_diff_t*)calloc(1, sizeof(kos_ontology_diff_t));
    if (!diff) {
        return NULL;
    }
    
    diff->capacity = 16;
    diff->items = (kos_ontology_diff_item_t*)malloc(
        diff->capacity * sizeof(kos_ontology_diff_item_t)
    );
    if (!diff->items) {
        free(diff);
        return NULL;
    }
    
    diff->count = 0;
    
    // 比较类型定义
    // 简化实现：检查每个类型是否存在或已修改
    
    // 检查 snap1 中的类型
    for (size_t i = 0; i < snap1->type_count; i++) {
        TypeDefinition* def1 = &snap1->type_definitions[i];
        TypeDefinition* def2 = NULL;
        
        // 在 snap2 中查找同名类型
        for (size_t j = 0; j < snap2->type_count; j++) {
            if (snap2->type_definitions[j].name &&
                strcmp(snap2->type_definitions[j].name, def1->name) == 0) {
                def2 = &snap2->type_definitions[j];
                break;
            }
        }
        
        if (!def2) {
            // 类型在 v1 中存在但在 v2 中不存在（被删除）
            if (diff->count >= diff->capacity) {
                diff->capacity *= 2;
                diff->items = (kos_ontology_diff_item_t*)realloc(
                    diff->items,
                    diff->capacity * sizeof(kos_ontology_diff_item_t)
                );
            }
            
            kos_ontology_diff_item_t* item = &diff->items[diff->count++];
            item->type_name = strdup(def1->name);
            item->change_type = DIFF_REMOVED;
            item->old_type_def = def1->type_def ? kos_term_copy(def1->type_def) : NULL;
            item->old_ctx = def1->ctx ? kos_term_copy(def1->ctx) : NULL;
            item->new_type_def = NULL;
            item->new_ctx = NULL;
        } else {
            // 检查类型是否被修改（简化实现：比较指针）
            // 实际应该深度比较类型定义
            if (def1->type_def != def2->type_def) {
                if (diff->count >= diff->capacity) {
                    diff->capacity *= 2;
                    diff->items = (kos_ontology_diff_item_t*)realloc(
                        diff->items,
                        diff->capacity * sizeof(kos_ontology_diff_item_t)
                    );
                }
                
                kos_ontology_diff_item_t* item = &diff->items[diff->count++];
                item->type_name = strdup(def1->name);
                item->change_type = DIFF_MODIFIED;
                item->old_type_def = def1->type_def ? kos_term_copy(def1->type_def) : NULL;
                item->old_ctx = def1->ctx ? kos_term_copy(def1->ctx) : NULL;
                item->new_type_def = def2->type_def ? kos_term_copy(def2->type_def) : NULL;
                item->new_ctx = def2->ctx ? kos_term_copy(def2->ctx) : NULL;
            }
        }
    }
    
    // 检查 snap2 中的新类型
    for (size_t i = 0; i < snap2->type_count; i++) {
        TypeDefinition* def2 = &snap2->type_definitions[i];
        bool found = false;
        
        for (size_t j = 0; j < snap1->type_count; j++) {
            if (snap1->type_definitions[j].name &&
                strcmp(snap1->type_definitions[j].name, def2->name) == 0) {
                found = true;
                break;
            }
        }
        
        if (!found) {
            // 类型在 v2 中存在但在 v1 中不存在（新增）
            if (diff->count >= diff->capacity) {
                diff->capacity *= 2;
                diff->items = (kos_ontology_diff_item_t*)realloc(
                    diff->items,
                    diff->capacity * sizeof(kos_ontology_diff_item_t)
                );
            }
            
            kos_ontology_diff_item_t* item = &diff->items[diff->count++];
            item->type_name = strdup(def2->name);
            item->change_type = DIFF_ADDED;
            item->old_type_def = NULL;
            item->old_ctx = NULL;
            item->new_type_def = def2->type_def ? kos_term_copy(def2->type_def) : NULL;
            item->new_ctx = def2->ctx ? kos_term_copy(def2->ctx) : NULL;
        }
    }
    
    return diff;
}

// 比较当前版本与指定版本
kos_ontology_diff_t* kos_ontology_diff_current(
    TypeOntology* ontology,
    const char* version_name) {
    if (!ontology || !version_name) {
        return NULL;
    }
    
    // 创建当前版本的临时快照
    kos_ontology_version_t* current = kos_ontology_get_current_version(ontology);
    if (!current) {
        // 如果没有当前版本，创建一个临时版本用于比较
        current = kos_ontology_create_version(ontology, "__current__", "Current state");
    }
    
    return kos_ontology_diff(ontology, version_name, current->version_name);
}

// 释放差异对象
void kos_ontology_diff_free(kos_ontology_diff_t* diff) {
    if (!diff) {
        return;
    }
    
    if (diff->items) {
        for (size_t i = 0; i < diff->count; i++) {
            kos_ontology_diff_item_t* item = &diff->items[i];
            if (item->type_name) {
                free(item->type_name);
            }
            if (item->old_type_def) {
                kos_term_free(item->old_type_def);
            }
            if (item->new_type_def) {
                kos_term_free(item->new_type_def);
            }
            if (item->old_ctx) {
                kos_term_free(item->old_ctx);
            }
            if (item->new_ctx) {
                kos_term_free(item->new_ctx);
            }
        }
        free(diff->items);
    }
    
    free(diff);
}

// ========== 列出所有版本（简化实现） ==========

// 列出所有版本
kos_ontology_version_t** kos_ontology_list_versions(
    TypeOntology* ontology,
    size_t* count) {
    if (!ontology || !count) {
        return NULL;
    }
    
    version_manager_t* vm = get_version_manager(ontology);
    if (!vm || !vm->current_version) {
        *count = 0;
        return NULL;
    }
    
    // 计算版本数量
    size_t version_count = 0;
    kos_ontology_version_t* v = vm->current_version;
    while (v) {
        version_count++;
        v = v->parent;
    }
    
    // 分配数组
    kos_ontology_version_t** versions = (kos_ontology_version_t**)malloc(
        version_count * sizeof(kos_ontology_version_t*)
    );
    if (!versions) {
        *count = 0;
        return NULL;
    }
    
    // 填充数组
    v = vm->current_version;
    for (size_t i = 0; i < version_count; i++) {
        versions[i] = v;
        v = v->parent;
    }
    
    *count = version_count;
    return versions;
}
