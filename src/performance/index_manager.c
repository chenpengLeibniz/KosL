// src/performance/index_manager.c
// Phase 7: Index Manager - Multi-level Index Management
// 索引管理器：管理多种类型的索引，支持自动索引选择

#include "../../include/kos_performance.h"
#include "../../include/kos_query.h"
#include <stdlib.h>
#include <string.h>
#include <time.h>

#define INITIAL_CAPACITY 16

// 创建索引管理器
kos_index_manager_t* kos_index_manager_create(void) {
    kos_index_manager_t* manager = (kos_index_manager_t*)calloc(1, sizeof(kos_index_manager_t));
    if (!manager) {
        return NULL;
    }
    
    manager->capacity = INITIAL_CAPACITY;
    manager->index_count = 0;
    manager->indices = (kos_index_metadata_t*)calloc(manager->capacity, sizeof(kos_index_metadata_t));
    manager->index_handles = (void**)calloc(manager->capacity, sizeof(void*));
    
    if (!manager->indices || !manager->index_handles) {
        if (manager->indices) free(manager->indices);
        if (manager->index_handles) free(manager->index_handles);
        free(manager);
        return NULL;
    }
    
    return manager;
}

// 释放索引管理器
void kos_index_manager_free(kos_index_manager_t* manager) {
    if (!manager) {
        return;
    }
    
    // 注意：索引句柄的实际释放由调用者负责
    // 这里只释放元数据
    
    if (manager->indices) {
        for (size_t i = 0; i < manager->index_count; i++) {
            if (manager->indices[i].name) {
                free((void*)manager->indices[i].name);
            }
            if (manager->indices[i].field_name) {
                free((void*)manager->indices[i].field_name);
            }
        }
        free(manager->indices);
    }
    
    if (manager->index_handles) {
        free(manager->index_handles);
    }
    
    free(manager);
}

// 扩展容量
static int expand_capacity(kos_index_manager_t* manager) {
    size_t new_capacity = manager->capacity * 2;
    
    kos_index_metadata_t* new_indices = (kos_index_metadata_t*)realloc(
        manager->indices, new_capacity * sizeof(kos_index_metadata_t));
    void** new_handles = (void**)realloc(
        manager->index_handles, new_capacity * sizeof(void*));
    
    if (!new_indices || !new_handles) {
        return -1;
    }
    
    // 初始化新分配的空间
    for (size_t i = manager->capacity; i < new_capacity; i++) {
        memset(&new_indices[i], 0, sizeof(kos_index_metadata_t));
        new_handles[i] = NULL;
    }
    
    manager->indices = new_indices;
    manager->index_handles = new_handles;
    manager->capacity = new_capacity;
    
    return 0;
}

// 注册索引
int kos_index_manager_register(
    kos_index_manager_t* manager,
    kos_index_type_t type,
    const char* name,
    const char* field_name,
    void* index_handle) {
    
    if (!manager || !name || !field_name || !index_handle) {
        return -1;
    }
    
    // 检查是否已存在同名索引
    for (size_t i = 0; i < manager->index_count; i++) {
        if (strcmp(manager->indices[i].name, name) == 0) {
            return -1; // 已存在
        }
    }
    
    // 扩展容量（如果需要）
    if (manager->index_count >= manager->capacity) {
        if (expand_capacity(manager) != 0) {
            return -1;
        }
    }
    
    // 添加新索引
    size_t idx = manager->index_count;
    manager->indices[idx].type = type;
    manager->indices[idx].name = strdup(name);
    manager->indices[idx].field_name = strdup(field_name);
    manager->indices[idx].entry_count = 0;
    manager->indices[idx].created_at = time(NULL);
    manager->indices[idx].last_updated = time(NULL);
    manager->indices[idx].is_auto_maintained = true;
    
    manager->index_handles[idx] = index_handle;
    manager->index_count++;
    
    return 0;
}

// 根据字段名查找可用索引
kos_index_metadata_t* kos_index_manager_find_by_field(
    kos_index_manager_t* manager,
    const char* field_name) {
    
    if (!manager || !field_name) {
        return NULL;
    }
    
    for (size_t i = 0; i < manager->index_count; i++) {
        if (strcmp(manager->indices[i].field_name, field_name) == 0) {
            return &manager->indices[i];
        }
    }
    
    return NULL;
}

// 根据索引名称查找索引
kos_index_metadata_t* kos_index_manager_find_by_name(
    kos_index_manager_t* manager,
    const char* index_name) {
    
    if (!manager || !index_name) {
        return NULL;
    }
    
    for (size_t i = 0; i < manager->index_count; i++) {
        if (strcmp(manager->indices[i].name, index_name) == 0) {
            return &manager->indices[i];
        }
    }
    
    return NULL;
}

// 获取所有索引
kos_index_metadata_t* kos_index_manager_get_all(
    kos_index_manager_t* manager,
    size_t* count) {
    
    if (!manager || !count) {
        return NULL;
    }
    
    *count = manager->index_count;
    return manager->indices;
}

// 删除索引
int kos_index_manager_remove(
    kos_index_manager_t* manager,
    const char* index_name) {
    
    if (!manager || !index_name) {
        return -1;
    }
    
    for (size_t i = 0; i < manager->index_count; i++) {
        if (strcmp(manager->indices[i].name, index_name) == 0) {
            // 释放字符串
            free((void*)manager->indices[i].name);
            free((void*)manager->indices[i].field_name);
            
            // 移动后续元素
            for (size_t j = i; j < manager->index_count - 1; j++) {
                manager->indices[j] = manager->indices[j + 1];
                manager->index_handles[j] = manager->index_handles[j + 1];
            }
            
            manager->index_count--;
            
            // 清空最后一个元素
            memset(&manager->indices[manager->index_count], 0, sizeof(kos_index_metadata_t));
            manager->index_handles[manager->index_count] = NULL;
            
            return 0;
        }
    }
    
    return -1; // 未找到
}

// 更新索引统计信息
int kos_index_manager_update_stats(
    kos_index_manager_t* manager,
    const char* index_name,
    size_t entry_count) {
    
    if (!manager || !index_name) {
        return -1;
    }
    
    kos_index_metadata_t* metadata = kos_index_manager_find_by_name(manager, index_name);
    if (!metadata) {
        return -1;
    }
    
    metadata->entry_count = entry_count;
    metadata->last_updated = time(NULL);
    
    return 0;
}

// 获取索引句柄（内部使用）
void* kos_index_manager_get_handle(kos_index_manager_t* manager, const char* index_name) {
    if (!manager || !index_name) {
        return NULL;
    }
    
    for (size_t i = 0; i < manager->index_count; i++) {
        if (strcmp(manager->indices[i].name, index_name) == 0) {
            return manager->index_handles[i];
        }
    }
    
    return NULL;
}
