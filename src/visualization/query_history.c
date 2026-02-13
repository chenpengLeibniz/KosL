// src/visualization/query_history.c
// Phase 8: Query History Management
// 查询历史管理：记录、查询、导出查询历史

#include "../../include/kos_visualization_enhanced.h"
#include "../../include/kos_query.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <time.h>

#define INITIAL_CAPACITY 64
#define INITIAL_BUFFER_SIZE 4096

// 生成唯一查询ID
static char* generate_query_id(void) {
    static unsigned int counter = 0;
    char* id = (char*)malloc(32);
    if (id) {
        snprintf(id, 32, "query_%u_%lld", counter++, (long long)time(NULL));
    }
    return id;
}

// 创建查询历史管理器
kos_query_history_t* kos_query_history_create(size_t max_entries) {
    kos_query_history_t* history = (kos_query_history_t*)calloc(1, sizeof(kos_query_history_t));
    if (!history) {
        return NULL;
    }
    
    history->capacity = INITIAL_CAPACITY;
    history->max_entries = max_entries;
    history->count = 0;
    history->entries = (kos_query_history_entry_t*)calloc(history->capacity, sizeof(kos_query_history_entry_t));
    
    if (!history->entries) {
        free(history);
        return NULL;
    }
    
    return history;
}

// 释放查询历史管理器
void kos_query_history_free(kos_query_history_t* history) {
    if (!history) {
        return;
    }
    
    for (size_t i = 0; i < history->count; i++) {
        kos_query_history_entry_t* entry = &history->entries[i];
        
        if (entry->query_id) {
            free(entry->query_id);
        }
        
        if (entry->query_string) {
            free(entry->query_string);
        }
        
        if (entry->query_object) {
            kos_query_free(entry->query_object);
        }
        
        // 注意：result可能已被释放，这里不释放
    }
    
    if (history->entries) {
        free(history->entries);
    }
    
    free(history);
}

// 扩展容量
static int expand_capacity(kos_query_history_t* history) {
    size_t new_capacity = history->capacity * 2;
    kos_query_history_entry_t* new_entries = (kos_query_history_entry_t*)realloc(
        history->entries, new_capacity * sizeof(kos_query_history_entry_t));
    
    if (!new_entries) {
        return -1;
    }
    
    // 初始化新分配的空间
    for (size_t i = history->capacity; i < new_capacity; i++) {
        memset(&new_entries[i], 0, sizeof(kos_query_history_entry_t));
    }
    
    history->entries = new_entries;
    history->capacity = new_capacity;
    
    return 0;
}

// 添加查询历史记录
int kos_query_history_add(
    kos_query_history_t* history,
    const char* query_id,
    const char* query_string,
    kos_query_t* query_object,
    kos_query_result_t* result,
    double execution_time_ms,
    bool cached) {
    
    if (!history) {
        return -1;
    }
    
    // 检查是否超过最大记录数
    if (history->max_entries > 0 && history->count >= history->max_entries) {
        // 删除最旧的记录（FIFO）
        kos_query_history_entry_t* oldest = &history->entries[0];
        if (oldest->query_id) free(oldest->query_id);
        if (oldest->query_string) free(oldest->query_string);
        if (oldest->query_object) kos_query_free(oldest->query_object);
        
        // 移动后续记录
        memmove(&history->entries[0], &history->entries[1],
                (history->count - 1) * sizeof(kos_query_history_entry_t));
        history->count--;
    }
    
    // 扩展容量（如果需要）
    if (history->count >= history->capacity) {
        if (expand_capacity(history) != 0) {
            return -1;
        }
    }
    
    // 添加新记录
    kos_query_history_entry_t* entry = &history->entries[history->count];
    
    entry->query_id = query_id ? strdup(query_id) : generate_query_id();
    entry->query_string = query_string ? strdup(query_string) : NULL;
    // 注意：不复制query_object，避免内存管理复杂性
    entry->query_object = NULL;
    entry->executed_at = time(NULL);
    entry->result_count = result ? result->count : 0;
    entry->execution_time_ms = execution_time_ms;
    entry->cached = cached;
    entry->result = NULL; // 不保存结果，避免内存占用过大
    
    history->count++;
    
    return 0;
}

// 根据查询ID查找历史记录
kos_query_history_entry_t* kos_query_history_find_by_id(
    kos_query_history_t* history,
    const char* query_id) {
    
    if (!history || !query_id) {
        return NULL;
    }
    
    for (size_t i = 0; i < history->count; i++) {
        if (strcmp(history->entries[i].query_id, query_id) == 0) {
            return &history->entries[i];
        }
    }
    
    return NULL;
}

// 获取最近N条查询历史
kos_query_history_entry_t* kos_query_history_get_recent(
    kos_query_history_t* history,
    size_t count,
    size_t* actual_count) {
    
    if (!history || !actual_count) {
        return NULL;
    }
    
    size_t start_idx = (history->count > count) ? history->count - count : 0;
    *actual_count = history->count - start_idx;
    
    if (*actual_count == 0) {
        return NULL;
    }
    
    // 返回指向起始位置的指针
    return &history->entries[start_idx];
}

// 根据时间范围查询历史记录
kos_query_history_entry_t* kos_query_history_query_by_time_range(
    kos_query_history_t* history,
    time_t start_time,
    time_t end_time,
    size_t* count) {
    
    if (!history || !count) {
        return NULL;
    }
    
    // 分配结果数组
    kos_query_history_entry_t* results = (kos_query_history_entry_t*)malloc(
        history->count * sizeof(kos_query_history_entry_t));
    if (!results) {
        return NULL;
    }
    
    *count = 0;
    
    for (size_t i = 0; i < history->count; i++) {
        kos_query_history_entry_t* entry = &history->entries[i];
        if (entry->executed_at >= start_time && entry->executed_at <= end_time) {
            results[*count] = *entry;
            (*count)++;
        }
    }
    
    if (*count == 0) {
        free(results);
        return NULL;
    }
    
    return results;
}

// 删除查询历史记录
int kos_query_history_remove(kos_query_history_t* history, const char* query_id) {
    if (!history || !query_id) {
        return -1;
    }
    
    for (size_t i = 0; i < history->count; i++) {
        if (strcmp(history->entries[i].query_id, query_id) == 0) {
            // 释放资源
            kos_query_history_entry_t* entry = &history->entries[i];
            if (entry->query_id) free(entry->query_id);
            if (entry->query_string) free(entry->query_string);
            if (entry->query_object) kos_query_free(entry->query_object);
            
            // 移动后续记录
            if (i + 1 < history->count) {
                memmove(&history->entries[i], &history->entries[i + 1],
                        (history->count - i - 1) * sizeof(kos_query_history_entry_t));
            }
            
            history->count--;
            return 0;
        }
    }
    
    return -1; // 未找到
}

// 清空查询历史
int kos_query_history_clear(kos_query_history_t* history) {
    if (!history) {
        return -1;
    }
    
    for (size_t i = 0; i < history->count; i++) {
        kos_query_history_entry_t* entry = &history->entries[i];
        if (entry->query_id) free(entry->query_id);
        if (entry->query_string) free(entry->query_string);
        if (entry->query_object) kos_query_free(entry->query_object);
    }
    
    history->count = 0;
    
    return 0;
}

// 导出查询历史为JSON
char* kos_query_history_export_json(kos_query_history_t* history) {
    if (!history) {
        return NULL;
    }
    
    size_t capacity = INITIAL_BUFFER_SIZE * 2;
    size_t pos = 0;
    char* buffer = (char*)malloc(capacity);
    if (!buffer) return NULL;
    buffer[0] = '\0';
    
    // 开始JSON对象
    char* json = "{\"history\":[";
    size_t json_len = strlen(json);
    while (pos + json_len >= capacity) {
        capacity *= 2;
        buffer = (char*)realloc(buffer, capacity);
        if (!buffer) return NULL;
    }
    strcpy(buffer + pos, json);
    pos += json_len;
    
    // 添加每条记录
    for (size_t i = 0; i < history->count; i++) {
        if (i > 0) {
            char comma = ',';
            if (pos + 1 >= capacity) {
                capacity *= 2;
                buffer = (char*)realloc(buffer, capacity);
                if (!buffer) return NULL;
            }
            buffer[pos++] = comma;
            buffer[pos] = '\0';
        }
        
        kos_query_history_entry_t* entry = &history->entries[i];
        char entry_json[512];
        snprintf(entry_json, sizeof(entry_json),
                "{\"id\":\"%s\",\"query\":\"%s\",\"executed_at\":%lld,\"result_count\":%zu,\"execution_time_ms\":%.2f,\"cached\":%s}",
                entry->query_id ? entry->query_id : "",
                entry->query_string ? entry->query_string : "",
                (long long)entry->executed_at,
                entry->result_count,
                entry->execution_time_ms,
                entry->cached ? "true" : "false");
        
        size_t entry_len = strlen(entry_json);
        while (pos + entry_len >= capacity) {
            capacity *= 2;
            buffer = (char*)realloc(buffer, capacity);
            if (!buffer) return NULL;
        }
        strcpy(buffer + pos, entry_json);
        pos += entry_len;
    }
    
    // 结束JSON对象
    char* end_json = "]}";
    size_t end_len = strlen(end_json);
    while (pos + end_len >= capacity) {
        capacity *= 2;
        buffer = (char*)realloc(buffer, capacity);
        if (!buffer) return NULL;
    }
    strcpy(buffer + pos, end_json);
    pos += end_len;
    
    return buffer;
}

// 保存查询历史到文件
int kos_query_history_save(kos_query_history_t* history, const char* filename) {
    if (!history || !filename) {
        return -1;
    }
    
    char* json = kos_query_history_export_json(history);
    if (!json) {
        return -1;
    }
    
    FILE* fp = fopen(filename, "w");
    if (!fp) {
        free(json);
        return -1;
    }
    
    fprintf(fp, "%s", json);
    fclose(fp);
    free(json);
    
    return 0;
}

// 从文件加载查询历史（简化实现）
kos_query_history_t* kos_query_history_load(const char* filename) {
    // 简化实现：返回空的历史管理器
    // 完整实现需要JSON解析
    return kos_query_history_create(0);
}

// 查询历史转JSON（用于Web前端）
char* kos_query_history_to_json(kos_query_history_t* history) {
    return kos_query_history_export_json(history);
}

