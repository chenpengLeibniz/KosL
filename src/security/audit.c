#include "kos_security.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <time.h>

#ifdef _WIN32
#include <windows.h>
#else
#include <sys/time.h>
#endif

// ========== 审计日志系统结构 ==========
struct kos_audit_system {
    char* log_file_path;
    FILE* log_file;
    kos_audit_log_entry_t* entries; // 内存中的日志条目（可选）
    size_t entry_count;
    size_t entry_capacity;
};

// ========== 审计日志系统创建与销毁 ==========

kos_audit_system_t* kos_audit_create(const char* log_file_path) {
    kos_audit_system_t* audit = (kos_audit_system_t*)calloc(1, sizeof(kos_audit_system_t));
    if (!audit) {
        return NULL;
    }
    
    if (log_file_path) {
        audit->log_file_path = strdup(log_file_path);
        audit->log_file = fopen(log_file_path, "a"); // 追加模式
        if (!audit->log_file) {
            // 如果无法打开文件，仍然创建系统（使用内存日志）
            audit->log_file_path = NULL;
        }
    }
    
    audit->entry_capacity = 1024;
    audit->entries = (kos_audit_log_entry_t*)calloc(
        audit->entry_capacity,
        sizeof(kos_audit_log_entry_t)
    );
    
    return audit;
}

void kos_audit_free(kos_audit_system_t* audit) {
    if (!audit) {
        return;
    }
    
    if (audit->log_file) {
        fclose(audit->log_file);
    }
    
    if (audit->log_file_path) {
        free(audit->log_file_path);
    }
    
    // 释放内存中的日志条目
    if (audit->entries) {
        for (size_t i = 0; i < audit->entry_count; i++) {
            kos_audit_log_entry_t* entry = &audit->entries[i];
            if (entry->user_id) free((void*)entry->user_id);
            if (entry->resource) free((void*)entry->resource);
            if (entry->details) free((void*)entry->details);
            if (entry->ip_address) free((void*)entry->ip_address);
        }
        free(audit->entries);
    }
    
    free(audit);
}

// ========== 记录审计日志 ==========

int kos_audit_log(
    kos_audit_system_t* audit,
    const char* user_id,
    kos_audit_operation_t operation,
    const char* resource,
    const char* details,
    bool success,
    const char* ip_address
) {
    if (!audit) {
        return -1;
    }
    
    int64_t timestamp = kos_get_current_timestamp_ms();
    
    // 创建日志条目
    kos_audit_log_entry_t entry;
    entry.timestamp_ms = timestamp;
    entry.user_id = user_id ? strdup(user_id) : NULL;
    entry.operation = operation;
    entry.resource = resource ? strdup(resource) : NULL;
    entry.details = details ? strdup(details) : NULL;
    entry.success = success;
    entry.ip_address = ip_address ? strdup(ip_address) : NULL;
    
    // 写入文件（JSON格式）
    if (audit->log_file) {
        fprintf(audit->log_file,
            "{\"timestamp\":%lld,\"user_id\":\"%s\",\"operation\":\"%s\","
            "\"resource\":\"%s\",\"details\":%s,\"success\":%s,\"ip\":\"%s\"}\n",
            (long long)timestamp,
            user_id ? user_id : "",
            kos_audit_operation_to_string(operation),
            resource ? resource : "",
            details ? details : "{}",
            success ? "true" : "false",
            ip_address ? ip_address : ""
        );
        fflush(audit->log_file);
    }
    
    // 保存到内存（如果空间足够）
    if (audit->entry_count < audit->entry_capacity) {
        audit->entries[audit->entry_count] = entry;
        audit->entry_count++;
    } else {
        // 内存已满，释放最旧的条目
        if (audit->entry_count > 0) {
            kos_audit_log_entry_t* oldest = &audit->entries[0];
            if (oldest->user_id) free((void*)oldest->user_id);
            if (oldest->resource) free((void*)oldest->resource);
            if (oldest->details) free((void*)oldest->details);
            if (oldest->ip_address) free((void*)oldest->ip_address);
            
            // 移动所有条目
            for (size_t i = 0; i < audit->entry_count - 1; i++) {
                audit->entries[i] = audit->entries[i + 1];
            }
            audit->entries[audit->entry_count - 1] = entry;
        }
    }
    
    return 0;
}

// ========== 查询审计日志 ==========

kos_audit_log_entry_t* kos_audit_query(
    kos_audit_system_t* audit,
    const kos_audit_query_t* query,
    size_t* result_count
) {
    if (!audit || !query || !result_count) {
        return NULL;
    }
    
    // 收集匹配的条目
    kos_audit_log_entry_t* results = NULL;
    size_t result_capacity = 64;
    size_t count = 0;
    
    results = (kos_audit_log_entry_t*)calloc(
        result_capacity,
        sizeof(kos_audit_log_entry_t)
    );
    
    if (!results) {
        return NULL;
    }
    
    // 遍历内存中的日志条目
    for (size_t i = 0; i < audit->entry_count; i++) {
        kos_audit_log_entry_t* entry = &audit->entries[i];
        
        // 应用过滤器
        if (query->user_id && entry->user_id && strcmp(entry->user_id, query->user_id) != 0) {
            continue;
        }
        
        if (query->operation >= 0 && entry->operation != query->operation) {
            continue;
        }
        
        if (query->resource && entry->resource && strcmp(entry->resource, query->resource) != 0) {
            continue;
        }
        
        if (query->start_time_ms > 0 && entry->timestamp_ms < query->start_time_ms) {
            continue;
        }
        
        if (query->end_time_ms > 0 && entry->timestamp_ms > query->end_time_ms) {
            continue;
        }
        
        // 检查限制
        if (query->limit > 0 && count >= query->limit) {
            break;
        }
        
        // 扩展结果数组（如果需要）
        if (count >= result_capacity) {
            size_t new_capacity = result_capacity * 2;
            kos_audit_log_entry_t* new_results = (kos_audit_log_entry_t*)realloc(
                results,
                new_capacity * sizeof(kos_audit_log_entry_t)
            );
            if (!new_results) {
                // 释放已分配的结果
                for (size_t j = 0; j < count; j++) {
                    kos_audit_log_entry_t* e = &results[j];
                    if (e->user_id) free((void*)e->user_id);
                    if (e->resource) free((void*)e->resource);
                    if (e->details) free((void*)e->details);
                    if (e->ip_address) free((void*)e->ip_address);
                }
                free(results);
                return NULL;
            }
            results = new_results;
            result_capacity = new_capacity;
        }
        
        // 复制条目
        results[count] = *entry;
        if (entry->user_id) results[count].user_id = strdup(entry->user_id);
        if (entry->resource) results[count].resource = strdup(entry->resource);
        if (entry->details) results[count].details = strdup(entry->details);
        if (entry->ip_address) results[count].ip_address = strdup(entry->ip_address);
        count++;
    }
    
    *result_count = count;
    return results;
}

void kos_audit_log_entry_free(kos_audit_log_entry_t* entry) {
    if (!entry) {
        return;
    }
    
    if (entry->user_id) free((void*)entry->user_id);
    if (entry->resource) free((void*)entry->resource);
    if (entry->details) free((void*)entry->details);
    if (entry->ip_address) free((void*)entry->ip_address);
}

void kos_audit_log_entries_free(kos_audit_log_entry_t* entries, size_t count) {
    if (!entries) {
        return;
    }
    
    for (size_t i = 0; i < count; i++) {
        kos_audit_log_entry_free(&entries[i]);
    }
    
    free(entries);
}

// ========== 辅助函数 ==========

int64_t kos_get_current_timestamp_ms(void) {
#ifdef _WIN32
    FILETIME ft;
    GetSystemTimeAsFileTime(&ft);
    ULARGE_INTEGER uli;
    uli.LowPart = ft.dwLowDateTime;
    uli.HighPart = ft.dwHighDateTime;
    return (int64_t)((uli.QuadPart / 10000) - 11644473600000LL);
#else
    struct timespec ts;
    if (clock_gettime(CLOCK_REALTIME, &ts) == 0) {
        return (int64_t)ts.tv_sec * 1000 + (int64_t)ts.tv_nsec / 1000000;
    }
    return 0;
#endif
}

const char* kos_audit_operation_to_string(kos_audit_operation_t op) {
    switch (op) {
        case KOS_AUDIT_OP_CREATE: return "CREATE";
        case KOS_AUDIT_OP_READ: return "READ";
        case KOS_AUDIT_OP_UPDATE: return "UPDATE";
        case KOS_AUDIT_OP_DELETE: return "DELETE";
        case KOS_AUDIT_OP_QUERY: return "QUERY";
        case KOS_AUDIT_OP_LOGIN: return "LOGIN";
        case KOS_AUDIT_OP_LOGOUT: return "LOGOUT";
        case KOS_AUDIT_OP_PERMISSION_DENIED: return "PERMISSION_DENIED";
        default: return "UNKNOWN";
    }
}

kos_audit_operation_t kos_audit_operation_from_string(const char* str) {
    if (!str) return KOS_AUDIT_OP_CREATE;
    
    if (strcmp(str, "CREATE") == 0) return KOS_AUDIT_OP_CREATE;
    if (strcmp(str, "READ") == 0) return KOS_AUDIT_OP_READ;
    if (strcmp(str, "UPDATE") == 0) return KOS_AUDIT_OP_UPDATE;
    if (strcmp(str, "DELETE") == 0) return KOS_AUDIT_OP_DELETE;
    if (strcmp(str, "QUERY") == 0) return KOS_AUDIT_OP_QUERY;
    if (strcmp(str, "LOGIN") == 0) return KOS_AUDIT_OP_LOGIN;
    if (strcmp(str, "LOGOUT") == 0) return KOS_AUDIT_OP_LOGOUT;
    if (strcmp(str, "PERMISSION_DENIED") == 0) return KOS_AUDIT_OP_PERMISSION_DENIED;
    
    return KOS_AUDIT_OP_CREATE;
}

const char* kos_permission_type_to_string(kos_permission_type_t type) {
    switch (type) {
        case KOS_PERM_READ: return "READ";
        case KOS_PERM_WRITE: return "WRITE";
        case KOS_PERM_DELETE: return "DELETE";
        case KOS_PERM_QUERY: return "QUERY";
        case KOS_PERM_ADMIN: return "ADMIN";
        default: return "UNKNOWN";
    }
}

kos_permission_type_t kos_permission_type_from_string(const char* str) {
    if (!str) return KOS_PERM_READ;
    
    if (strcmp(str, "READ") == 0) return KOS_PERM_READ;
    if (strcmp(str, "WRITE") == 0) return KOS_PERM_WRITE;
    if (strcmp(str, "DELETE") == 0) return KOS_PERM_DELETE;
    if (strcmp(str, "QUERY") == 0) return KOS_PERM_QUERY;
    if (strcmp(str, "ADMIN") == 0) return KOS_PERM_ADMIN;
    
    return KOS_PERM_READ;
}
