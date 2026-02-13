// src/runtime/storage_manager.c
// Physical Storage Manager: 抽象底层媒体差异，管理数据库和内存映射
// 实现原子提交栅栏（Atomic Commit Fence）

#include "kos_runtime.h"
#include "kos_core.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

// ========== 内存存储后端（测试用）==========

typedef struct {
    kos_term** facts;          // 事实数组
    int* logical_clocks;        // 对应的逻辑时钟
    size_t capacity;
    size_t count;
    bool in_transaction;        // 是否在事务中
    size_t transaction_start;  // 事务开始位置
} memory_backend_t;

static int memory_write(storage_backend_t* backend, const kos_term* fact, int logical_clock) {
    if (!backend || !fact) {
        return -1;
    }
    
    memory_backend_t* mem = (memory_backend_t*)backend->backend_handle;
    if (!mem) {
        return -1;
    }
    
    // 扩展容量（如果需要）
    if (mem->count >= mem->capacity) {
        size_t new_capacity = mem->capacity == 0 ? 16 : mem->capacity * 2;
        kos_term** new_facts = (kos_term**)realloc(mem->facts, new_capacity * sizeof(kos_term*));
        int* new_clocks = (int*)realloc(mem->logical_clocks, new_capacity * sizeof(int));
        
        if (!new_facts || !new_clocks) {
            return -1;
        }
        
        mem->facts = new_facts;
        mem->logical_clocks = new_clocks;
        mem->capacity = new_capacity;
    }
    
    // 深拷贝事实
    mem->facts[mem->count] = kos_term_copy(fact);
    if (!mem->facts[mem->count]) {
        return -1;
    }
    
    mem->logical_clocks[mem->count] = logical_clock;
    mem->count++;
    
    return 0;
}

static int memory_read(storage_backend_t* backend, int logical_clock, kos_term** fact) {
    if (!backend || !fact) {
        return -1;
    }
    
    memory_backend_t* mem = (memory_backend_t*)backend->backend_handle;
    if (!mem) {
        return -1;
    }
    
    // 查找对应逻辑时钟的事实
    for (size_t i = 0; i < mem->count; i++) {
        if (mem->logical_clocks[i] == logical_clock) {
            *fact = kos_term_copy(mem->facts[i]);
            return 0;
        }
    }
    
    return -1;  // 未找到
}

static int memory_commit(storage_backend_t* backend) {
    if (!backend) {
        return -1;
    }
    
    memory_backend_t* mem = (memory_backend_t*)backend->backend_handle;
    if (!mem) {
        return -1;
    }
    
    // 内存后端：提交就是确认写入（简化实现）
    mem->in_transaction = false;
    return 0;
}

static int memory_rollback(storage_backend_t* backend) {
    if (!backend) {
        return -1;
    }
    
    memory_backend_t* mem = (memory_backend_t*)backend->backend_handle;
    if (!mem) {
        return -1;
    }
    
    // 回滚：删除事务开始后的所有写入
    if (mem->in_transaction && mem->transaction_start < mem->count) {
        for (size_t i = mem->transaction_start; i < mem->count; i++) {
            if (mem->facts[i]) {
                kos_term_free(mem->facts[i]);
            }
        }
        mem->count = mem->transaction_start;
    }
    
    mem->in_transaction = false;
    return 0;
}

static void memory_free(storage_backend_t* backend) {
    if (!backend) {
        return;
    }
    
    memory_backend_t* mem = (memory_backend_t*)backend->backend_handle;
    if (!mem) {
        return;
    }
    
    // 释放所有事实
    for (size_t i = 0; i < mem->count; i++) {
        if (mem->facts[i]) {
            kos_term_free(mem->facts[i]);
        }
    }
    
    if (mem->facts) {
        free(mem->facts);
    }
    if (mem->logical_clocks) {
        free(mem->logical_clocks);
    }
    
    free(mem);
    free(backend);
}

// ========== 存储后端创建 ==========

storage_backend_t* kos_storage_create(storage_backend_type type, const char* config) {
    (void)config;  // 配置参数暂时未使用
    
    storage_backend_t* backend = (storage_backend_t*)calloc(1, sizeof(storage_backend_t));
    if (!backend) {
        return NULL;
    }
    
    backend->type = type;
    
    switch (type) {
        case STORAGE_BACKEND_MEMORY: {
            memory_backend_t* mem = (memory_backend_t*)calloc(1, sizeof(memory_backend_t));
            if (!mem) {
                free(backend);
                return NULL;
            }
            
            mem->facts = NULL;
            mem->logical_clocks = NULL;
            mem->capacity = 0;
            mem->count = 0;
            mem->in_transaction = false;
            mem->transaction_start = 0;
            
            backend->backend_handle = mem;
            backend->write = memory_write;
            backend->read = memory_read;
            backend->commit = memory_commit;
            backend->rollback = memory_rollback;
            backend->free = memory_free;
            break;
        }
        
        case STORAGE_BACKEND_FILE:
        case STORAGE_BACKEND_DATABASE:
        case STORAGE_BACKEND_CUSTOM:
        default:
            // 其他后端类型待实现
            free(backend);
            return NULL;
    }
    
    return backend;
}

void kos_storage_free(storage_backend_t* backend) {
    if (!backend) {
        return;
    }
    
    if (backend->free) {
        backend->free(backend);
    } else {
        free(backend);
    }
}

// ========== M 算子：物化 ==========

int kos_materialize(kos_state_t* sigma, storage_backend_t* backend) {
    if (!sigma || !backend || !backend->write) {
        return -1;
    }
    
    // 原子提交栅栏：两阶段提交
    // Phase 1: Prepare - 准备写入
    // Phase 2: Commit - 确认写入
    
    // 从知识集 K 中提取最新事实（简化实现：假设 K 是最后一个事实）
    // 实际实现中，应该遍历 K 的 Σ 链结构
    
    kos_term* latest_fact = NULL;
    
    // 简化实现：如果 K 是 KOS_PAIR，提取 data 部分
    if (sigma->K && sigma->K->kind == KOS_PAIR) {
        latest_fact = sigma->K->data.pair.data;
    } else if (sigma->K) {
        latest_fact = sigma->K;
    }
    
    if (!latest_fact) {
        return -1;  // 没有可物化的事实
    }
    
    // Phase 1: Prepare - 写入到存储后端
    int write_result = backend->write(backend, latest_fact, sigma->TS);
    if (write_result != 0) {
        return -1;  // 写入失败
    }
    
    // Phase 2: Commit - 确认提交
    int commit_result = backend->commit(backend);
    if (commit_result != 0) {
        // 提交失败，回滚
        backend->rollback(backend);
        return -1;
    }
    
    printf("[M] Materialized fact at TS=%d\n", sigma->TS);
    return 0;
}

int kos_materialize_fact(storage_backend_t* backend, const kos_term* fact, int logical_clock) {
    if (!backend || !fact || !backend->write) {
        return -1;
    }
    
    // 两阶段提交：Prepare + Commit
    int write_result = backend->write(backend, fact, logical_clock);
    if (write_result != 0) {
        return -1;
    }
    
    int commit_result = backend->commit(backend);
    if (commit_result != 0) {
        backend->rollback(backend);
        return -1;
    }
    
    return 0;
}
