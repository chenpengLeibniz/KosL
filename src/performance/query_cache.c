// src/performance/query_cache.c
// Phase 7: Query Caching System (LRU/LFU)
// 查询缓存系统：支持LRU和LFU替换策略，以及基于时间和变更的失效策略

#include "../../include/kos_performance.h"
#include "../../include/kos_query.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <time.h>

// 默认哈希表桶数量
#define DEFAULT_BUCKET_COUNT 1024

// 计算哈希值（用于确定桶位置）
static size_t hash_query_key(const char* key, size_t bucket_count) {
    uint64_t hash = kos_hash_string(key);
    return hash % bucket_count;
}

// 创建缓存项
static kos_cache_entry_t* cache_entry_create(const char* query_key, kos_query_result_t* result) {
    kos_cache_entry_t* entry = (kos_cache_entry_t*)calloc(1, sizeof(kos_cache_entry_t));
    if (!entry) {
        return NULL;
    }
    
    entry->query_key = strdup(query_key);
    entry->result = result;
    entry->created_at = time(NULL);
    entry->last_accessed = time(NULL);
    entry->access_count = 1;
    
    return entry;
}

// 释放缓存项
static void cache_entry_free(kos_cache_entry_t* entry) {
    if (!entry) {
        return;
    }
    
    if (entry->query_key) {
        free(entry->query_key);
    }
    
    if (entry->result) {
        kos_query_result_free(entry->result);
    }
    
    free(entry);
}

// 从LRU链表中移除节点
static void lru_remove(kos_query_cache_t* cache, kos_cache_entry_t* entry) {
    if (!entry || !cache) {
        return;
    }
    
    if (entry->prev) {
        entry->prev->next = entry->next;
    } else {
        cache->lru_head = entry->next;
    }
    
    if (entry->next) {
        entry->next->prev = entry->prev;
    } else {
        cache->lru_tail = entry->prev;
    }
    
    entry->prev = NULL;
    entry->next = NULL;
}

// 添加到LRU链表头部
static void lru_add_to_head(kos_query_cache_t* cache, kos_cache_entry_t* entry) {
    if (!entry || !cache) {
        return;
    }
    
    entry->next = cache->lru_head;
    entry->prev = NULL;
    
    if (cache->lru_head) {
        cache->lru_head->prev = entry;
    } else {
        cache->lru_tail = entry;
    }
    
    cache->lru_head = entry;
}

// 移动到LRU链表头部（访问时调用）
static void lru_move_to_head(kos_query_cache_t* cache, kos_cache_entry_t* entry) {
    lru_remove(cache, entry);
    lru_add_to_head(cache, entry);
}

// 移除LRU链表尾部节点（用于LRU驱逐）
static kos_cache_entry_t* lru_remove_tail(kos_query_cache_t* cache) {
    if (!cache || !cache->lru_tail) {
        return NULL;
    }
    
    kos_cache_entry_t* tail = cache->lru_tail;
    lru_remove(cache, tail);
    return tail;
}

// 查找LFU最少访问的项（用于LFU驱逐）
static kos_cache_entry_t* find_lfu_entry(kos_query_cache_t* cache) {
    if (!cache || cache->entry_count == 0) {
        return NULL;
    }
    
    kos_cache_entry_t* lfu_entry = NULL;
    size_t min_access = SIZE_MAX;
    
    // 遍历所有桶查找最少访问的项
    for (size_t i = 0; i < cache->bucket_count; i++) {
        kos_cache_entry_t* entry = cache->buckets[i];
        while (entry) {
            if (entry->access_count < min_access) {
                min_access = entry->access_count;
                lfu_entry = entry;
            }
            entry = entry->next;
        }
    }
    
    return lfu_entry;
}

// 从哈希表中移除缓存项
static void remove_from_bucket(kos_query_cache_t* cache, kos_cache_entry_t* entry) {
    if (!cache || !entry) {
        return;
    }
    
    size_t bucket_idx = hash_query_key(entry->query_key, cache->bucket_count);
    kos_cache_entry_t* curr = cache->buckets[bucket_idx];
    kos_cache_entry_t* prev = NULL;
    
    while (curr) {
        if (curr == entry) {
            if (prev) {
                prev->next = curr->next;
            } else {
                cache->buckets[bucket_idx] = curr->next;
            }
            break;
        }
        prev = curr;
        curr = curr->next;
    }
}

// 驱逐一个缓存项（根据策略）
static void evict_entry(kos_query_cache_t* cache) {
    if (!cache || cache->entry_count == 0) {
        return;
    }
    
    kos_cache_entry_t* to_evict = NULL;
    
    if (cache->policy == KOS_CACHE_LRU) {
        to_evict = lru_remove_tail(cache);
    } else if (cache->policy == KOS_CACHE_LFU) {
        to_evict = find_lfu_entry(cache);
        if (to_evict) {
            lru_remove(cache, to_evict);
        }
    }
    
    if (to_evict) {
        remove_from_bucket(cache, to_evict);
        cache_entry_free(to_evict);
        cache->entry_count--;
    }
}

// 检查缓存项是否过期
static bool is_entry_expired(kos_query_cache_t* cache, kos_cache_entry_t* entry) {
    if (!cache || !entry) {
        return true;
    }
    
    if (cache->invalidation == KOS_CACHE_INVALIDATE_TIME ||
        cache->invalidation == KOS_CACHE_INVALIDATE_BOTH) {
        time_t now = time(NULL);
        time_t age = now - entry->created_at;
        if (age > cache->default_ttl_seconds) {
            return true;
        }
    }
    
    return false;
}

// 创建查询缓存
kos_query_cache_t* kos_query_cache_create(
    size_t max_size,
    kos_cache_policy_t policy,
    kos_cache_invalidation_t invalidation,
    time_t default_ttl_seconds) {
    
    kos_query_cache_t* cache = (kos_query_cache_t*)calloc(1, sizeof(kos_query_cache_t));
    if (!cache) {
        return NULL;
    }
    
    cache->bucket_count = DEFAULT_BUCKET_COUNT;
    cache->buckets = (kos_cache_entry_t**)calloc(cache->bucket_count, sizeof(kos_cache_entry_t*));
    if (!cache->buckets) {
        free(cache);
        return NULL;
    }
    
    cache->max_size = max_size;
    cache->policy = policy;
    cache->invalidation = invalidation;
    cache->default_ttl_seconds = default_ttl_seconds;
    cache->entry_count = 0;
    cache->lru_head = NULL;
    cache->lru_tail = NULL;
    
    return cache;
}

// 释放查询缓存
void kos_query_cache_free(kos_query_cache_t* cache) {
    if (!cache) {
        return;
    }
    
    // 清空所有缓存项
    kos_query_cache_clear(cache);
    
    if (cache->buckets) {
        free(cache->buckets);
    }
    
    free(cache);
}

// 从缓存获取查询结果
kos_query_result_t* kos_query_cache_get(
    kos_query_cache_t* cache,
    const char* query_key) {
    
    if (!cache || !query_key) {
        return NULL;
    }
    
    size_t bucket_idx = hash_query_key(query_key, cache->bucket_count);
    kos_cache_entry_t* entry = cache->buckets[bucket_idx];
    
    // 在链表中查找
    while (entry) {
        if (strcmp(entry->query_key, query_key) == 0) {
            // 检查是否过期
            if (is_entry_expired(cache, entry)) {
                // 过期，移除
                remove_from_bucket(cache, entry);
                lru_remove(cache, entry);
                cache_entry_free(entry);
                cache->entry_count--;
                return NULL;
            }
            
            // 更新访问信息
            entry->last_accessed = time(NULL);
            entry->access_count++;
            
            // 更新LRU链表
            if (cache->policy == KOS_CACHE_LRU) {
                lru_move_to_head(cache, entry);
            }
            
            // 返回结果的副本（避免外部修改影响缓存）
            kos_query_result_t* result_copy = (kos_query_result_t*)calloc(1, sizeof(kos_query_result_t));
            if (result_copy) {
                result_copy->count = entry->result->count;
                result_copy->capacity = entry->result->capacity;
                result_copy->results = (kos_term**)calloc(entry->result->count, sizeof(kos_term*));
                if (result_copy->results) {
                    for (size_t i = 0; i < entry->result->count; i++) {
                        result_copy->results[i] = kos_term_copy(entry->result->results[i]);
                    }
                }
            }
            
            return result_copy;
        }
        entry = entry->next;
    }
    
    return NULL;
}

// 将查询结果存入缓存
int kos_query_cache_put(
    kos_query_cache_t* cache,
    const char* query_key,
    kos_query_result_t* result) {
    
    if (!cache || !query_key || !result) {
        return -1;
    }
    
    // 如果缓存已满，驱逐一个项
    if (cache->entry_count >= cache->max_size) {
        evict_entry(cache);
    }
    
    // 检查是否已存在
    size_t bucket_idx = hash_query_key(query_key, cache->bucket_count);
    kos_cache_entry_t* entry = cache->buckets[bucket_idx];
    
    while (entry) {
        if (strcmp(entry->query_key, query_key) == 0) {
            // 更新现有项
            kos_query_result_free(entry->result);
            entry->result = result;
            entry->created_at = time(NULL);
            entry->last_accessed = time(NULL);
            entry->access_count = 1;
            
            if (cache->policy == KOS_CACHE_LRU) {
                lru_move_to_head(cache, entry);
            }
            
            return 0;
        }
        entry = entry->next;
    }
    
    // 创建新项
    kos_cache_entry_t* new_entry = cache_entry_create(query_key, result);
    if (!new_entry) {
        return -1;
    }
    
    // 添加到哈希表
    new_entry->next = cache->buckets[bucket_idx];
    cache->buckets[bucket_idx] = new_entry;
    
    // 添加到LRU链表
    if (cache->policy == KOS_CACHE_LRU) {
        lru_add_to_head(cache, new_entry);
    }
    
    cache->entry_count++;
    
    return 0;
}

// 使缓存失效
int kos_query_cache_invalidate(
    kos_query_cache_t* cache,
    const char* query_key) {
    
    if (!cache || !query_key) {
        return -1;
    }
    
    size_t bucket_idx = hash_query_key(query_key, cache->bucket_count);
    kos_cache_entry_t* entry = cache->buckets[bucket_idx];
    kos_cache_entry_t* prev = NULL;
    
    while (entry) {
        if (strcmp(entry->query_key, query_key) == 0) {
            // 从哈希表移除
            if (prev) {
                prev->next = entry->next;
            } else {
                cache->buckets[bucket_idx] = entry->next;
            }
            
            // 从LRU链表移除
            lru_remove(cache, entry);
            
            // 释放
            cache_entry_free(entry);
            cache->entry_count--;
            
            return 0;
        }
        prev = entry;
        entry = entry->next;
    }
    
    return -1; // 未找到
}

// 清空所有缓存
int kos_query_cache_clear(kos_query_cache_t* cache) {
    if (!cache) {
        return -1;
    }
    
    for (size_t i = 0; i < cache->bucket_count; i++) {
        kos_cache_entry_t* entry = cache->buckets[i];
        while (entry) {
            kos_cache_entry_t* next = entry->next;
            cache_entry_free(entry);
            entry = next;
        }
        cache->buckets[i] = NULL;
    }
    
    cache->entry_count = 0;
    cache->lru_head = NULL;
    cache->lru_tail = NULL;
    
    return 0;
}

// 获取缓存统计信息
kos_cache_stats_t kos_query_cache_get_stats(kos_query_cache_t* cache) {
    kos_cache_stats_t stats = {0};
    
    if (!cache) {
        return stats;
    }
    
    stats.total_entries = cache->entry_count;
    
    // 注意：hit_count和miss_count需要在调用处统计
    // 这里只返回基本统计信息
    
    return stats;
}

// 计算字符串哈希值
uint64_t kos_hash_string(const char* str) {
    if (!str) {
        return 0;
    }
    
    // FNV-1a哈希算法
    uint64_t hash = 14695981039346656037ULL; // FNV offset basis
    const char* p = str;
    
    while (*p) {
        hash ^= (uint64_t)(unsigned char)(*p);
        hash *= 1099511628211ULL; // FNV prime
        p++;
    }
    
    return hash;
}
