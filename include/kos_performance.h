// include/kos_performance.h
// Phase 7: Performance Optimization
// 性能优化模块：查询缓存、增强索引系统、查询优化器

#ifndef KOS_PERFORMANCE_H
#define KOS_PERFORMANCE_H

#include "kos_core.h"
#include "kos_query.h"
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <time.h>

// ========== 查询缓存 ==========

// 缓存替换策略
typedef enum {
    KOS_CACHE_LRU,  // Least Recently Used (最近最少使用)
    KOS_CACHE_LFU   // Least Frequently Used (最少使用频率)
} kos_cache_policy_t;

// 缓存失效策略
typedef enum {
    KOS_CACHE_INVALIDATE_TIME,      // 基于时间失效
    KOS_CACHE_INVALIDATE_CHANGE,    // 基于变更失效
    KOS_CACHE_INVALIDATE_BOTH       // 时间 + 变更
} kos_cache_invalidation_t;

// 缓存项
typedef struct kos_cache_entry {
    char* query_key;                // 查询键（查询字符串的哈希）
    kos_query_result_t* result;      // 缓存的结果
    time_t created_at;              // 创建时间
    time_t last_accessed;            // 最后访问时间
    size_t access_count;            // 访问次数（用于LFU）
    struct kos_cache_entry* next;    // 链表下一个节点
    struct kos_cache_entry* prev;    // 链表上一个节点
} kos_cache_entry_t;

// 查询缓存
typedef struct {
    kos_cache_entry_t** buckets;    // 哈希表桶数组
    size_t bucket_count;            // 桶数量
    size_t entry_count;             // 当前缓存项数量
    size_t max_size;                // 最大缓存项数量
    kos_cache_policy_t policy;      // 替换策略（LRU/LFU）
    kos_cache_invalidation_t invalidation; // 失效策略
    time_t default_ttl_seconds;     // 默认生存时间（秒）
    kos_cache_entry_t* lru_head;    // LRU链表头（用于LRU策略）
    kos_cache_entry_t* lru_tail;    // LRU链表尾
} kos_query_cache_t;

// ========== 增强索引系统 ==========

// 索引类型
typedef enum {
    KOS_INDEX_TIME_SERIES,      // 时间序列索引（已有）
    KOS_INDEX_GRAPH,             // 关系图索引（已有）
    KOS_INDEX_BPLUS_TREE,        // B+树索引（新增）
    KOS_INDEX_INVERTED,          // 倒排索引（新增）
    KOS_INDEX_HASH               // 哈希索引（新增）
} kos_index_type_t;

// 索引元数据
typedef struct {
    kos_index_type_t type;           // 索引类型
    const char* name;                // 索引名称
    const char* field_name;          // 索引字段名
    size_t entry_count;              // 索引项数量
    time_t created_at;              // 创建时间
    time_t last_updated;            // 最后更新时间
    bool is_auto_maintained;        // 是否自动维护
} kos_index_metadata_t;

// 索引管理器
typedef struct {
    kos_index_metadata_t* indices;  // 索引元数据数组
    size_t index_count;             // 索引数量
    size_t capacity;                // 容量
    void** index_handles;           // 索引句柄数组（指向实际的索引对象）
} kos_index_manager_t;

// ========== 查询优化器 ==========

// 查询计划节点类型
typedef enum {
    KOS_PLAN_SCAN,              // 全表扫描
    KOS_PLAN_INDEX_SCAN,        // 索引扫描
    KOS_PLAN_INDEX_SEEK,        // 索引查找
    KOS_PLAN_FILTER,            // 过滤
    KOS_PLAN_SORT,              // 排序
    KOS_PLAN_JOIN,              // 连接
    KOS_PLAN_AGGREGATE          // 聚合
} kos_plan_node_type_t;

// 查询计划节点
typedef struct kos_plan_node {
    kos_plan_node_type_t type;       // 节点类型
    const char* index_name;          // 使用的索引名称（如果是索引操作）
    double estimated_cost;          // 估计代价
    size_t estimated_rows;          // 估计行数
    struct kos_plan_node** children; // 子节点数组（指针数组）
    size_t child_count;             // 子节点数量
} kos_plan_node_t;

// 查询计划
typedef struct {
    kos_plan_node_t* root;           // 根节点
    double total_cost;               // 总代价
    size_t estimated_result_count;   // 估计结果数量
} kos_query_plan_t;

// ========== 查询缓存 API ==========

// 创建查询缓存
kos_query_cache_t* kos_query_cache_create(
    size_t max_size,
    kos_cache_policy_t policy,
    kos_cache_invalidation_t invalidation,
    time_t default_ttl_seconds
);

// 释放查询缓存
void kos_query_cache_free(kos_query_cache_t* cache);

// 从缓存获取查询结果
kos_query_result_t* kos_query_cache_get(
    kos_query_cache_t* cache,
    const char* query_key
);

// 将查询结果存入缓存
int kos_query_cache_put(
    kos_query_cache_t* cache,
    const char* query_key,
    kos_query_result_t* result
);

// 使缓存失效（基于查询键）
int kos_query_cache_invalidate(
    kos_query_cache_t* cache,
    const char* query_key
);

// 清空所有缓存
int kos_query_cache_clear(kos_query_cache_t* cache);

// 获取缓存统计信息
typedef struct {
    size_t total_entries;        // 总缓存项数
    size_t hit_count;            // 命中次数
    size_t miss_count;           // 未命中次数
    double hit_ratio;            // 命中率
    size_t evicted_count;        // 被驱逐的项数
} kos_cache_stats_t;

kos_cache_stats_t kos_query_cache_get_stats(kos_query_cache_t* cache);

// ========== 增强索引系统 API ==========

// 创建索引管理器
kos_index_manager_t* kos_index_manager_create(void);

// 释放索引管理器
void kos_index_manager_free(kos_index_manager_t* manager);

// 注册索引
int kos_index_manager_register(
    kos_index_manager_t* manager,
    kos_index_type_t type,
    const char* name,
    const char* field_name,
    void* index_handle
);

// 根据字段名查找可用索引
kos_index_metadata_t* kos_index_manager_find_by_field(
    kos_index_manager_t* manager,
    const char* field_name
);

// 根据索引名称查找索引
kos_index_metadata_t* kos_index_manager_find_by_name(
    kos_index_manager_t* manager,
    const char* index_name
);

// 获取所有索引
kos_index_metadata_t* kos_index_manager_get_all(
    kos_index_manager_t* manager,
    size_t* count
);

// 删除索引
int kos_index_manager_remove(
    kos_index_manager_t* manager,
    const char* index_name
);

// 更新索引统计信息
int kos_index_manager_update_stats(
    kos_index_manager_t* manager,
    const char* index_name,
    size_t entry_count
);

// ========== B+树索引 API ==========

// B+树索引（用于范围查询和排序）
typedef struct kos_bplus_tree_index kos_bplus_tree_index_t;

// 创建B+树索引
kos_bplus_tree_index_t* kos_bplus_tree_create(const char* field_name, size_t order);

// 插入数据到B+树
int kos_bplus_tree_insert(kos_bplus_tree_index_t* tree, kos_term* key, kos_term* value);

// 范围查询
kos_query_result_t* kos_bplus_tree_range_query(
    kos_bplus_tree_index_t* tree,
    kos_term* min_key,
    kos_term* max_key
);

// 释放B+树索引
void kos_bplus_tree_free(kos_bplus_tree_index_t* tree);

// ========== 倒排索引 API ==========

// 倒排索引（用于全文搜索和精确匹配）
typedef struct kos_inverted_index kos_inverted_index_t;

// 创建倒排索引
kos_inverted_index_t* kos_inverted_index_create(const char* field_name);

// 插入文档到倒排索引
int kos_inverted_index_insert(
    kos_inverted_index_t* index,
    kos_term* document_id,
    const char* text_content
);

// 搜索（返回匹配的文档ID）
kos_query_result_t* kos_inverted_index_search(
    kos_inverted_index_t* index,
    const char* query_term
);

// 释放倒排索引
void kos_inverted_index_free(kos_inverted_index_t* index);

// ========== 查询优化器 API ==========

// 创建查询计划
kos_query_plan_t* kos_query_optimizer_create_plan(
    kos_query_t* query,
    kos_index_manager_t* index_manager
);

// 释放查询计划
void kos_query_plan_free(kos_query_plan_t* plan);

// 执行查询计划
kos_query_result_t* kos_query_plan_execute(
    kos_query_plan_t* plan,
    const kos_term* knowledge_set,
    kos_index_manager_t* index_manager
);

// 打印查询计划（用于调试）
void kos_query_plan_print(kos_query_plan_t* plan);

// ========== 辅助函数 ==========

// 生成查询键（从查询对象生成唯一键）
char* kos_query_generate_key(kos_query_t* query);

// 计算字符串哈希值（用于缓存键）
uint64_t kos_hash_string(const char* str);

#endif // KOS_PERFORMANCE_H
