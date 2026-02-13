// examples/performance_demo.c
// Phase 7: Performance Optimization Demo
// 演示查询缓存、增强索引系统和查询优化器的使用

#include "../include/kos_performance.h"
#include "../include/kos_query.h"
#include "../include/kos_core.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#ifdef _WIN32
#include <windows.h>
#define sleep_ms(ms) Sleep(ms)
#else
#include <unistd.h>
#define sleep_ms(ms) usleep((ms) * 1000)
#endif

// 创建示例数据
static kos_term* create_sample_event(const char* id, const char* timestamp, const char* value) {
    // 创建原子项作为示例
    kos_term* event = kos_mk_atomic(value, NULL);
    return event;
}

// 演示查询缓存
static void demo_query_cache(void) {
    printf("=== Query Cache Demo ===\n\n");
    
    // 创建LRU缓存，最大100项，TTL 60秒
    kos_query_cache_t* cache = kos_query_cache_create(
        100,
        KOS_CACHE_LRU,
        KOS_CACHE_INVALIDATE_TIME,
        60
    );
    
    if (!cache) {
        printf("Failed to create query cache\n");
        return;
    }
    
    // 创建示例查询结果
    kos_query_result_t* result1 = (kos_query_result_t*)calloc(1, sizeof(kos_query_result_t));
    result1->capacity = 10;
    result1->count = 3;
    result1->results = (kos_term**)calloc(3, sizeof(kos_term*));
    result1->results[0] = create_sample_event("1", "2023-01-01", "event1");
    result1->results[1] = create_sample_event("2", "2023-01-02", "event2");
    result1->results[2] = create_sample_event("3", "2023-01-03", "event3");
    
    // 存入缓存
    const char* query_key1 = "SELECT * FROM Events WHERE date > '2023-01-01'";
    kos_query_cache_put(cache, query_key1, result1);
    printf("1. Cached query result for: %s\n", query_key1);
    
    // 从缓存获取
    kos_query_result_t* cached_result = kos_query_cache_get(cache, query_key1);
    if (cached_result) {
        printf("2. Cache HIT! Retrieved %zu results from cache\n", cached_result->count);
        kos_query_result_free(cached_result);
    } else {
        printf("2. Cache MISS\n");
    }
    
    // 获取缓存统计
    kos_cache_stats_t stats = kos_query_cache_get_stats(cache);
    printf("3. Cache stats: total_entries=%zu\n", stats.total_entries);
    
    // 清空缓存
    kos_query_cache_clear(cache);
    printf("4. Cache cleared\n\n");
    
    kos_query_cache_free(cache);
}

// 演示索引管理器
static void demo_index_manager(void) {
    printf("=== Index Manager Demo ===\n\n");
    
    // 创建索引管理器
    kos_index_manager_t* manager = kos_index_manager_create();
    if (!manager) {
        printf("Failed to create index manager\n");
        return;
    }
    
    // 创建B+树索引
    kos_bplus_tree_index_t* bplus_tree = kos_bplus_tree_create("timestamp", 5);
    if (bplus_tree) {
        kos_index_manager_register(
            manager,
            KOS_INDEX_BPLUS_TREE,
            "idx_timestamp",
            "timestamp",
            bplus_tree
        );
        printf("1. Registered B+ tree index: idx_timestamp on field 'timestamp'\n");
    }
    
    // 创建倒排索引
    kos_inverted_index_t* inverted_idx = kos_inverted_index_create("description");
    if (inverted_idx) {
        kos_index_manager_register(
            manager,
            KOS_INDEX_INVERTED,
            "idx_description",
            "description",
            inverted_idx
        );
        printf("2. Registered inverted index: idx_description on field 'description'\n");
    }
    
    // 查找索引
    kos_index_metadata_t* found = kos_index_manager_find_by_field(manager, "timestamp");
    if (found) {
        printf("3. Found index '%s' for field 'timestamp'\n", found->name);
    }
    
    // 获取所有索引
    size_t count = 0;
    kos_index_metadata_t* all_indices = kos_index_manager_get_all(manager, &count);
    printf("4. Total indices: %zu\n", count);
    for (size_t i = 0; i < count; i++) {
        printf("   - %s (%s)\n", all_indices[i].name, all_indices[i].field_name);
    }
    
    // 清理
    kos_bplus_tree_free(bplus_tree);
    kos_inverted_index_free(inverted_idx);
    kos_index_manager_free(manager);
    printf("\n");
}

// 演示查询优化器
static void demo_query_optimizer(void) {
    printf("=== Query Optimizer Demo ===\n\n");
    
    // 创建索引管理器并注册索引
    kos_index_manager_t* manager = kos_index_manager_create();
    if (!manager) {
        printf("Failed to create index manager\n");
        return;
    }
    
    // 注册一个时间索引
    kos_bplus_tree_index_t* time_index = kos_bplus_tree_create("timestamp", 5);
    if (time_index) {
        kos_index_manager_register(
            manager,
            KOS_INDEX_BPLUS_TREE,
            "idx_timestamp",
            "timestamp",
            time_index
        );
    }
    
    // 创建查询
    kos_query_t* query = kos_query_create("Event");
    if (query) {
        // 添加条件（使用有索引的字段）
        kos_term* timestamp_val = kos_mk_atomic("2023-01-01", NULL);
        kos_query_add_condition(query, "timestamp", OP_GE, timestamp_val);
        
        // 设置排序
        kos_query_set_order_by(query, "timestamp", false);
        
        // 设置限制
        kos_query_set_limit(query, 100);
        
        printf("1. Created query:\n");
        printf("   Type: %s\n", query->type_name);
        printf("   Order by: %s\n", query->order_by_field);
        printf("   Limit: %zu\n", query->limit);
        
        // 生成查询计划
        kos_query_plan_t* plan = kos_query_optimizer_create_plan(query, manager);
        if (plan) {
            printf("\n2. Query Plan:\n");
            kos_query_plan_print(plan);
            
            kos_query_plan_free(plan);
        }
        
        kos_term_free(timestamp_val);
        kos_query_free(query);
    }
    
    kos_bplus_tree_free(time_index);
    kos_index_manager_free(manager);
    printf("\n");
}

// 演示B+树索引
static void demo_bplus_tree(void) {
    printf("=== B+ Tree Index Demo ===\n\n");
    
    kos_bplus_tree_index_t* tree = kos_bplus_tree_create("value", 5);
    if (!tree) {
        printf("Failed to create B+ tree\n");
        return;
    }
    
    // 插入一些数据
    for (int i = 0; i < 10; i++) {
        char key_str[32], val_str[32];
        snprintf(key_str, sizeof(key_str), "%d", i);
        snprintf(val_str, sizeof(val_str), "value_%d", i);
        
        kos_term* key = kos_mk_atomic(key_str, NULL);
        kos_term* value = kos_mk_atomic(val_str, NULL);
        
        kos_bplus_tree_insert(tree, key, value);
        
        kos_term_free(key);
        kos_term_free(value);
    }
    
    printf("1. Inserted 10 entries into B+ tree\n");
    
    // 范围查询
    kos_term* min_key = kos_mk_atomic("3", NULL);
    kos_term* max_key = kos_mk_atomic("7", NULL);
    
    kos_query_result_t* result = kos_bplus_tree_range_query(tree, min_key, max_key);
    if (result) {
        printf("2. Range query [3, 7]: found %zu results\n", result->count);
        kos_query_result_free(result);
    }
    
    kos_term_free(min_key);
    kos_term_free(max_key);
    kos_bplus_tree_free(tree);
    printf("\n");
}

// 演示倒排索引
static void demo_inverted_index(void) {
    printf("=== Inverted Index Demo ===\n\n");
    
    kos_inverted_index_t* index = kos_inverted_index_create("content");
    if (!index) {
        printf("Failed to create inverted index\n");
        return;
    }
    
    // 插入文档
    kos_term* doc1 = kos_mk_atomic("doc1", NULL);
    kos_term* doc2 = kos_mk_atomic("doc2", NULL);
    kos_term* doc3 = kos_mk_atomic("doc3", NULL);
    
    kos_inverted_index_insert(index, doc1, "hello world");
    kos_inverted_index_insert(index, doc2, "hello kos");
    kos_inverted_index_insert(index, doc3, "world peace");
    
    printf("1. Inserted 3 documents into inverted index\n");
    
    // 搜索
    kos_query_result_t* result = kos_inverted_index_search(index, "hello");
    if (result) {
        printf("2. Search for 'hello': found %zu documents\n", result->count);
        kos_query_result_free(result);
    }
    
    result = kos_inverted_index_search(index, "world");
    if (result) {
        printf("3. Search for 'world': found %zu documents\n", result->count);
        kos_query_result_free(result);
    }
    
    kos_term_free(doc1);
    kos_term_free(doc2);
    kos_term_free(doc3);
    kos_inverted_index_free(index);
    printf("\n");
}

int main(void) {
    printf("========================================\n");
    printf("KOS-TL Performance Optimization Demo\n");
    printf("Phase 7: Query Cache, Enhanced Indexes, Query Optimizer\n");
    printf("========================================\n\n");
    
    demo_query_cache();
    demo_index_manager();
    demo_bplus_tree();
    demo_inverted_index();
    demo_query_optimizer();
    
    printf("========================================\n");
    printf("Demo completed successfully!\n");
    printf("========================================\n");
    
    return 0;
}
