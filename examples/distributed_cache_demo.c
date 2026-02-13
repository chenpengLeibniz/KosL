// examples/distributed_cache_demo.c
// Phase 7 Enhancement: Distributed Caching Demo
// 分布式缓存演示程序：展示分布式缓存的使用

#include "../include/kos_distributed_cache.h"
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

// 创建示例查询结果（简化版本，仅用于演示）
static kos_query_result_t* create_sample_query_result(const char* query_name) {
    (void)query_name;  // 避免未使用参数警告
    kos_query_result_t* result = (kos_query_result_t*)calloc(1, sizeof(kos_query_result_t));
    if (!result) {
        return NULL;
    }
    
    // 创建空的结果集（仅用于演示缓存功能）
    result->count = 3;
    result->capacity = 3;
    result->results = NULL;  // 简化实现，不创建实际的term对象
    
    return result;
}

// 演示模拟后端（Mock Backend）
static void demo_mock_backend(void) {
    printf("\n========================================\n");
    printf("Demo 1: Mock Distributed Cache Backend\n");
    printf("========================================\n\n");
    
    // 创建配置
    kos_dist_cache_config_t config = kos_dist_cache_config_default(KOS_DIST_CACHE_MOCK);
    
    // 创建分布式缓存（启用本地二级缓存）
    kos_distributed_cache_t* cache = kos_distributed_cache_create(
        KOS_DIST_CACHE_MOCK,
        &config,
        true,   // 使用本地缓存
        100     // 本地缓存大小
    );
    
    if (!cache) {
        printf("Error: Failed to create distributed cache\n");
        return;
    }
    
    // 连接到缓存
    kos_dist_cache_result_t res = kos_distributed_cache_connect(cache);
    if (res != KOS_DIST_CACHE_OK) {
        printf("Error: Failed to connect to cache: %s\n", 
               kos_dist_cache_error_string(res));
        kos_distributed_cache_free(cache);
        return;
    }
    
    printf("✓ Connected to mock distributed cache\n\n");
    
    // 测试1: 存储查询结果
    printf("Test 1: Storing query results...\n");
    const char* query_keys[] = {
        "query_1",
        "query_2",
        "query_3"
    };
    
    for (size_t i = 0; i < 3; i++) {
        kos_query_result_t* result = create_sample_query_result(query_keys[i]);
        if (result) {
            res = kos_distributed_cache_set(cache, query_keys[i], result, 60);  // TTL: 60秒
            if (res == KOS_DIST_CACHE_OK) {
                printf("  ✓ Stored: %s\n", query_keys[i]);
            } else {
                printf("  ✗ Failed to store %s: %s\n", 
                       query_keys[i], 
                       kos_dist_cache_error_string(res));
            }
        }
    }
    
    printf("\n");
    
    // 测试2: 检索查询结果
    printf("Test 2: Retrieving query results...\n");
    for (size_t i = 0; i < 3; i++) {
        kos_query_result_t* result = kos_distributed_cache_get(cache, query_keys[i]);
        if (result) {
            printf("  ✓ Retrieved: %s (count: %zu)\n", query_keys[i], result->count);
        } else {
            printf("  ✗ Not found: %s\n", query_keys[i]);
        }
    }
    
    printf("\n");
    
    // 测试3: 检查键是否存在
    printf("Test 3: Checking key existence...\n");
    printf("  query_1 exists: %s\n", 
           kos_distributed_cache_exists(cache, "query_1") ? "Yes" : "No");
    printf("  query_nonexistent exists: %s\n",
           kos_distributed_cache_exists(cache, "query_nonexistent") ? "Yes" : "No");
    
    printf("\n");
    
    // 测试4: 获取统计信息
    printf("Test 4: Cache statistics...\n");
    kos_dist_cache_stats_t stats = {0};
    res = kos_distributed_cache_get_stats(cache, &stats);
    if (res == KOS_DIST_CACHE_OK) {
        printf("  Total keys: %zu\n", stats.total_keys);
        printf("  Memory used: %zu bytes\n", stats.memory_used_bytes);
        printf("  Hit count: %zu\n", stats.hit_count);
        printf("  Miss count: %zu\n", stats.miss_count);
        printf("  Hit ratio: %.2f%%\n", stats.hit_ratio * 100.0);
    } else {
        printf("  ✗ Failed to get stats: %s\n", kos_dist_cache_error_string(res));
    }
    
    printf("\n");
    
    // 测试5: 删除键
    printf("Test 5: Deleting keys...\n");
    res = kos_distributed_cache_delete(cache, "query_2");
    if (res == KOS_DIST_CACHE_OK) {
        printf("  ✓ Deleted: query_2\n");
    } else {
        printf("  ✗ Failed to delete query_2: %s\n", kos_dist_cache_error_string(res));
    }
    
    // 验证删除
    printf("  query_2 exists after deletion: %s\n",
           kos_distributed_cache_exists(cache, "query_2") ? "Yes" : "No");
    
    printf("\n");
    
    // 测试6: 清空缓存
    printf("Test 6: Flushing cache...\n");
    res = kos_distributed_cache_flush(cache);
    if (res == KOS_DIST_CACHE_OK) {
        printf("  ✓ Cache flushed\n");
    } else {
        printf("  ✗ Failed to flush: %s\n", kos_dist_cache_error_string(res));
    }
    
    // 验证清空
    printf("  query_1 exists after flush: %s\n",
           kos_distributed_cache_exists(cache, "query_1") ? "Yes" : "No");
    
    // 断开连接并释放
    kos_distributed_cache_disconnect(cache);
    kos_distributed_cache_free(cache);
    
    printf("\n✓ Mock backend demo completed\n");
}

// 演示二级缓存（本地缓存 + 分布式缓存）
static void demo_two_level_cache(void) {
    printf("\n========================================\n");
    printf("Demo 2: Two-Level Cache (Local + Distributed)\n");
    printf("========================================\n\n");
    
    kos_dist_cache_config_t config = kos_dist_cache_config_default(KOS_DIST_CACHE_MOCK);
    
    // 创建分布式缓存（启用本地二级缓存）
    kos_distributed_cache_t* cache = kos_distributed_cache_create(
        KOS_DIST_CACHE_MOCK,
        &config,
        true,   // 使用本地缓存
        10      // 本地缓存大小（小缓存以演示效果）
    );
    
    if (!cache) {
        printf("Error: Failed to create distributed cache\n");
        return;
    }
    
    kos_distributed_cache_connect(cache);
    
    printf("Two-level cache setup:\n");
    printf("  Level 1: Local cache (LRU, size: 10)\n");
    printf("  Level 2: Distributed cache (Mock)\n\n");
    
    // 存储多个查询结果
    printf("Storing 15 query results...\n");
    for (int i = 1; i <= 15; i++) {
        char key[32];
        snprintf(key, sizeof(key), "query_%d", i);
        
        kos_query_result_t* result = create_sample_query_result(key);
        if (result) {
            kos_distributed_cache_set(cache, key, result, 300);
        }
    }
    
    printf("\n");
    
    // 测试缓存命中（应该从本地缓存获取）
    printf("Testing cache hits (should hit local cache)...\n");
    for (int i = 1; i <= 5; i++) {
        char key[32];
        snprintf(key, sizeof(key), "query_%d", i);
        
        clock_t start = clock();
        kos_query_result_t* result = kos_distributed_cache_get(cache, key);
        clock_t end = clock();
        
        if (result) {
            double elapsed_ms = ((double)(end - start) / CLOCKS_PER_SEC) * 1000.0;
            printf("  ✓ Retrieved %s in %.3f ms\n", key, elapsed_ms);
        }
    }
    
    printf("\n");
    
    // 获取统计信息
    kos_dist_cache_stats_t stats = {0};
    kos_distributed_cache_get_stats(cache, &stats);
    printf("Distributed cache stats:\n");
    printf("  Hit ratio: %.2f%%\n", stats.hit_ratio * 100.0);
    
    kos_distributed_cache_disconnect(cache);
    kos_distributed_cache_free(cache);
    
    printf("\n✓ Two-level cache demo completed\n");
}

// 演示适配器切换（Mock -> Redis -> Memcached）
static void demo_adapter_switching(void) {
    printf("\n========================================\n");
    printf("Demo 3: Adapter Switching\n");
    printf("========================================\n\n");
    
    printf("This demo shows how to switch between different cache backends:\n\n");
    
    // Mock 适配器
    printf("1. Mock Adapter (for testing, no external dependencies):\n");
    kos_dist_cache_adapter_t* mock_adapter = kos_dist_cache_create_mock_adapter();
    if (mock_adapter) {
        printf("   ✓ Created mock adapter\n");
        kos_dist_cache_adapter_free(mock_adapter);
    }
    
    // Redis 适配器（占位实现）
    printf("\n2. Redis Adapter (placeholder, requires hiredis library):\n");
    kos_dist_cache_adapter_t* redis_adapter = kos_dist_cache_create_redis_adapter();
    if (redis_adapter) {
        printf("   ✓ Created Redis adapter (not connected - requires Redis server)\n");
        printf("   Note: Real Redis integration requires:\n");
        printf("     - hiredis library\n");
        printf("     - Redis server running\n");
        printf("     - Implementation of redis_* functions\n");
        kos_dist_cache_adapter_free(redis_adapter);
    }
    
    // Memcached 适配器（占位实现）
    printf("\n3. Memcached Adapter (placeholder, requires libmemcached):\n");
    kos_dist_cache_adapter_t* memcached_adapter = kos_dist_cache_create_memcached_adapter();
    if (memcached_adapter) {
        printf("   ✓ Created Memcached adapter (not connected - requires Memcached server)\n");
        printf("   Note: Real Memcached integration requires:\n");
        printf("     - libmemcached library\n");
        printf("     - Memcached server running\n");
        printf("     - Implementation of memcached_* functions\n");
        kos_dist_cache_adapter_free(memcached_adapter);
    }
    
    printf("\n✓ Adapter switching demo completed\n");
}

int main(void) {
    printf("========================================\n");
    printf("KOS-TL Distributed Caching Demo\n");
    printf("Phase 7 Enhancement: Distributed Cache Integration\n");
    printf("========================================\n");
    
    // 演示1: Mock 后端
    demo_mock_backend();
    
    // 演示2: 二级缓存
    demo_two_level_cache();
    
    // 演示3: 适配器切换
    demo_adapter_switching();
    
    printf("\n========================================\n");
    printf("All demos completed successfully!\n");
    printf("========================================\n");
    
    printf("\nNext Steps:\n");
    printf("1. Implement real Redis adapter using hiredis\n");
    printf("2. Implement real Memcached adapter using libmemcached\n");
    printf("3. Add connection pooling for better performance\n");
    printf("4. Add retry logic for network failures\n");
    printf("5. Add monitoring and metrics collection\n");
    printf("========================================\n");
    
    return 0;
}
