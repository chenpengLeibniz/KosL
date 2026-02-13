// include/kos_distributed_cache.h
// Phase 7 Enhancement: Distributed Caching
// 分布式缓存模块：支持 Redis、Memcached 等分布式缓存后端

#ifndef KOS_DISTRIBUTED_CACHE_H
#define KOS_DISTRIBUTED_CACHE_H

#include "kos_core.h"
#include "kos_query.h"
#include "kos_performance.h"  // For kos_query_cache_t
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <time.h>

// ========== 分布式缓存后端类型 ==========

typedef enum {
    KOS_DIST_CACHE_REDIS,        // Redis 后端
    KOS_DIST_CACHE_MEMCACHED,    // Memcached 后端
    KOS_DIST_CACHE_MOCK          // 模拟后端（用于测试）
} kos_dist_cache_backend_t;

// ========== 分布式缓存配置 ==========

typedef struct {
    kos_dist_cache_backend_t backend;  // 后端类型
    const char* host;                  // 服务器地址
    int port;                          // 端口号
    const char* password;              // 密码（可选）
    int timeout_ms;                    // 超时时间（毫秒）
    size_t max_connections;            // 最大连接数
    bool use_ssl;                      // 是否使用 SSL
} kos_dist_cache_config_t;

// ========== 分布式缓存操作结果 ==========

typedef enum {
    KOS_DIST_CACHE_OK,                // 成功
    KOS_DIST_CACHE_ERROR,              // 一般错误
    KOS_DIST_CACHE_NOT_FOUND,          // 未找到
    KOS_DIST_CACHE_CONNECTION_ERROR,   // 连接错误
    KOS_DIST_CACHE_TIMEOUT,            // 超时
    KOS_DIST_CACHE_SERIALIZATION_ERROR // 序列化错误
} kos_dist_cache_result_t;

// ========== 分布式缓存适配器接口 ==========

// 前向声明
typedef struct kos_dist_cache_adapter kos_dist_cache_adapter_t;

// 连接函数：建立与分布式缓存的连接
typedef kos_dist_cache_result_t (*kos_dist_cache_connect_fn)(
    kos_dist_cache_adapter_t* adapter,
    const kos_dist_cache_config_t* config
);

// 断开连接函数
typedef kos_dist_cache_result_t (*kos_dist_cache_disconnect_fn)(
    kos_dist_cache_adapter_t* adapter
);

// 获取函数：从分布式缓存获取值
typedef kos_dist_cache_result_t (*kos_dist_cache_get_fn)(
    kos_dist_cache_adapter_t* adapter,
    const char* key,
    kos_query_result_t** result  // 输出参数
);

// 设置函数：将值存入分布式缓存
typedef kos_dist_cache_result_t (*kos_dist_cache_set_fn)(
    kos_dist_cache_adapter_t* adapter,
    const char* key,
    kos_query_result_t* result,
    time_t ttl_seconds  // 生存时间（秒），0 表示不过期
);

// 删除函数：从分布式缓存删除键
typedef kos_dist_cache_result_t (*kos_dist_cache_delete_fn)(
    kos_dist_cache_adapter_t* adapter,
    const char* key
);

// 存在性检查函数
typedef bool (*kos_dist_cache_exists_fn)(
    kos_dist_cache_adapter_t* adapter,
    const char* key
);

// 清空函数：清空所有缓存
typedef kos_dist_cache_result_t (*kos_dist_cache_flush_fn)(
    kos_dist_cache_adapter_t* adapter
);

// 获取统计信息函数
typedef struct {
    size_t total_keys;         // 总键数
    size_t memory_used_bytes;   // 使用的内存（字节）
    size_t hit_count;           // 命中次数
    size_t miss_count;          // 未命中次数
    double hit_ratio;           // 命中率
} kos_dist_cache_stats_t;

typedef kos_dist_cache_result_t (*kos_dist_cache_stats_fn)(
    kos_dist_cache_adapter_t* adapter,
    kos_dist_cache_stats_t* stats
);

// 分布式缓存适配器结构
struct kos_dist_cache_adapter {
    kos_dist_cache_backend_t backend_type;  // 后端类型
    void* backend_handle;                    // 后端句柄（私有数据）
    
    // 适配器函数
    kos_dist_cache_connect_fn connect;
    kos_dist_cache_disconnect_fn disconnect;
    kos_dist_cache_get_fn get;
    kos_dist_cache_set_fn set;
    kos_dist_cache_delete_fn delete;
    kos_dist_cache_exists_fn exists;
    kos_dist_cache_flush_fn flush;
    kos_dist_cache_stats_fn stats;
    
    // 状态
    bool is_connected;                      // 是否已连接
    kos_dist_cache_config_t config;         // 配置信息
};

// ========== 分布式缓存管理器 ==========

typedef struct {
    kos_dist_cache_adapter_t* adapter;      // 当前使用的适配器
    kos_query_cache_t* local_cache;          // 本地缓存（作为二级缓存）
    bool use_local_cache;                    // 是否使用本地缓存
    size_t local_cache_size;                 // 本地缓存大小
} kos_distributed_cache_t;

// ========== 分布式缓存 API ==========

// 创建分布式缓存管理器
kos_distributed_cache_t* kos_distributed_cache_create(
    kos_dist_cache_backend_t backend,
    const kos_dist_cache_config_t* config,
    bool use_local_cache,
    size_t local_cache_size
);

// 释放分布式缓存管理器
void kos_distributed_cache_free(kos_distributed_cache_t* cache);

// 连接到分布式缓存服务器
kos_dist_cache_result_t kos_distributed_cache_connect(
    kos_distributed_cache_t* cache
);

// 断开连接
kos_dist_cache_result_t kos_distributed_cache_disconnect(
    kos_distributed_cache_t* cache
);

// 从分布式缓存获取查询结果（支持二级缓存）
kos_query_result_t* kos_distributed_cache_get(
    kos_distributed_cache_t* cache,
    const char* query_key
);

// 将查询结果存入分布式缓存（同时更新本地缓存）
kos_dist_cache_result_t kos_distributed_cache_set(
    kos_distributed_cache_t* cache,
    const char* query_key,
    kos_query_result_t* result,
    time_t ttl_seconds
);

// 从分布式缓存删除键
kos_dist_cache_result_t kos_distributed_cache_delete(
    kos_distributed_cache_t* cache,
    const char* query_key
);

// 检查键是否存在
bool kos_distributed_cache_exists(
    kos_distributed_cache_t* cache,
    const char* query_key
);

// 清空所有缓存
kos_dist_cache_result_t kos_distributed_cache_flush(
    kos_distributed_cache_t* cache
);

// 获取统计信息
kos_dist_cache_result_t kos_distributed_cache_get_stats(
    kos_distributed_cache_t* cache,
    kos_dist_cache_stats_t* stats
);

// ========== 适配器注册函数 ==========

// 注册 Redis 适配器
kos_dist_cache_adapter_t* kos_dist_cache_create_redis_adapter(void);

// 注册 Memcached 适配器
kos_dist_cache_adapter_t* kos_dist_cache_create_memcached_adapter(void);

// 注册模拟适配器（用于测试，不依赖外部服务）
kos_dist_cache_adapter_t* kos_dist_cache_create_mock_adapter(void);

// 释放适配器
void kos_dist_cache_adapter_free(kos_dist_cache_adapter_t* adapter);

// ========== 辅助函数 ==========

// 创建默认配置
kos_dist_cache_config_t kos_dist_cache_config_default(
    kos_dist_cache_backend_t backend
);

// 获取错误消息
const char* kos_dist_cache_error_string(kos_dist_cache_result_t result);

// 序列化查询结果为字符串（用于存储到分布式缓存）
char* kos_dist_cache_serialize_result(kos_query_result_t* result);

// 反序列化字符串为查询结果
kos_query_result_t* kos_dist_cache_deserialize_result(const char* data);

#endif // KOS_DISTRIBUTED_CACHE_H
