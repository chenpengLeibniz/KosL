// include/kos_data_integration.h
// Phase 3: Data Integration & Fusion
// 数据集成与融合框架 - 对标 Palantir 多数据源集成能力

#ifndef KOS_DATA_INTEGRATION_H
#define KOS_DATA_INTEGRATION_H

#include "kos_core.h"
#include <stdbool.h>
#include <stddef.h>

// ========== 数据源连接器框架 ==========

// 数据源类型
typedef enum {
    DATA_SOURCE_CSV,        // CSV 文件
    DATA_SOURCE_JSON,       // JSON 文件
    DATA_SOURCE_POSTGRESQL, // PostgreSQL 数据库
    DATA_SOURCE_MYSQL,      // MySQL 数据库
    DATA_SOURCE_MONGODB,    // MongoDB 数据库
    DATA_SOURCE_KAFKA,      // Kafka 流
    DATA_SOURCE_MQTT,       // MQTT 流
    DATA_SOURCE_REST,       // REST API
    DATA_SOURCE_GRAPHQL,    // GraphQL API
    DATA_SOURCE_CUSTOM      // 自定义数据源
} data_source_type_t;

// 数据源连接配置（通用配置结构）
typedef struct {
    data_source_type_t type;
    const char* name;           // 数据源名称
    const char* connection_string; // 连接字符串（如文件路径、数据库URL等）
    void* custom_config;        // 自定义配置（特定于数据源类型）
} kos_data_source_config_t;

// 数据源连接器接口
typedef struct kos_data_source_connector {
    const char* name;                    // 连接器名称
    data_source_type_t type;             // 数据源类型
    
    // 连接/断开连接
    int (*connect)(kos_data_source_config_t* config, void** handle);
    int (*disconnect)(void* handle);
    
    // 读取模式（Schema）
    kos_term* (*read_schema)(void* handle, const char* table_or_collection);
    
    // 读取数据
    kos_term* (*read_data)(void* handle, const char* query_or_filter);
    
    // 测试连接
    int (*test_connection)(kos_data_source_config_t* config);
    
    // 释放资源
    void (*free_handle)(void* handle);
} kos_data_source_connector_t;

// 数据源连接句柄
typedef struct {
    kos_data_source_connector_t* connector;
    void* handle;                        // 连接器特定的句柄
    kos_data_source_config_t config;     // 配置信息
    bool connected;                      // 连接状态
} kos_data_source_handle_t;

// ========== 连接器注册表 ==========

// 注册数据源连接器
int kos_register_data_source_connector(
    const char* name,
    kos_data_source_connector_t* connector
);

// 注销数据源连接器
int kos_unregister_data_source_connector(const char* name);

// 获取连接器
kos_data_source_connector_t* kos_get_data_source_connector(
    data_source_type_t type,
    const char* name
);

// ========== 数据源管理 ==========

// 创建数据源连接
kos_data_source_handle_t* kos_create_data_source(
    kos_data_source_config_t* config
);

// 关闭数据源连接
int kos_close_data_source(kos_data_source_handle_t* handle);

// 从数据源读取模式
kos_term* kos_data_source_read_schema(
    kos_data_source_handle_t* handle,
    const char* table_or_collection
);

// 从数据源读取数据
kos_term* kos_data_source_read_data(
    kos_data_source_handle_t* handle,
    const char* query_or_filter
);

// ========== 数据融合规则引擎 ==========

// 冲突解决策略
typedef enum {
    FUSION_LATEST_FIRST,      // 最新优先（时间戳最新）
    FUSION_WEIGHTED_AVG,      // 加权平均
    FUSION_MAX,               // 取最大值
    FUSION_MIN,               // 取最小值
    FUSION_UNION,             // 并集（合并所有值）
    FUSION_INTERSECTION,      // 交集（只保留共同值）
    FUSION_CUSTOM             // 自定义策略（使用KOS-TL规则）
} fusion_strategy_t;

// 字段融合规则
typedef struct {
    const char* field_name;           // 字段名
    fusion_strategy_t strategy;       // 融合策略
    double weight;                    // 权重（用于加权平均）
    kos_term* custom_rule;            // 自定义规则（KOS-TL类型）
} kos_field_fusion_rule_t;

// 数据融合规则
typedef struct {
    const char* target_type;          // 目标类型名
    kos_field_fusion_rule_t* field_rules; // 字段规则数组
    size_t field_rule_count;          // 字段规则数量
    fusion_strategy_t default_strategy; // 默认策略
    kos_term* type_constraint;        // 类型约束（KOS-TL类型）
} kos_fusion_rule_t;

// 创建融合规则
kos_fusion_rule_t* kos_create_fusion_rule(
    const char* target_type,
    fusion_strategy_t default_strategy
);

// 添加字段融合规则
int kos_fusion_rule_add_field(
    kos_fusion_rule_t* rule,
    const char* field_name,
    fusion_strategy_t strategy,
    double weight
);

// 释放融合规则
void kos_fusion_rule_free(kos_fusion_rule_t* rule);

// ========== 数据融合执行 ==========

// 融合两个数据项
kos_term* kos_fuse_data(
    kos_term* data1,
    kos_term* data2,
    kos_fusion_rule_t* rule
);

// 融合多个数据项（数组）
kos_term* kos_fuse_data_array(
    kos_term** data_array,
    size_t count,
    kos_fusion_rule_t* rule
);

// 融合来自多个数据源的数据
kos_term* kos_fuse_from_sources(
    kos_data_source_handle_t** sources,
    size_t source_count,
    const char* query_or_filter,
    kos_fusion_rule_t* rule
);

// ========== 内置连接器（简化实现） ==========

// 初始化内置连接器（CSV, JSON等）
int kos_init_builtin_connectors(void);

// 清理内置连接器
void kos_cleanup_builtin_connectors(void);

// ========== 增强的数据融合 API ==========

// 融合两个数据项（增强版：字段级融合）
kos_term* kos_fuse_data_enhanced(
    kos_term* data1,
    kos_term* data2,
    kos_fusion_rule_t* rule
);

// 融合多个数据项（增强版）
kos_term* kos_fuse_data_array_enhanced(
    kos_term** data_array,
    size_t count,
    kos_fusion_rule_t* rule
);

// 时间戳感知的融合
kos_term* kos_fuse_data_with_timestamp(
    kos_term* data1,
    kos_term* data2,
    kos_fusion_rule_t* rule
);

#endif // KOS_DATA_INTEGRATION_H
