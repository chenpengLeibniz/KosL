// include/kos_ontology_version.h
// KOS Dynamic Ontology Management API - Phase 2
// 动态本体管理：版本控制、运行时更新、变更影响分析

#ifndef KOS_ONTOLOGY_VERSION_H
#define KOS_ONTOLOGY_VERSION_H

#include "kos_ontology.h"
#include <stdbool.h>
#include <stddef.h>

// ========== 本体版本管理 ==========

// 版本信息结构
typedef struct {
    char* version_name;        // 版本名称（如 "v1.0.0"）
    char* description;         // 版本描述
    char* commit_hash;         // 提交哈希（类似 Git）
    char* timestamp;           // 创建时间戳
    TypeOntology* snapshot;    // 本体快照（深拷贝）
    struct kos_ontology_version* parent; // 父版本（用于版本链）
} kos_ontology_version_t;

// 版本差异结构
typedef struct {
    char* type_name;          // 变更的类型名
    enum {
        DIFF_ADDED,           // 新增类型
        DIFF_REMOVED,         // 删除类型
        DIFF_MODIFIED         // 修改类型
    } change_type;
    kos_term* old_type_def;   // 旧类型定义（如果是修改或删除）
    kos_term* new_type_def;   // 新类型定义（如果是新增或修改）
    kos_term* old_ctx;        // 旧上下文
    kos_term* new_ctx;        // 新上下文
} kos_ontology_diff_item_t;

// 版本差异列表
typedef struct {
    kos_ontology_diff_item_t* items; // 差异项数组
    size_t count;                    // 差异项数量
    size_t capacity;                 // 数组容量
} kos_ontology_diff_t;

// ========== 变更影响分析 ==========

// 依赖类型
typedef enum {
    DEPENDENCY_INSTANCE,      // 实例依赖（使用该类型的实例）
    DEPENDENCY_QUERY,         // 查询依赖（查询中使用该类型）
    DEPENDENCY_TYPE_DEF,      // 类型定义依赖（其他类型定义中引用）
    DEPENDENCY_PREDICATE      // 谓词依赖（谓词中使用该类型）
} dependency_type_t;

// 依赖项
typedef struct {
    dependency_type_t type;   // 依赖类型
    char* resource_id;        // 资源标识符（实例ID、查询ID等）
    char* description;        // 依赖描述
    struct kos_dependency_item* next; // 下一个依赖项
} kos_dependency_item_t;

// 影响分析结果
typedef struct {
    char* type_name;                  // 变更的类型名
    kos_term* old_type_def;           // 旧类型定义
    kos_term* new_type_def;           // 新类型定义
    kos_term* old_ctx;                // 旧上下文
    kos_term* new_ctx;                // 新上下文
    kos_dependency_item_t* dependencies; // 依赖列表
    size_t affected_instance_count;    // 受影响的实例数量
    size_t affected_query_count;       // 受影响的查询数量
    bool breaking_change;             // 是否破坏性变更
    char* migration_hint;             // 迁移提示
} kos_impact_analysis_t;

// 迁移脚本
typedef struct {
    char* script_content;     // 迁移脚本内容（JSON 或 KOS-TL 格式）
    size_t script_length;     // 脚本长度
    kos_impact_analysis_t* impact; // 关联的影响分析
} kos_migration_script_t;

// ========== 运行时更新 ==========

// 更新操作类型
typedef enum {
    UPDATE_ADD_TYPE,          // 添加类型
    UPDATE_REMOVE_TYPE,       // 删除类型
    UPDATE_MODIFY_TYPE        // 修改类型
} update_operation_t;

// 单个更新操作
typedef struct {
    update_operation_t op;    // 操作类型
    char* type_name;          // 类型名
    kos_term* new_type_def;   // 新类型定义（如果是添加或修改）
    kos_term* new_ctx;        // 新上下文
    kos_term* old_type_def;   // 旧类型定义（如果是修改或删除，用于回滚）
    kos_term* old_ctx;        // 旧上下文
} kos_ontology_update_t;

// 更新事务
typedef struct {
    kos_ontology_update_t* updates; // 更新操作数组
    size_t count;                   // 更新数量
    size_t capacity;                // 数组容量
    bool committed;                 // 是否已提交
    kos_ontology_version_t* version_before; // 更新前的版本
    kos_ontology_version_t* version_after;  // 更新后的版本
} kos_ontology_transaction_t;

// ========== 版本管理 API ==========

// --- 版本创建和管理 ---

// 创建本体版本（创建快照）
kos_ontology_version_t* kos_ontology_create_version(
    TypeOntology* ontology,
    const char* version_name,
    const char* description
);

// 获取当前版本
kos_ontology_version_t* kos_ontology_get_current_version(TypeOntology* ontology);

// 列出所有版本
kos_ontology_version_t** kos_ontology_list_versions(
    TypeOntology* ontology,
    size_t* count
);

// 查找指定版本
kos_ontology_version_t* kos_ontology_find_version(
    TypeOntology* ontology,
    const char* version_name
);

// 回滚到指定版本
int kos_ontology_rollback(
    TypeOntology* ontology,
    const char* version_name
);

// 释放版本对象
void kos_ontology_version_free(kos_ontology_version_t* version);

// --- 版本比较和差异 ---

// 比较两个版本
kos_ontology_diff_t* kos_ontology_diff(
    TypeOntology* ontology,
    const char* version1_name,
    const char* version2_name
);

// 比较当前版本与指定版本
kos_ontology_diff_t* kos_ontology_diff_current(
    TypeOntology* ontology,
    const char* version_name
);

// 释放差异对象
void kos_ontology_diff_free(kos_ontology_diff_t* diff);

// ========== 运行时更新 API ==========

// --- 原子性更新 ---

// 开始更新事务
kos_ontology_transaction_t* kos_ontology_begin_transaction(TypeOntology* ontology);

// 添加更新操作到事务
int kos_ontology_transaction_add_update(
    kos_ontology_transaction_t* transaction,
    update_operation_t op,
    const char* type_name,
    kos_term* new_type_def,
    kos_term* new_ctx
);

// 从 .kos 源添加更新操作（经 kos-core 校验，非法类型不能创建）
int kos_ontology_transaction_add_update_from_kos(
    kos_ontology_transaction_t* transaction,
    update_operation_t op,
    const char* type_name,
    const char* kos_type_expr,
    kos_term* new_ctx,
    char* errmsg,
    size_t errmsg_size
);

// 提交事务（原子性）
int kos_ontology_commit_transaction(
    TypeOntology* ontology,
    kos_ontology_transaction_t* transaction,
    const char* version_name,
    const char* description
);

// 回滚事务
int kos_ontology_rollback_transaction(
    TypeOntology* ontology,
    kos_ontology_transaction_t* transaction
);

// 释放事务对象
void kos_ontology_transaction_free(kos_ontology_transaction_t* transaction);

// --- 批量更新 ---

// 批量更新（原子性）
int kos_ontology_batch_update(
    TypeOntology* ontology,
    kos_ontology_update_t* updates,
    size_t count,
    const char* version_name,
    const char* description
);

// --- 原子性更新单个类型 ---

// 原子性更新类型定义
int kos_ontology_update_atomic(
    TypeOntology* ontology,
    const char* type_name,
    kos_term* new_type_def,
    kos_term* new_ctx,
    const char* version_name,
    const char* description
);

// ========== 变更影响分析 API ==========

// 分析类型变更的影响
kos_impact_analysis_t* kos_ontology_analyze_impact(
    TypeOntology* ontology,
    const char* type_name,
    kos_term* new_type_def,
    kos_term* new_ctx
);

// 分析批量更新的影响
kos_impact_analysis_t** kos_ontology_analyze_batch_impact(
    TypeOntology* ontology,
    kos_ontology_update_t* updates,
    size_t count,
    size_t* analysis_count
);

// 检测依赖关系
kos_dependency_item_t* kos_ontology_detect_dependencies(
    TypeOntology* ontology,
    const char* type_name
);

// 释放影响分析结果
void kos_impact_analysis_free(kos_impact_analysis_t* analysis);

// 释放依赖项列表
void kos_dependency_list_free(kos_dependency_item_t* dependencies);

// ========== 迁移脚本生成 API ==========

// 生成迁移脚本
kos_migration_script_t* kos_ontology_generate_migration(
    kos_impact_analysis_t* impact
);

// 生成批量更新的迁移脚本
kos_migration_script_t* kos_ontology_generate_batch_migration(
    kos_impact_analysis_t** analyses,
    size_t count
);

// 执行迁移脚本（应用到知识集 K）
int kos_ontology_apply_migration(
    kos_term** K,  // 知识集指针（可能需要修改）
    kos_migration_script_t* script
);

// 释放迁移脚本
void kos_migration_script_free(kos_migration_script_t* script);

// ========== 变更通知 API ==========

// 变更通知回调函数类型
typedef void (*kos_ontology_change_callback_t)(
    const char* type_name,
    update_operation_t op,
    void* user_data
);

// 注册变更通知回调
int kos_ontology_register_change_callback(
    TypeOntology* ontology,
    kos_ontology_change_callback_t callback,
    void* user_data
);

// 取消注册变更通知回调
int kos_ontology_unregister_change_callback(
    TypeOntology* ontology,
    kos_ontology_change_callback_t callback
);

#endif // KOS_ONTOLOGY_VERSION_H
