// Phase 6: Access Control & Security
// 权限与安全框架 - 对标 Palantir 细粒度访问控制和审计日志
#ifndef KOS_SECURITY_H
#define KOS_SECURITY_H

#include "kos_core.h"
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <time.h>

// ========== 权限类型 ==========
typedef enum {
    KOS_PERM_READ,          // 读权限
    KOS_PERM_WRITE,         // 写权限
    KOS_PERM_DELETE,        // 删除权限
    KOS_PERM_QUERY,         // 查询权限
    KOS_PERM_ADMIN          // 管理员权限
} kos_permission_type_t;

// ========== 权限 ==========
typedef struct {
    kos_permission_type_t type;     // 权限类型
    const char* resource;           // 资源（如 "ontology/types", "instances", "*" 表示所有）
    bool allowed;                   // 是否允许
} kos_permission_t;

// ========== 角色 ==========
typedef struct kos_role kos_role_t;

// ========== 用户 ==========
typedef struct {
    const char* user_id;           // 用户ID
    const char* username;           // 用户名
    kos_role_t** roles;             // 角色列表
    size_t role_count;              // 角色数量
    // ABAC 属性
    char** attributes;               // 属性键值对（格式："key=value"）
    size_t attribute_count;          // 属性数量
} kos_user_t;

// ========== RBAC 系统 ==========
typedef struct kos_rbac_system kos_rbac_system_t;

// 创建 RBAC 系统
kos_rbac_system_t* kos_rbac_create(void);
void kos_rbac_free(kos_rbac_system_t* rbac);

// 角色管理
kos_role_t* kos_create_role(
    const char* role_name,
    const kos_permission_t* permissions,
    size_t count
);

void kos_role_free(kos_role_t* role);

const char* kos_role_get_name(const kos_role_t* role);
int kos_role_add_permission(kos_role_t* role, const kos_permission_t* permission);
int kos_role_remove_permission(kos_role_t* role, kos_permission_type_t type, const char* resource);

// 用户管理
kos_user_t* kos_create_user(const char* user_id, const char* username);
void kos_user_free(kos_user_t* user);

// 角色分配
int kos_assign_role(
    kos_rbac_system_t* rbac,
    const char* user_id,
    const char* role_name
);

int kos_remove_role(
    kos_rbac_system_t* rbac,
    const char* user_id,
    const char* role_name
);

// 权限检查
bool kos_check_permission(
    kos_rbac_system_t* rbac,
    const char* user_id,
    const char* resource,
    kos_permission_type_t permission
);

// 用户查询
kos_user_t* kos_get_user(kos_rbac_system_t* rbac, const char* user_id);
kos_role_t* kos_get_role(kos_rbac_system_t* rbac, const char* role_name);

// 注册角色到系统
int kos_rbac_register_role(kos_rbac_system_t* rbac, kos_role_t* role);

// ========== ABAC 属性 ==========
typedef struct {
    const char* key;                // 属性键
    const char* value;              // 属性值
} kos_attribute_t;

// ABAC 策略
typedef struct {
    const char* policy_name;       // 策略名称
    kos_attribute_t* conditions;   // 条件属性列表
    size_t condition_count;         // 条件数量
    kos_permission_t* permissions;  // 允许的权限
    size_t permission_count;        // 权限数量
} kos_abac_policy_t;

// ABAC 系统
typedef struct kos_abac_system kos_abac_system_t;

kos_abac_system_t* kos_abac_create(void);
void kos_abac_free(kos_abac_system_t* abac);

// 策略管理
int kos_abac_add_policy(
    kos_abac_system_t* abac,
    const kos_abac_policy_t* policy
);

int kos_abac_remove_policy(
    kos_abac_system_t* abac,
    const char* policy_name
);

// ABAC 权限检查
bool kos_abac_check_permission(
    kos_abac_system_t* abac,
    const kos_user_t* user,
    const char* resource,
    kos_permission_type_t permission,
    const kos_attribute_t* context_attributes,
    size_t context_attribute_count
);

// ========== 审计日志 ==========

// 审计日志操作类型
typedef enum {
    KOS_AUDIT_OP_CREATE,           // 创建操作
    KOS_AUDIT_OP_READ,             // 读取操作
    KOS_AUDIT_OP_UPDATE,           // 更新操作
    KOS_AUDIT_OP_DELETE,           // 删除操作
    KOS_AUDIT_OP_QUERY,            // 查询操作
    KOS_AUDIT_OP_LOGIN,            // 登录操作
    KOS_AUDIT_OP_LOGOUT,           // 登出操作
    KOS_AUDIT_OP_PERMISSION_DENIED // 权限拒绝
} kos_audit_operation_t;

// 审计日志条目
typedef struct {
    int64_t timestamp_ms;          // 时间戳（毫秒）
    const char* user_id;            // 用户ID
    kos_audit_operation_t operation; // 操作类型
    const char* resource;           // 资源
    const char* details;            // 详细信息（JSON格式）
    bool success;                   // 是否成功
    const char* ip_address;         // IP地址（可选）
} kos_audit_log_entry_t;

// 审计日志系统
typedef struct kos_audit_system kos_audit_system_t;

kos_audit_system_t* kos_audit_create(const char* log_file_path);
void kos_audit_free(kos_audit_system_t* audit);

// 记录审计日志
int kos_audit_log(
    kos_audit_system_t* audit,
    const char* user_id,
    kos_audit_operation_t operation,
    const char* resource,
    const char* details,
    bool success,
    const char* ip_address
);

// 查询审计日志
typedef struct {
    const char* user_id;            // 过滤：用户ID（NULL表示不过滤）
    kos_audit_operation_t operation; // 过滤：操作类型（-1表示不过滤）
    const char* resource;           // 过滤：资源（NULL表示不过滤）
    int64_t start_time_ms;          // 过滤：开始时间（0表示不过滤）
    int64_t end_time_ms;            // 过滤：结束时间（0表示不过滤）
    size_t limit;                   // 限制返回数量（0表示不限制）
} kos_audit_query_t;

kos_audit_log_entry_t* kos_audit_query(
    kos_audit_system_t* audit,
    const kos_audit_query_t* query,
    size_t* result_count
);

void kos_audit_log_entry_free(kos_audit_log_entry_t* entry);
void kos_audit_log_entries_free(kos_audit_log_entry_t* entries, size_t count);

// ========== 统一安全上下文 ==========

// 安全上下文（结合 RBAC 和 ABAC）
typedef struct {
    kos_rbac_system_t* rbac;
    kos_abac_system_t* abac;
    kos_audit_system_t* audit;
} kos_security_context_t;

kos_security_context_t* kos_security_context_create(const char* audit_log_path);
void kos_security_context_free(kos_security_context_t* ctx);

// 统一权限检查（先检查 RBAC，再检查 ABAC）
bool kos_security_check_permission(
    kos_security_context_t* ctx,
    const char* user_id,
    const char* resource,
    kos_permission_type_t permission,
    const kos_attribute_t* context_attributes,
    size_t context_attribute_count,
    const char* ip_address
);

// ========== 辅助函数 ==========

// 权限类型转字符串
const char* kos_permission_type_to_string(kos_permission_type_t type);
kos_permission_type_t kos_permission_type_from_string(const char* str);

// 操作类型转字符串
const char* kos_audit_operation_to_string(kos_audit_operation_t op);
kos_audit_operation_t kos_audit_operation_from_string(const char* str);

// 获取当前时间戳（毫秒）
int64_t kos_get_current_timestamp_ms(void);

#endif // KOS_SECURITY_H
