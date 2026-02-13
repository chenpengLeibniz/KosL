// Phase 6: Access Control & Security Demo
// 权限与安全演示程序
#include "kos_security.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int main() {
    printf("=== KOS-TL Phase 6: Access Control & Security Demo ===\n\n");
    
    // 1. 创建安全上下文
    kos_security_context_t* security = kos_security_context_create("audit.log");
    if (!security) {
        fprintf(stderr, "Failed to create security context\n");
        return 1;
    }
    
    printf("Created security context with RBAC and audit logging\n\n");
    
    // 2. 创建角色和权限
    printf("=== Creating Roles and Permissions ===\n");
    
    // 管理员角色：所有权限
    kos_permission_t admin_perms[] = {
        {KOS_PERM_READ, "*", true},
        {KOS_PERM_WRITE, "*", true},
        {KOS_PERM_DELETE, "*", true},
        {KOS_PERM_QUERY, "*", true},
        {KOS_PERM_ADMIN, "*", true}
    };
    kos_role_t* admin_role = kos_create_role("admin", admin_perms, 5);
    kos_rbac_register_role(security->rbac, admin_role);
    printf("Created role: admin (all permissions)\n");
    
    // 读者角色：只有读权限
    kos_permission_t reader_perms[] = {
        {KOS_PERM_READ, "*", true},
        {KOS_PERM_QUERY, "*", true}
    };
    kos_role_t* reader_role = kos_create_role("reader", reader_perms, 2);
    kos_rbac_register_role(security->rbac, reader_role);
    printf("Created role: reader (read and query permissions)\n");
    
    // 编辑者角色：读写权限
    kos_permission_t editor_perms[] = {
        {KOS_PERM_READ, "*", true},
        {KOS_PERM_WRITE, "*", true},
        {KOS_PERM_QUERY, "*", true}
    };
    kos_role_t* editor_role = kos_create_role("editor", editor_perms, 3);
    kos_rbac_register_role(security->rbac, editor_role);
    printf("Created role: editor (read, write, and query permissions)\n\n");
    
    // 3. 创建用户并分配角色
    printf("=== Creating Users and Assigning Roles ===\n");
    
    kos_user_t* user1 = kos_create_user("user1", "Alice");
    kos_assign_role(security->rbac, "user1", "admin");
    printf("Created user: user1 (Alice) - assigned role: admin\n");
    
    kos_user_t* user2 = kos_create_user("user2", "Bob");
    kos_assign_role(security->rbac, "user2", "reader");
    printf("Created user: user2 (Bob) - assigned role: reader\n");
    
    kos_user_t* user3 = kos_create_user("user3", "Charlie");
    kos_assign_role(security->rbac, "user3", "editor");
    printf("Created user: user3 (Charlie) - assigned role: editor\n\n");
    
    // 4. 测试权限检查
    printf("=== Testing Permission Checks ===\n");
    
    const char* test_resource = "ontology/types";
    
    // user1 (admin) - 应该可以执行所有操作
    bool can_read = kos_check_permission(security->rbac, "user1", test_resource, KOS_PERM_READ);
    bool can_write = kos_check_permission(security->rbac, "user1", test_resource, KOS_PERM_WRITE);
    bool can_delete = kos_check_permission(security->rbac, "user1", test_resource, KOS_PERM_DELETE);
    printf("user1 (admin): READ=%s, WRITE=%s, DELETE=%s\n",
           can_read ? "ALLOWED" : "DENIED",
           can_write ? "ALLOWED" : "DENIED",
           can_delete ? "ALLOWED" : "DENIED");
    
    // user2 (reader) - 只能读和查询
    can_read = kos_check_permission(security->rbac, "user2", test_resource, KOS_PERM_READ);
    can_write = kos_check_permission(security->rbac, "user2", test_resource, KOS_PERM_WRITE);
    can_delete = kos_check_permission(security->rbac, "user2", test_resource, KOS_PERM_DELETE);
    printf("user2 (reader): READ=%s, WRITE=%s, DELETE=%s\n",
           can_read ? "ALLOWED" : "DENIED",
           can_write ? "ALLOWED" : "DENIED",
           can_delete ? "ALLOWED" : "DENIED");
    
    // user3 (editor) - 可以读写，但不能删除
    can_read = kos_check_permission(security->rbac, "user3", test_resource, KOS_PERM_READ);
    can_write = kos_check_permission(security->rbac, "user3", test_resource, KOS_PERM_WRITE);
    can_delete = kos_check_permission(security->rbac, "user3", test_resource, KOS_PERM_DELETE);
    printf("user3 (editor): READ=%s, WRITE=%s, DELETE=%s\n",
           can_read ? "ALLOWED" : "DENIED",
           can_write ? "ALLOWED" : "DENIED",
           can_delete ? "ALLOWED" : "DENIED");
    
    printf("\n");
    
    // 5. 测试统一权限检查（包含审计日志）
    printf("=== Testing Unified Permission Check with Audit Logging ===\n");
    
    bool allowed1 = kos_security_check_permission(
        security, "user1", "ontology/types", KOS_PERM_WRITE,
        NULL, 0, "192.168.1.100"
    );
    printf("user1 write permission: %s\n", allowed1 ? "ALLOWED" : "DENIED");
    
    bool allowed2 = kos_security_check_permission(
        security, "user2", "ontology/types", KOS_PERM_WRITE,
        NULL, 0, "192.168.1.101"
    );
    printf("user2 write permission: %s\n", allowed2 ? "ALLOWED" : "DENIED");
    
    printf("\n");
    
    // 6. 查询审计日志
    printf("=== Querying Audit Logs ===\n");
    
    kos_audit_query_t query = {0};
    query.user_id = "user1";
    query.limit = 10;
    
    size_t log_count = 0;
    kos_audit_log_entry_t* logs = kos_audit_query(security->audit, &query, &log_count);
    
    if (logs) {
        printf("Found %zu audit log entries for user1:\n", log_count);
        for (size_t i = 0; i < log_count; i++) {
            printf("  [%lld] %s: %s on %s - %s\n",
                   (long long)logs[i].timestamp_ms,
                   logs[i].user_id ? logs[i].user_id : "unknown",
                   kos_audit_operation_to_string(logs[i].operation),
                   logs[i].resource ? logs[i].resource : "unknown",
                   logs[i].success ? "SUCCESS" : "FAILED");
        }
        kos_audit_log_entries_free(logs, log_count);
    } else {
        printf("No audit logs found\n");
    }
    
    printf("\n");
    
    // 7. 清理资源
    kos_user_free(user1);
    kos_user_free(user2);
    kos_user_free(user3);
    kos_security_context_free(security);
    
    printf("=== Demo Completed ===\n");
    printf("Audit logs have been written to: audit.log\n");
    
    return 0;
}
