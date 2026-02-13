#include "kos_security.h"
#include <stdlib.h>
#include <string.h>

// ========== 统一安全上下文 ==========

kos_security_context_t* kos_security_context_create(const char* audit_log_path) {
    kos_security_context_t* ctx = (kos_security_context_t*)calloc(1, sizeof(kos_security_context_t));
    if (!ctx) {
        return NULL;
    }
    
    ctx->rbac = kos_rbac_create();
    ctx->abac = NULL; // ABAC 待实现
    ctx->audit = kos_audit_create(audit_log_path);
    
    if (!ctx->rbac || !ctx->audit) {
        if (ctx->rbac) kos_rbac_free(ctx->rbac);
        if (ctx->audit) kos_audit_free(ctx->audit);
        free(ctx);
        return NULL;
    }
    
    return ctx;
}

void kos_security_context_free(kos_security_context_t* ctx) {
    if (!ctx) {
        return;
    }
    
    if (ctx->rbac) {
        kos_rbac_free(ctx->rbac);
    }
    
    if (ctx->abac) {
        // kos_abac_free(ctx->abac); // 待实现
    }
    
    if (ctx->audit) {
        kos_audit_free(ctx->audit);
    }
    
    free(ctx);
}

bool kos_security_check_permission(
    kos_security_context_t* ctx,
    const char* user_id,
    const char* resource,
    kos_permission_type_t permission,
    const kos_attribute_t* context_attributes,
    size_t context_attribute_count,
    const char* ip_address
) {
    if (!ctx || !user_id || !resource) {
        return false;
    }
    
    // 先检查 RBAC
    bool rbac_allowed = kos_check_permission(ctx->rbac, user_id, resource, permission);
    
    // 记录审计日志
    kos_audit_log(
        ctx->audit,
        user_id,
        rbac_allowed ? KOS_AUDIT_OP_READ : KOS_AUDIT_OP_PERMISSION_DENIED,
        resource,
        NULL,
        rbac_allowed,
        ip_address
    );
    
    // TODO: 检查 ABAC（待实现）
    
    return rbac_allowed;
}
