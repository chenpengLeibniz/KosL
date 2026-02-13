#include "kos_security.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

// ========== 角色结构 ==========
struct kos_role {
    char* name;
    kos_permission_t* permissions;
    size_t permission_count;
    size_t permission_capacity;
};

// ========== RBAC 系统结构 ==========
struct kos_rbac_system {
    kos_user_t* users;
    size_t user_count;
    size_t user_capacity;
    
    kos_role_t** roles;  // 指针数组
    size_t role_count;
    size_t role_capacity;
};

// ========== RBAC 系统创建与销毁 ==========

kos_rbac_system_t* kos_rbac_create(void) {
    kos_rbac_system_t* rbac = (kos_rbac_system_t*)calloc(1, sizeof(kos_rbac_system_t));
    if (!rbac) {
        return NULL;
    }
    
    rbac->user_capacity = 16;
    rbac->role_capacity = 16;
    rbac->users = (kos_user_t*)calloc(rbac->user_capacity, sizeof(kos_user_t));
    rbac->roles = (kos_role_t**)calloc(rbac->role_capacity, sizeof(kos_role_t*));
    
    if (!rbac->users || !rbac->roles) {
        if (rbac->users) free(rbac->users);
        if (rbac->roles) free(rbac->roles);
        free(rbac);
        return NULL;
    }
    
    return rbac;
}

void kos_rbac_free(kos_rbac_system_t* rbac) {
    if (!rbac) {
        return;
    }
    
    // 释放所有用户
    for (size_t i = 0; i < rbac->user_count; i++) {
        kos_user_free(&rbac->users[i]);
    }
    free(rbac->users);
    
    // 释放所有角色
    for (size_t i = 0; i < rbac->role_count; i++) {
        kos_role_free(rbac->roles[i]);
    }
    free(rbac->roles);
    
    free(rbac);
}

// ========== 角色管理 ==========

kos_role_t* kos_create_role(
    const char* role_name,
    const kos_permission_t* permissions,
    size_t count
) {
    if (!role_name) {
        return NULL;
    }
    
    kos_role_t* role = (kos_role_t*)calloc(1, sizeof(kos_role_t));
    if (!role) {
        return NULL;
    }
    
    role->name = strdup(role_name);
    role->permission_capacity = count > 0 ? count : 8;
    role->permissions = (kos_permission_t*)calloc(role->permission_capacity, sizeof(kos_permission_t));
    
    if (!role->permissions) {
        free(role->name);
        free(role);
        return NULL;
    }
    
    // 复制权限
    for (size_t i = 0; i < count; i++) {
        role->permissions[i] = permissions[i];
        if (permissions[i].resource) {
            role->permissions[i].resource = strdup(permissions[i].resource);
        }
    }
    role->permission_count = count;
    
    return role;
}

void kos_role_free(kos_role_t* role) {
    if (!role) {
        return;
    }
    
    if (role->name) {
        free(role->name);
    }
    
    if (role->permissions) {
        for (size_t i = 0; i < role->permission_count; i++) {
            if (role->permissions[i].resource) {
                free((void*)role->permissions[i].resource);
            }
        }
        free(role->permissions);
    }
    
    free(role);
}

const char* kos_role_get_name(const kos_role_t* role) {
    return role ? role->name : NULL;
}

int kos_role_add_permission(kos_role_t* role, const kos_permission_t* permission) {
    if (!role || !permission) {
        return -1;
    }
    
    // 扩展权限数组（如果需要）
    if (role->permission_count >= role->permission_capacity) {
        size_t new_capacity = role->permission_capacity * 2;
        kos_permission_t* new_permissions = (kos_permission_t*)realloc(
            role->permissions,
            new_capacity * sizeof(kos_permission_t)
        );
        if (!new_permissions) {
            return -1;
        }
        role->permissions = new_permissions;
        role->permission_capacity = new_capacity;
    }
    
    // 添加权限
    role->permissions[role->permission_count] = *permission;
    if (permission->resource) {
        role->permissions[role->permission_count].resource = strdup(permission->resource);
    }
    role->permission_count++;
    
    return 0;
}

int kos_role_remove_permission(kos_role_t* role, kos_permission_type_t type, const char* resource) {
    if (!role) {
        return -1;
    }
    
    for (size_t i = 0; i < role->permission_count; i++) {
        if (role->permissions[i].type == type) {
            if (!resource || (role->permissions[i].resource && strcmp(role->permissions[i].resource, resource) == 0)) {
                // 释放资源字符串
                if (role->permissions[i].resource) {
                    free((void*)role->permissions[i].resource);
                }
                
                // 移动后续权限
                for (size_t j = i; j < role->permission_count - 1; j++) {
                    role->permissions[j] = role->permissions[j + 1];
                }
                role->permission_count--;
                return 0;
            }
        }
    }
    
    return -1; // 未找到
}

// ========== 用户管理 ==========

kos_user_t* kos_create_user(const char* user_id, const char* username) {
    if (!user_id) {
        return NULL;
    }
    
    kos_user_t* user = (kos_user_t*)calloc(1, sizeof(kos_user_t));
    if (!user) {
        return NULL;
    }
    
    user->user_id = strdup(user_id);
    user->username = username ? strdup(username) : NULL;
    user->roles = NULL;
    user->role_count = 0;
    user->attributes = NULL;
    user->attribute_count = 0;
    
    return user;
}

void kos_user_free(kos_user_t* user) {
    if (!user) {
        return;
    }
    
    if (user->user_id) free((void*)user->user_id);
    if (user->username) free((void*)user->username);
    
    if (user->roles) {
        free(user->roles);
    }
    
    if (user->attributes) {
        for (size_t i = 0; i < user->attribute_count; i++) {
            if (user->attributes[i]) free((void*)user->attributes[i]);
        }
        free(user->attributes);
    }
    
    free(user);
}

// ========== 角色分配 ==========

int kos_assign_role(
    kos_rbac_system_t* rbac,
    const char* user_id,
    const char* role_name
) {
    if (!rbac || !user_id || !role_name) {
        return -1;
    }
    
    // 查找用户
    kos_user_t* user = NULL;
    for (size_t i = 0; i < rbac->user_count; i++) {
        if (strcmp(rbac->users[i].user_id, user_id) == 0) {
            user = &rbac->users[i];
            break;
        }
    }
    
    if (!user) {
        // 创建新用户
        if (rbac->user_count >= rbac->user_capacity) {
            size_t new_capacity = rbac->user_capacity * 2;
            kos_user_t* new_users = (kos_user_t*)realloc(
                rbac->users,
                new_capacity * sizeof(kos_user_t)
            );
            if (!new_users) {
                return -1;
            }
            rbac->users = new_users;
            rbac->user_capacity = new_capacity;
        }
        
        user = &rbac->users[rbac->user_count];
        *user = *kos_create_user(user_id, NULL);
        rbac->user_count++;
    }
    
    // 查找角色
    kos_role_t* role = NULL;
    for (size_t i = 0; i < rbac->role_count; i++) {
        if (strcmp(rbac->roles[i]->name, role_name) == 0) {
            role = rbac->roles[i];
            break;
        }
    }
    
    if (!role) {
        return -1; // 角色不存在
    }
    
    // 检查是否已分配
    for (size_t i = 0; i < user->role_count; i++) {
        if (user->roles[i] == role) {
            return 0; // 已分配
        }
    }
    
    // 分配角色
    size_t new_capacity = user->role_count + 1;
    kos_role_t** new_roles = (kos_role_t**)realloc(
        user->roles,
        new_capacity * sizeof(kos_role_t*)
    );
    if (!new_roles) {
        return -1;
    }
    
    user->roles = new_roles;
    user->roles[user->role_count] = role;
    user->role_count++;
    
    return 0;
}

int kos_remove_role(
    kos_rbac_system_t* rbac,
    const char* user_id,
    const char* role_name
) {
    if (!rbac || !user_id || !role_name) {
        return -1;
    }
    
    // 查找用户
    kos_user_t* user = NULL;
    for (size_t i = 0; i < rbac->user_count; i++) {
        if (strcmp(rbac->users[i].user_id, user_id) == 0) {
            user = &rbac->users[i];
            break;
        }
    }
    
    if (!user) {
        return -1;
    }
    
    // 移除角色
    for (size_t i = 0; i < user->role_count; i++) {
        if (strcmp(user->roles[i]->name, role_name) == 0) {
            // 移动后续角色
            for (size_t j = i; j < user->role_count - 1; j++) {
                user->roles[j] = user->roles[j + 1];
            }
            user->role_count--;
            return 0;
        }
    }
    
    return -1; // 未找到角色
}

// ========== 权限检查 ==========

static bool match_resource(const char* pattern, const char* resource) {
    if (!pattern || !resource) {
        return false;
    }
    
    // 通配符 "*" 匹配所有资源
    if (strcmp(pattern, "*") == 0) {
        return true;
    }
    
    // 精确匹配
    return strcmp(pattern, resource) == 0;
}

bool kos_check_permission(
    kos_rbac_system_t* rbac,
    const char* user_id,
    const char* resource,
    kos_permission_type_t permission
) {
    if (!rbac || !user_id || !resource) {
        return false;
    }
    
    // 查找用户
    kos_user_t* user = NULL;
    for (size_t i = 0; i < rbac->user_count; i++) {
        if (strcmp(rbac->users[i].user_id, user_id) == 0) {
            user = &rbac->users[i];
            break;
        }
    }
    
    if (!user) {
        return false;
    }
    
    // 检查用户的所有角色
    for (size_t i = 0; i < user->role_count; i++) {
        kos_role_t* role = user->roles[i];
        if (!role) continue;
        
        // 检查角色的所有权限
        for (size_t j = 0; j < role->permission_count; j++) {
            kos_permission_t* perm = &role->permissions[j];
            
            // 检查权限类型和资源是否匹配
            if (perm->type == permission && match_resource(perm->resource, resource)) {
                return perm->allowed;
            }
        }
    }
    
    return false; // 默认拒绝
}

// ========== 用户和角色查询 ==========

kos_user_t* kos_get_user(kos_rbac_system_t* rbac, const char* user_id) {
    if (!rbac || !user_id) {
        return NULL;
    }
    
    for (size_t i = 0; i < rbac->user_count; i++) {
        if (strcmp(rbac->users[i].user_id, user_id) == 0) {
            return &rbac->users[i];
        }
    }
    
    return NULL;
}

kos_role_t* kos_get_role(kos_rbac_system_t* rbac, const char* role_name) {
    if (!rbac || !role_name) {
        return NULL;
    }
    
    for (size_t i = 0; i < rbac->role_count; i++) {
        if (strcmp(rbac->roles[i]->name, role_name) == 0) {
            return rbac->roles[i];
        }
    }
    
    return NULL;
}

// ========== 辅助函数：注册角色到系统 ==========

int kos_rbac_register_role(kos_rbac_system_t* rbac, kos_role_t* role) {
    if (!rbac || !role) {
        return -1;
    }
    
    // 检查是否已存在
    for (size_t i = 0; i < rbac->role_count; i++) {
        if (rbac->roles[i] == role) {
            return 0; // 已存在
        }
    }
    
    // 扩展角色数组
    if (rbac->role_count >= rbac->role_capacity) {
        size_t new_capacity = rbac->role_capacity * 2;
        kos_role_t** new_roles = (kos_role_t**)realloc(
            rbac->roles,
            new_capacity * sizeof(kos_role_t*)
        );
        if (!new_roles) {
            return -1;
        }
        rbac->roles = new_roles;
        rbac->role_capacity = new_capacity;
    }
    
    rbac->roles[rbac->role_count] = role;
    rbac->role_count++;
    
    return 0;
}
