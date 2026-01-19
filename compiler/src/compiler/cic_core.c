// compiler/src/compiler/cic_core.c
// Calculus of Inductive Constructions (CIC) 内核实现
// 基于双向类型检查算法和归纳类型系统

#include "compiler/cic_core.h"
#include "../../include/kos_core.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

// strdup 的跨平台实现
#ifdef _WIN32
#define strdup _strdup
#else
#ifndef _POSIX_C_SOURCE
#define _POSIX_C_SOURCE 200809L
#endif
#endif

// ========== 辅助函数 ==========

static void cic_error_internal(cic_core* core, const char* message) {
    if (!core) {
        return;
    }
    
    core->has_error = true;
    if (core->error_message) {
        free(core->error_message);
    }
    core->error_message = strdup(message);
}

// ========== CIC 内核创建和释放 ==========

cic_core* cic_core_create(void) {
    cic_core* core = (cic_core*)calloc(1, sizeof(cic_core));
    if (!core) {
        return NULL;
    }
    
    core->ctx = cic_context_create();
    if (!core->ctx) {
        free(core);
        return NULL;
    }
    
    core->has_error = false;
    core->error_message = NULL;
    core->error_node = NULL;
    core->max_universe_level = 10; // 默认最大层级
    
    return core;
}

void cic_core_free(cic_core* core) {
    if (!core) {
        return;
    }
    
    if (core->ctx) {
        cic_context_free(core->ctx);
    }
    
    if (core->error_message) {
        free(core->error_message);
    }
    
    free(core);
}

// ========== 环境操作 ==========

cic_context* cic_context_create(void) {
    cic_context* ctx = (cic_context*)calloc(1, sizeof(cic_context));
    if (!ctx) {
        return NULL;
    }
    
    ctx->head = NULL;
    ctx->length = 0;
    
    return ctx;
}

void cic_context_free(cic_context* ctx) {
    if (!ctx) {
        return;
    }
    
    cic_entry* entry = ctx->head;
    while (entry) {
        cic_entry* next = entry->next;
        
        if (entry->name) {
            free(entry->name);
        }
        if (entry->type) {
            kos_term_free(entry->type);
        }
        if (entry->definition) {
            kos_term_free(entry->definition);
        }
        if (entry->inductive) {
            cic_inductive_free(entry->inductive);
        }
        
        free(entry);
        entry = next;
    }
    
    free(ctx);
}

cic_context* cic_context_add_var(cic_context* ctx, const char* name, kos_term* type) {
    if (!ctx || !name || !type) {
        return NULL;
    }
    
    cic_entry* entry = (cic_entry*)calloc(1, sizeof(cic_entry));
    if (!entry) {
        return NULL;
    }
    
    entry->kind = CIC_ENTRY_VAR;
    entry->name = strdup(name);
    entry->type = kos_term_copy(type);
    entry->definition = NULL;
    entry->inductive = NULL;
    entry->next = ctx->head;
    
    ctx->head = entry;
    ctx->length++;
    
    return ctx;
}

cic_context* cic_context_add_def(cic_context* ctx, const char* name, kos_term* type, kos_term* definition) {
    if (!ctx || !name || !type || !definition) {
        return NULL;
    }
    
    cic_entry* entry = (cic_entry*)calloc(1, sizeof(cic_entry));
    if (!entry) {
        return NULL;
    }
    
    entry->kind = CIC_ENTRY_DEF;
    entry->name = strdup(name);
    entry->type = kos_term_copy(type);
    entry->definition = kos_term_copy(definition);
    entry->inductive = NULL;
    entry->next = ctx->head;
    
    ctx->head = entry;
    ctx->length++;
    
    return ctx;
}

cic_context* cic_context_add_inductive(cic_context* ctx, cic_inductive_def* inductive) {
    if (!ctx || !inductive) {
        return NULL;
    }
    
    cic_entry* entry = (cic_entry*)calloc(1, sizeof(cic_entry));
    if (!entry) {
        return NULL;
    }
    
    entry->kind = CIC_ENTRY_INDUCTIVE;
    entry->name = strdup(inductive->name);
    entry->type = kos_term_copy(inductive->type);
    entry->definition = NULL;
    entry->inductive = inductive; // 注意：不复制，共享引用
    entry->next = ctx->head;
    
    ctx->head = entry;
    ctx->length++;
    
    return ctx;
}

cic_entry* cic_context_lookup(cic_context* ctx, const char* name) {
    if (!ctx || !name) {
        return NULL;
    }
    
    cic_entry* entry = ctx->head;
    while (entry) {
        if (entry->name && strcmp(entry->name, name) == 0) {
            return entry;
        }
        entry = entry->next;
    }
    
    return NULL;
}

// ========== 类型检查（双向算法） ==========

bool cic_check(cic_core* core, kos_term* term, kos_term* type) {
    if (!core || !term || !type) {
        return false;
    }
    
    // 简化：先推断项的类型，然后检查是否相等
    kos_term* inferred_type = cic_infer(core, term);
    if (!inferred_type) {
        return false;
    }
    
    bool equal = cic_types_equal(core, inferred_type, type);
    kos_term_free(inferred_type);
    
    return equal;
}

kos_term* cic_infer(cic_core* core, kos_term* term) {
    if (!core || !term) {
        return NULL;
    }
    
    // TODO: 实现完整的类型推断算法
    // 这里先返回一个占位实现
    cic_error_internal(core, "Type inference not yet implemented");
    return NULL;
}

bool cic_check_sort(cic_core* core, kos_term* type) {
    if (!core || !type) {
        return false;
    }
    
    // 检查是否为Sort类型（U_i, Type_i, Prop）
    return (type->kind == KOS_U || type->kind == KOS_TYPE || type->kind == KOS_PROP);
}

bool cic_types_equal(cic_core* core, kos_term* type1, kos_term* type2) {
    if (!core || !type1 || !type2) {
        return false;
    }
    
    // 归约到范式后比较
    kos_term* nf1 = cic_nf(core, type1);
    kos_term* nf2 = cic_nf(core, type2);
    
    // TODO: 实现结构相等的比较
    // 这里先做简单的指针比较（实际应该做深度比较）
    bool equal = (nf1 == nf2);
    
    if (nf1 != type1) kos_term_free(nf1);
    if (nf2 != type2) kos_term_free(nf2);
    
    return equal;
}

// ========== 归约（Reduction） ==========

kos_term* cic_beta_reduce(cic_core* core, kos_term* lambda, kos_term* argument) {
    if (!core || !lambda || !argument) {
        return NULL;
    }
    
    // TODO: 实现β-归约
    // (λx:A.M) N → M[x := N]
    cic_error_internal(core, "Beta reduction not yet implemented");
    return NULL;
}

kos_term* cic_iota_reduce(cic_core* core, kos_term* match_expr) {
    if (!core || !match_expr) {
        return NULL;
    }
    
    // TODO: 实现ι-归约（模式匹配归约）
    cic_error_internal(core, "Iota reduction not yet implemented");
    return NULL;
}

kos_term* cic_whnf(cic_core* core, kos_term* term) {
    if (!core || !term) {
        return NULL;
    }
    
    // TODO: 实现弱头范式归约
    // 这里先返回原项
    return kos_term_copy(term);
}

kos_term* cic_nf(cic_core* core, kos_term* term) {
    if (!core || !term) {
        return NULL;
    }
    
    // TODO: 实现范式归约
    // 这里先返回原项
    return kos_term_copy(term);
}

// ========== 归纳类型操作 ==========

cic_inductive_def* cic_inductive_create(const char* name, kos_term* type) {
    if (!name || !type) {
        return NULL;
    }
    
    cic_inductive_def* inductive = (cic_inductive_def*)calloc(1, sizeof(cic_inductive_def));
    if (!inductive) {
        return NULL;
    }
    
    inductive->name = strdup(name);
    inductive->type = kos_term_copy(type);
    inductive->parameters = NULL;
    inductive->constructors = NULL;
    inductive->constructor_count = 0;
    inductive->constructor_capacity = 0;
    inductive->is_positive = false;
    
    return inductive;
}

void cic_inductive_free(cic_inductive_def* inductive) {
    if (!inductive) {
        return;
    }
    
    if (inductive->name) {
        free(inductive->name);
    }
    if (inductive->type) {
        kos_term_free(inductive->type);
    }
    if (inductive->parameters) {
        kos_term_free(inductive->parameters);
    }
    if (inductive->constructors) {
        for (size_t i = 0; i < inductive->constructor_count; i++) {
            if (inductive->constructors[i].name) {
                free(inductive->constructors[i].name);
            }
            if (inductive->constructors[i].type) {
                kos_term_free(inductive->constructors[i].type);
            }
        }
        free(inductive->constructors);
    }
    
    free(inductive);
}

bool cic_inductive_add_constructor(cic_inductive_def* inductive, const char* name, kos_term* type) {
    if (!inductive || !name || !type) {
        return false;
    }
    
    // 检查是否需要扩容
    if (inductive->constructor_count >= inductive->constructor_capacity) {
        size_t new_capacity = inductive->constructor_capacity == 0 ? 4 : inductive->constructor_capacity * 2;
        cic_constructor* new_constructors = (cic_constructor*)realloc(
            inductive->constructors, 
            new_capacity * sizeof(cic_constructor)
        );
        if (!new_constructors) {
            return false;
        }
        inductive->constructors = new_constructors;
        inductive->constructor_capacity = new_capacity;
    }
    
    // 添加构造子
    cic_constructor* ctor = &inductive->constructors[inductive->constructor_count];
    ctor->name = strdup(name);
    ctor->type = kos_term_copy(type);
    ctor->arity = 0; // TODO: 计算arity
    inductive->constructor_count++;
    
    return true;
}

bool cic_check_inductive(cic_core* core, cic_inductive_def* inductive) {
    if (!core || !inductive) {
        return false;
    }
    
    // TODO: 实现归纳类型检查
    // 1. 检查类型是否为Sort
    // 2. 检查构造子类型的合法性
    // 3. 检查严格正性（strict positivity）
    
    return true;
}

// ========== 模式匹配 ==========

cic_case_expr* cic_case_create(kos_term* matched_term, kos_term* matched_type) {
    if (!matched_term || !matched_type) {
        return NULL;
    }
    
    cic_case_expr* case_expr = (cic_case_expr*)calloc(1, sizeof(cic_case_expr));
    if (!case_expr) {
        return NULL;
    }
    
    case_expr->matched_term = kos_term_copy(matched_term);
    case_expr->matched_type = kos_term_copy(matched_type);
    case_expr->cases = NULL;
    case_expr->case_count = 0;
    
    return case_expr;
}

bool cic_case_add_branch(cic_case_expr* case_expr, const char* constructor_name, 
                         const char** pattern_vars, size_t var_count, kos_term* body) {
    if (!case_expr || !constructor_name || !body) {
        return false;
    }
    
    // TODO: 实现添加Case分支
    return true;
}

kos_term* cic_check_case(cic_core* core, cic_case_expr* case_expr, kos_term* expected_type) {
    if (!core || !case_expr || !expected_type) {
        return NULL;
    }
    
    // TODO: 实现Case表达式类型检查
    return NULL;
}

// ========== 递归定义 ==========

kos_term* cic_mk_fix(const char* name, kos_term* type, kos_term* body) {
    // TODO: 实现Fix表达式创建
    return NULL;
}

kos_term* cic_mk_cofix(const char* name, kos_term* type, kos_term* body) {
    // TODO: 实现CoFix表达式创建
    return NULL;
}

bool cic_check_fix(cic_core* core, kos_term* fix_term) {
    if (!core || !fix_term) {
        return false;
    }
    
    // TODO: 实现递归定义检查（严格正性）
    return true;
}

// ========== 错误处理 ==========

bool cic_has_error(cic_core* core) {
    return core && core->has_error;
}

const char* cic_get_error(cic_core* core) {
    if (!core || !core->has_error) {
        return NULL;
    }
    return core->error_message;
}

void cic_error(cic_core* core, ASTNode* node, const char* message) {
    if (!core) {
        return;
    }
    
    cic_error_internal(core, message);
    core->error_node = node;
}

// ========== 占位实现 ==========

kos_term* cic_mk_constructor_app(cic_core* core, const char* constructor_name, kos_term** args, size_t arg_count) {
    // TODO: 实现构造子应用
    return NULL;
}

bool cic_check_universe(cic_core* core, kos_term* type, int min_level) {
    // TODO: 实现Universe层级检查
    return true;
}

int cic_universe_level(cic_core* core, kos_term* type) {
    // TODO: 实现Universe层级计算
    return 1;
}

kos_term* cic_ast_to_term(cic_core* core, ASTNode* ast) {
    // TODO: 实现AST到CIC术语的转换
    return NULL;
}

ASTNode* cic_term_to_ast(cic_core* core, kos_term* term) {
    // TODO: 实现CIC术语到AST的转换
    return NULL;
}
