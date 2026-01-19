// compiler/src/compiler/type_checker.c
// KOS-TL 类型检查器实现

#include "compiler/type_checker.h"
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

// ========== 类型环境实现 ==========

TypeEnv* type_env_create(void) {
    TypeEnv* env = (TypeEnv*)calloc(1, sizeof(TypeEnv));
    if (!env) {
        return NULL;
    }
    
    env->capacity = 16;
    env->count = 0;
    env->entries = (TypeEnvEntry*)calloc(env->capacity, sizeof(TypeEnvEntry));
    if (!env->entries) {
        free(env);
        return NULL;
    }
    
    return env;
}

void type_env_free(TypeEnv* env) {
    if (!env) {
        return;
    }
    
    if (env->entries) {
        for (size_t i = 0; i < env->count; i++) {
            if (env->entries[i].name) {
                free(env->entries[i].name);
            }
            if (env->entries[i].type) {
                kos_term_free(env->entries[i].type);
            }
        }
        free(env->entries);
    }
    
    free(env);
}

bool type_env_add(TypeEnv* env, const char* name, kos_term* type, bool is_type_var) {
    if (!env || !name) {
        return false;
    }
    
    // 检查是否需要扩容
    if (env->count >= env->capacity) {
        size_t new_capacity = env->capacity * 2;
        TypeEnvEntry* new_entries = (TypeEnvEntry*)realloc(env->entries, new_capacity * sizeof(TypeEnvEntry));
        if (!new_entries) {
            return false;
        }
        env->entries = new_entries;
        env->capacity = new_capacity;
    }
    
    // 添加新条目
    env->entries[env->count].name = strdup(name);
    env->entries[env->count].type = type ? kos_term_copy(type) : NULL;
    env->entries[env->count].is_type_var = is_type_var;
    env->count++;
    
    return true;
}

TypeEnvEntry* type_env_lookup(TypeEnv* env, const char* name) {
    if (!env || !name) {
        return NULL;
    }
    
    // 从后往前查找（支持遮蔽）
    for (int i = (int)env->count - 1; i >= 0; i--) {
        if (env->entries[i].name && strcmp(env->entries[i].name, name) == 0) {
            return &env->entries[i];
        }
    }
    
    return NULL;
}

TypeEnv* type_env_push_scope(TypeEnv* env) {
    // 简单实现：返回同一个环境（实际应该创建新作用域）
    // TODO: 实现真正的作用域栈
    return env;
}

void type_env_pop_scope(TypeEnv* env) {
    // TODO: 实现作用域弹出
    (void)env;
}

// ========== 类型检查器创建和释放 ==========

TypeChecker* type_checker_create(void) {
    TypeChecker* checker = (TypeChecker*)calloc(1, sizeof(TypeChecker));
    if (!checker) {
        return NULL;
    }
    
    checker->env = type_env_create();
    if (!checker->env) {
        free(checker);
        return NULL;
    }
    
    checker->has_error = false;
    checker->error_message = NULL;
    checker->error_node = NULL;
    checker->universe_level = 1; // 默认从U₁开始
    
    return checker;
}

void type_checker_free(TypeChecker* checker) {
    if (!checker) {
        return;
    }
    
    if (checker->env) {
        type_env_free(checker->env);
    }
    
    if (checker->error_message) {
        free(checker->error_message);
    }
    
    free(checker);
}

// ========== 错误处理 ==========

void type_checker_error(TypeChecker* checker, ASTNode* node, const char* message) {
    if (!checker) {
        return;
    }
    
    checker->has_error = true;
    checker->error_node = node;
    
    if (checker->error_message) {
        free(checker->error_message);
    }
    
    // 构建错误消息（包含位置信息）
    if (node && node->token) {
        char* error_msg = (char*)malloc(512);
        if (error_msg) {
            snprintf(error_msg, 512, "%s at line %d, column %d",
                    message,
                    node->token->line,
                    node->token->column);
            checker->error_message = error_msg;
        } else {
            checker->error_message = strdup(message);
        }
    } else {
        checker->error_message = strdup(message);
    }
}

bool type_checker_has_error(TypeChecker* checker) {
    return checker && checker->has_error;
}

const char* type_checker_get_error(TypeChecker* checker) {
    if (!checker || !checker->has_error) {
        return NULL;
    }
    return checker->error_message;
}

// ========== 辅助函数：创建基础类型 ==========

kos_term* type_checker_mk_universe(TypeChecker* checker, bool is_computational, int level) {
    (void)checker; // 暂时未使用
    if (is_computational) {
        return kos_mk_universe_computational(level);
    } else {
        return kos_mk_universe_logical(level);
    }
}

kos_term* type_checker_mk_prop(TypeChecker* checker) {
    (void)checker;
    // Prop : Type₁
    return kos_mk_prop("Prop");
}

kos_term* type_checker_mk_id(TypeChecker* checker, const char* name) {
    (void)checker;
    return kos_mk_id(name);
}

kos_term* type_checker_mk_val(TypeChecker* checker, const char* value) {
    (void)checker;
    return kos_mk_val(value);
}

kos_term* type_checker_mk_pi_type(TypeChecker* checker, const char* param_name, kos_term* param_type, kos_term* body_type) {
    (void)checker;
    (void)param_name; // TODO: 处理依赖类型中的参数名
    return kos_mk_pi(param_type, body_type);
}

kos_term* type_checker_mk_sigma_type(TypeChecker* checker, const char* first_name, kos_term* first_type, kos_term* second_type) {
    (void)checker;
    (void)first_name; // TODO: 处理依赖类型中的参数名
    return kos_mk_sigma(first_type, second_type);
}

kos_term* type_checker_mk_function_type(TypeChecker* checker, kos_term* domain, kos_term* codomain) {
    // 非依赖函数类型 A → B 等价于 Π(x:A).B（其中B不依赖x）
    return type_checker_mk_pi_type(checker, NULL, domain, codomain);
}

// ========== AST 到 kos_term 类型转换 ==========

kos_term* type_checker_ast_to_type(TypeChecker* checker, ASTNode* type_ast) {
    if (!checker || !type_ast) {
        return NULL;
    }
    
    switch (type_ast->kind) {
        case AST_UNIVERSE: {
            // 解析 U₁, Type₁ 等
            // TODO: 解析下标
            return type_checker_mk_universe(checker, true, 1);
        }
        
        case AST_PROP: {
            return type_checker_mk_prop(checker);
        }
        
        case AST_VARIABLE: {
            // 查找类型变量
            TypeEnvEntry* entry = type_env_lookup(checker->env, type_ast->name);
            if (entry && entry->type) {
                return kos_term_copy(entry->type);
            }
            type_checker_error(checker, type_ast, "Undefined type variable");
            return NULL;
        }
        
        case AST_FUNCTION_TYPE: {
            kos_term* domain = type_checker_ast_to_type(checker, type_ast->function);
            kos_term* codomain = type_checker_ast_to_type(checker, type_ast->argument);
            if (!domain || !codomain) {
                if (domain) kos_term_free(domain);
                if (codomain) kos_term_free(codomain);
                return NULL;
            }
            kos_term* func_type = type_checker_mk_function_type(checker, domain, codomain);
            kos_term_free(domain);
            kos_term_free(codomain);
            return func_type;
        }
        
        case AST_PI_TYPE: {
            kos_term* param_type = type_checker_ast_to_type(checker, type_ast->param_type);
            if (!param_type) {
                return NULL;
            }
            
            // 添加参数到环境
            type_env_add(checker->env, type_ast->param_name, param_type, false);
            
            kos_term* body_type = type_checker_ast_to_type(checker, type_ast->lambda_body);
            if (!body_type) {
                kos_term_free(param_type);
                return NULL;
            }
            
            kos_term* pi_type = type_checker_mk_pi_type(checker, type_ast->param_name, param_type, body_type);
            kos_term_free(param_type);
            kos_term_free(body_type);
            return pi_type;
        }
        
        case AST_SIGMA_TYPE: {
            kos_term* first_type = type_checker_ast_to_type(checker, type_ast->first_type);
            if (!first_type) {
                return NULL;
            }
            
            // 添加第一个组件到环境
            type_env_add(checker->env, type_ast->first_name, first_type, false);
            
            kos_term* second_type = type_checker_ast_to_type(checker, type_ast->sigma_second_type);
            if (!second_type) {
                kos_term_free(first_type);
                return NULL;
            }
            
            kos_term* sigma_type = type_checker_mk_sigma_type(checker, type_ast->first_name, first_type, second_type);
            kos_term_free(first_type);
            kos_term_free(second_type);
            return sigma_type;
        }
        
        default:
            type_checker_error(checker, type_ast, "Invalid type expression");
            return NULL;
    }
}

// ========== 类型检查 ==========

bool type_checker_types_equal(TypeChecker* checker, kos_term* type1, kos_term* type2) {
    if (!type1 || !type2) {
        return false;
    }
    
    // 归约两个类型
    kos_term* reduced1 = kos_reduce(type1);
    kos_term* reduced2 = kos_reduce(type2);
    
    // 检查kind是否相同
    if (reduced1->kind != reduced2->kind) {
        if (reduced1 != type1) kos_term_free(reduced1);
        if (reduced2 != type2) kos_term_free(reduced2);
        return false;
    }
    
    // 根据kind进行详细比较
    bool result = false;
    switch (reduced1->kind) {
        case KOS_VAL:
        case KOS_TIME:
        case KOS_ID:
            // 基本类型：比较值
            if (reduced1->data.atomic.val && reduced2->data.atomic.val) {
                result = (strcmp(reduced1->data.atomic.val, reduced2->data.atomic.val) == 0);
            } else {
                result = (reduced1->data.atomic.val == reduced2->data.atomic.val);
            }
            break;
            
        case KOS_PROP:
            if (reduced1->data.atomic.val && reduced2->data.atomic.val) {
                result = (strcmp(reduced1->data.atomic.val, reduced2->data.atomic.val) == 0);
            } else {
                result = (reduced1->data.atomic.val == reduced2->data.atomic.val);
            }
            break;
            
        case KOS_U:
        case KOS_TYPE:
            result = (reduced1->data.universe.axis == reduced2->data.universe.axis &&
                     reduced1->data.universe.level == reduced2->data.universe.level);
            break;
            
        case KOS_PI:
            // 递归检查domain和body
            result = type_checker_types_equal(checker, reduced1->data.pi.domain, reduced2->data.pi.domain) &&
                     type_checker_types_equal(checker, reduced1->data.pi.body, reduced2->data.pi.body);
            break;
            
        case KOS_SIGMA:
            result = type_checker_types_equal(checker, reduced1->data.sigma.domain, reduced2->data.sigma.domain) &&
                     type_checker_types_equal(checker, reduced1->data.sigma.body, reduced2->data.sigma.body);
            break;
            
        default:
            result = true; // 其他类型暂时认为相等
            break;
    }
    
    if (reduced1 != type1) kos_term_free(reduced1);
    if (reduced2 != type2) kos_term_free(reduced2);
    return result;
}

bool type_checker_types_compatible(TypeChecker* checker, kos_term* subtype, kos_term* supertype) {
    if (!subtype || !supertype) {
        return false;
    }
    
    // 首先检查类型相等
    if (type_checker_types_equal(checker, subtype, supertype)) {
        return true;
    }
    
    // 检查Universe层级关系
    universe_info sub_info = kos_get_universe_info(subtype);
    universe_info super_info = kos_get_universe_info(supertype);
    
    return kos_universe_leq(sub_info, super_info);
}

bool type_checker_check_universe_level(TypeChecker* checker, kos_term* type, int min_level) {
    if (!type) {
        return false;
    }
    
    universe_info info = kos_get_universe_info(type);
    
    // 检查层级是否满足要求
    if (info.axis == UNIVERSE_COMPUTATIONAL) {
        return info.level >= min_level;
    } else if (info.axis == UNIVERSE_LOGICAL) {
        // Type_i 的层级要求
        return info.level >= min_level;
    }
    
    return true;
}

kos_term* type_checker_lift_universe(TypeChecker* checker, kos_term* type) {
    if (!type) {
        return NULL;
    }
    
    universe_info info = kos_get_universe_info(type);
    
    // 如果是计算轴，尝试提升到逻辑轴
    if (info.axis == UNIVERSE_COMPUTATIONAL) {
        return kos_universe_lift_to_logic(type);
    }
    
    return kos_term_copy(type);
}

// ========== AST 到 kos_term 表达式转换 ==========

kos_term* type_checker_ast_to_term(TypeChecker* checker, ASTNode* expr_ast, kos_term* expected_type) {
    if (!checker || !expr_ast) {
        return NULL;
    }
    
    switch (expr_ast->kind) {
        case AST_VARIABLE: {
            // 查找变量
            TypeEnvEntry* entry = type_env_lookup(checker->env, expr_ast->name);
            if (!entry || !entry->type) {
                type_checker_error(checker, expr_ast, "Undefined variable");
                return NULL;
            }
            
            kos_term* var_type = kos_term_copy(entry->type);
            
            // 如果有期望类型，检查兼容性
            if (expected_type) {
                if (!type_checker_types_compatible(checker, var_type, expected_type)) {
                    kos_term_free(var_type);
                    type_checker_error(checker, expr_ast, "Type mismatch");
                    return NULL;
                }
            }
            
            // 创建变量项（使用ID类型）
            kos_term* var_term = kos_mk_id(expr_ast->name);
            return var_term;
        }
        
        case AST_LITERAL: {
            // 字面量
            if (expr_ast->value) {
                kos_term* literal = kos_mk_val(expr_ast->value);
                
                // 检查类型兼容性
                if (expected_type) {
                    if (!kos_check(NULL, literal, expected_type)) {
                        kos_term_free(literal);
                        type_checker_error(checker, expr_ast, "Literal type mismatch");
                        return NULL;
                    }
                }
                
                return literal;
            }
            type_checker_error(checker, expr_ast, "Invalid literal");
            return NULL;
        }
        
        case AST_LAMBDA: {
            // λ表达式
            if (!expr_ast->param_name || !expr_ast->lambda_body) {
                type_checker_error(checker, expr_ast, "Invalid lambda expression");
                return NULL;
            }
            
            // 检查参数类型
            kos_term* param_type = NULL;
            if (expr_ast->param_type) {
                param_type = type_checker_ast_to_type(checker, expr_ast->param_type);
                if (!param_type) {
                    return NULL;
                }
            } else {
                type_checker_error(checker, expr_ast, "Lambda parameter type required");
                return NULL;
            }
            
            // 添加参数到环境
            type_env_add(checker->env, expr_ast->param_name, param_type, false);
            
            // 检查函数体类型
            kos_term* body_type = NULL;
            if (expected_type && expected_type->kind == KOS_PI) {
                body_type = kos_term_copy(expected_type->data.pi.body);
            }
            
            kos_term* body_term = type_checker_ast_to_term(checker, expr_ast->lambda_body, body_type);
            if (body_type) {
                kos_term_free(body_type);
                body_type = NULL;
            }
            
            if (!body_term) {
                kos_term_free(param_type);
                return NULL;
            }
            
            // 推断body的类型（如果还没有推断）
            if (!body_type) {
                body_type = type_checker_infer_expr(checker, expr_ast->lambda_body);
                if (!body_type) {
                    kos_term_free(param_type);
                    return NULL;
                }
            }
            
            // 创建Π类型
            kos_term* pi_type = kos_mk_pi(param_type, body_type);
            
            // 检查类型兼容性
            if (expected_type) {
                if (!type_checker_types_compatible(checker, pi_type, expected_type)) {
                    kos_term_free(pi_type);
                    kos_term_free(param_type);
                    kos_term_free(body_type);
                    type_checker_error(checker, expr_ast, "Lambda type mismatch");
                    return NULL;
                }
            }
            
            // 创建λ抽象
            kos_term* lambda_term = kos_mk_lambda(param_type, body_term);
            
            kos_term_free(pi_type);
            kos_term_free(param_type);
            kos_term_free(body_type);
            return lambda_term;
        }
        
        case AST_APPLICATION: {
            // 函数应用
            if (!expr_ast->function || !expr_ast->argument) {
                type_checker_error(checker, expr_ast, "Invalid function application");
                return NULL;
            }
            
            // 推断函数类型
            kos_term* func_term = type_checker_infer_expr(checker, expr_ast->function);
            if (!func_term) {
                return NULL;
            }
            
            // 检查是否为Π类型
            if (func_term->kind != KOS_PI) {
                kos_term_free(func_term);
                type_checker_error(checker, expr_ast, "Cannot apply non-function");
                return NULL;
            }
            
            // 检查参数类型
            kos_term* arg_term = type_checker_ast_to_term(checker, expr_ast->argument, func_term->data.pi.domain);
            if (!arg_term) {
                kos_term_free(func_term);
                return NULL;
            }
            
            // 创建函数应用
            kos_term* app_term = kos_mk_app(func_term, arg_term);
            
            // 应用后的类型是body类型（依赖类型需要替换）
            kos_term* result_type = kos_term_copy(func_term->data.pi.body);
            
            // 检查期望类型
            if (expected_type) {
                if (!type_checker_types_compatible(checker, result_type, expected_type)) {
                    kos_term_free(app_term);
                    kos_term_free(result_type);
                    type_checker_error(checker, expr_ast, "Application result type mismatch");
                    return NULL;
                }
            }
            
            kos_term_free(func_term);
            kos_term_free(result_type);
            return app_term;
        }
        
        case AST_TYPE_ANNOTATION: {
            // 类型注解 e : T
            if (!expr_ast->function || !expr_ast->argument) {
                type_checker_error(checker, expr_ast, "Invalid type annotation");
                return NULL;
            }
            
            // 解析类型
            kos_term* annot_type = type_checker_ast_to_type(checker, expr_ast->argument);
            if (!annot_type) {
                return NULL;
            }
            
            // 检查表达式是否符合注解类型
            kos_term* term = type_checker_ast_to_term(checker, expr_ast->function, annot_type);
            if (!term) {
                kos_term_free(annot_type);
                return NULL;
            }
            
            kos_term_free(annot_type);
            return term;
        }
        
        case AST_LET: {
            // let x = e1 in e2
            if (!expr_ast->var_name || !expr_ast->var_value || !expr_ast->let_body) {
                type_checker_error(checker, expr_ast, "Invalid let expression");
                return NULL;
            }
            
            // 推断e1的类型
            kos_term* value_type = type_checker_infer_expr(checker, expr_ast->var_value);
            if (!value_type) {
                return NULL;
            }
            
            // 添加变量到环境
            type_env_add(checker->env, expr_ast->var_name, value_type, false);
            
            // 检查e2
            kos_term* body_term = type_checker_ast_to_term(checker, expr_ast->let_body, expected_type);
            if (!body_term) {
                kos_term_free(value_type);
                return NULL;
            }
            
            kos_term_free(value_type);
            return body_term;
        }
        
        case AST_IF: {
            // if e1 then e2 else e3
            if (!expr_ast->condition || !expr_ast->then_expr || !expr_ast->else_expr) {
                type_checker_error(checker, expr_ast, "Invalid if expression");
                return NULL;
            }
            
            // 检查条件类型（应该是Prop）
            kos_term* prop_type = type_checker_mk_prop(checker);
            kos_term* cond_term = type_checker_ast_to_term(checker, expr_ast->condition, prop_type);
            kos_term_free(prop_type);
            if (!cond_term) {
                return NULL;
            }
            
            // 推断then分支类型
            kos_term* then_type = type_checker_infer_expr(checker, expr_ast->then_expr);
            if (!then_type) {
                kos_term_free(cond_term);
                return NULL;
            }
            
            // 检查else分支类型兼容
            kos_term* else_term = type_checker_ast_to_term(checker, expr_ast->else_expr, then_type);
            if (!else_term) {
                kos_term_free(cond_term);
                kos_term_free(then_type);
                return NULL;
            }
            
            // 检查期望类型
            if (expected_type) {
                if (!type_checker_types_compatible(checker, then_type, expected_type)) {
                    kos_term_free(cond_term);
                    kos_term_free(then_type);
                    kos_term_free(else_term);
                    type_checker_error(checker, expr_ast, "If expression type mismatch");
                    return NULL;
                }
            }
            
            // 返回then分支（简化实现）
            kos_term_free(cond_term);
            kos_term_free(then_type);
            kos_term_free(else_term);
            
            // 重新创建then_term以返回
            return type_checker_ast_to_term(checker, expr_ast->then_expr, expected_type);
        }
        
        default:
            type_checker_error(checker, expr_ast, "Unsupported expression type");
            return NULL;
    }
}

// ========== 表达式类型检查（双向类型检查） ==========

kos_term* type_checker_check_expr(TypeChecker* checker, ASTNode* expr, kos_term* expected_type, bool mode) {
    if (!checker || !expr) {
        return NULL;
    }
    
    if (mode) {
        // 检查模式：给定期望类型，检查表达式是否符合
        return type_checker_ast_to_term(checker, expr, expected_type);
    } else {
        // 推断模式：推断表达式类型
        return type_checker_infer_expr(checker, expr);
    }
}

kos_term* type_checker_infer_expr(TypeChecker* checker, ASTNode* expr) {
    if (!checker || !expr) {
        return NULL;
    }
    
    switch (expr->kind) {
        case AST_VARIABLE: {
            TypeEnvEntry* entry = type_env_lookup(checker->env, expr->name);
            if (!entry || !entry->type) {
                type_checker_error(checker, expr, "Undefined variable");
                return NULL;
            }
            return kos_term_copy(entry->type);
        }
        
        case AST_LITERAL: {
            // 字面量类型推断
            if (expr->value) {
                // 简单实现：返回基础类型
                return kos_mk_universe_computational(0);
            }
            type_checker_error(checker, expr, "Invalid literal");
            return NULL;
        }
        
        case AST_LAMBDA: {
            if (!expr->param_type || !expr->lambda_body) {
                type_checker_error(checker, expr, "Invalid lambda expression");
                return NULL;
            }
            
            kos_term* param_type = type_checker_ast_to_type(checker, expr->param_type);
            if (!param_type) {
                return NULL;
            }
            
            type_env_add(checker->env, expr->param_name, param_type, false);
            
            kos_term* body_type = type_checker_infer_expr(checker, expr->lambda_body);
            if (!body_type) {
                kos_term_free(param_type);
                return NULL;
            }
            
            kos_term* pi_type = kos_mk_pi(param_type, body_type);
            kos_term_free(param_type);
            kos_term_free(body_type);
            return pi_type;
        }
        
        case AST_APPLICATION: {
            kos_term* func_type = type_checker_infer_expr(checker, expr->function);
            if (!func_type || func_type->kind != KOS_PI) {
                if (func_type) kos_term_free(func_type);
                type_checker_error(checker, expr, "Cannot infer type of function application");
                return NULL;
            }
            
            kos_term* result_type = kos_term_copy(func_type->data.pi.body);
            kos_term_free(func_type);
            return result_type;
        }
        
        case AST_TYPE_ANNOTATION: {
            // 类型注解：直接返回注解的类型
            return type_checker_ast_to_type(checker, expr->argument);
        }
        
        default:
            type_checker_error(checker, expr, "Cannot infer type for this expression");
            return NULL;
    }
}

// ========== 声明检查 ==========

bool type_checker_check_decl(TypeChecker* checker, ASTNode* decl) {
    if (!checker || !decl) {
        return false;
    }
    
    switch (decl->kind) {
        case AST_TYPE_DECL: {
            // 检查类型声明 type T : U₁
            if (!decl->name) {
                type_checker_error(checker, decl, "Type declaration missing name");
                return false;
            }
            
            // 解析类型表达式（如果有）
            kos_term* type_expr = NULL;
            if (decl->param_type) {
                // 从AST中提取类型表达式
                type_expr = type_checker_ast_to_type(checker, decl->param_type);
                if (!type_expr) {
                    return false;
                }
            } else {
                // 默认使用U₁
                type_expr = type_checker_mk_universe(checker, true, 1);
            }
            
            // 将类型名添加到环境
            if (!type_env_add(checker->env, decl->name, type_expr, true)) {
                kos_term_free(type_expr);
                type_checker_error(checker, decl, "Failed to add type to environment");
                return false;
            }
            
            return true;
        }
        
        case AST_DEF: {
            // 检查函数定义 def f : T = e
            if (!decl->name) {
                type_checker_error(checker, decl, "Function definition missing name");
                return false;
            }
            
            // 从AST中提取类型表达式和函数体
            kos_term* expected_type = NULL;
            if (decl->param_type) {
                // 解析类型注解
                expected_type = type_checker_ast_to_type(checker, decl->param_type);
                if (!expected_type) {
                    return false;
                }
            }
            
            // 检查函数体
            if (decl->lambda_body) {
                kos_term* body_type = type_checker_ast_to_term(checker, decl->lambda_body, expected_type);
                if (!body_type) {
                    if (expected_type) kos_term_free(expected_type);
                    return false;
                }
                
                // 验证函数体类型是否符合类型注解
                if (expected_type) {
                    if (!type_checker_types_compatible(checker, body_type, expected_type)) {
                        kos_term_free(body_type);
                        kos_term_free(expected_type);
                        type_checker_error(checker, decl, "Function body type does not match annotation");
                        return false;
                    }
                }
                
                // 将函数添加到环境
                kos_term* func_type = expected_type ? kos_term_copy(expected_type) : body_type;
                if (!type_env_add(checker->env, decl->name, func_type, false)) {
                    kos_term_free(body_type);
                    if (expected_type) kos_term_free(expected_type);
                    if (func_type != body_type && func_type != expected_type) kos_term_free(func_type);
                    type_checker_error(checker, decl, "Failed to add function to environment");
                    return false;
                }
                
                kos_term_free(body_type);
                if (expected_type && func_type != expected_type) {
                    kos_term_free(expected_type);
                }
            } else {
                if (expected_type) kos_term_free(expected_type);
                type_checker_error(checker, decl, "Function definition missing body");
                return false;
            }
            
            return true;
        }
        
        default:
            type_checker_error(checker, decl, "Unknown declaration type");
            return false;
    }
}

// ========== 程序检查 ==========

bool type_checker_check_program(TypeChecker* checker, ASTNode* program) {
    if (!checker || !program) {
        return false;
    }
    
    if (program->kind != AST_MODULE) {
        type_checker_error(checker, program, "Expected module declaration");
        return false;
    }
    
    // 检查模块中的所有声明
    if (program->declarations) {
        for (size_t i = 0; i < program->decl_count; i++) {
            if (!type_checker_check_decl(checker, program->declarations[i])) {
                return false;
            }
        }
    }
    
    return !checker->has_error;
}

