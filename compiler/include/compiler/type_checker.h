// compiler/include/compiler/type_checker.h
// KOS-TL 类型检查器
// 实现双向类型检查和类型推断

#ifndef KOS_TL_TYPE_CHECKER_H
#define KOS_TL_TYPE_CHECKER_H

#include "compiler/parser.h"
#include <stdbool.h>
#include <stddef.h>

// 前向声明
typedef struct kos_term kos_term;

// ========== 类型环境 (Type Environment) ==========

// 类型环境条目
typedef struct {
    char* name;          // 变量名
    kos_term* type;      // 类型
    bool is_type_var;    // 是否为类型变量（如 U₁, Type₁）
} TypeEnvEntry;

// 类型环境
typedef struct {
    TypeEnvEntry* entries;
    size_t count;
    size_t capacity;
} TypeEnv;

// ========== 类型检查器状态 ==========

typedef struct {
    TypeEnv* env;                // 类型环境
    bool has_error;              // 是否有错误
    char* error_message;         // 错误消息
    ASTNode* error_node;         // 出错的AST节点
    int universe_level;          // 当前Universe层级
} TypeChecker;

// ========== 类型检查器接口 ==========

// 创建类型检查器
TypeChecker* type_checker_create(void);

// 释放类型检查器
void type_checker_free(TypeChecker* checker);

// 检查整个程序（模块）
bool type_checker_check_program(TypeChecker* checker, ASTNode* program);

// 检查表达式（双向类型检查）
// mode: true = 检查模式（给定类型），false = 推断模式（推断类型）
kos_term* type_checker_check_expr(TypeChecker* checker, ASTNode* expr, kos_term* expected_type, bool mode);

// 推断表达式类型
kos_term* type_checker_infer_expr(TypeChecker* checker, ASTNode* expr);

// 检查类型表达式
kos_term* type_checker_check_type(TypeChecker* checker, ASTNode* type_expr);

// 检查声明
bool type_checker_check_decl(TypeChecker* checker, ASTNode* decl);

// ========== 类型环境操作 ==========

// 创建类型环境
TypeEnv* type_env_create(void);

// 释放类型环境
void type_env_free(TypeEnv* env);

// 添加条目到类型环境
bool type_env_add(TypeEnv* env, const char* name, kos_term* type, bool is_type_var);

// 查找类型环境中的条目
TypeEnvEntry* type_env_lookup(TypeEnv* env, const char* name);

// 创建新的作用域（用于let、lambda等）
TypeEnv* type_env_push_scope(TypeEnv* env);

// 弹出作用域
void type_env_pop_scope(TypeEnv* env);

// ========== 类型转换和检查 ==========

// 检查类型相等性（考虑Universe层级）
bool type_checker_types_equal(TypeChecker* checker, kos_term* type1, kos_term* type2);

// 检查类型兼容性（子类型关系）
bool type_checker_types_compatible(TypeChecker* checker, kos_term* subtype, kos_term* supertype);

// 检查Universe层级关系
bool type_checker_check_universe_level(TypeChecker* checker, kos_term* type, int min_level);

// 应用Universe提升规则
kos_term* type_checker_lift_universe(TypeChecker* checker, kos_term* type);

// ========== AST 到 kos_term 转换 ==========

// 将AST类型节点转换为kos_term
kos_term* type_checker_ast_to_type(TypeChecker* checker, ASTNode* type_ast);

// 将AST表达式节点转换为kos_term（带类型检查）
kos_term* type_checker_ast_to_term(TypeChecker* checker, ASTNode* expr_ast, kos_term* expected_type);

// ========== 错误处理 ==========

// 检查是否有错误
bool type_checker_has_error(TypeChecker* checker);

// 获取错误消息
const char* type_checker_get_error(TypeChecker* checker);

// 报告错误
void type_checker_error(TypeChecker* checker, ASTNode* node, const char* message);

// ========== 辅助函数 ==========

// 创建基础类型（U₁, Type₁, Prop等）
kos_term* type_checker_mk_universe(TypeChecker* checker, bool is_computational, int level);
kos_term* type_checker_mk_prop(TypeChecker* checker);
kos_term* type_checker_mk_id(TypeChecker* checker, const char* name);
kos_term* type_checker_mk_val(TypeChecker* checker, const char* value);

// 创建依赖类型
kos_term* type_checker_mk_pi_type(TypeChecker* checker, const char* param_name, kos_term* param_type, kos_term* body_type);
kos_term* type_checker_mk_sigma_type(TypeChecker* checker, const char* first_name, kos_term* first_type, kos_term* second_type);

// 创建函数类型（非依赖）
kos_term* type_checker_mk_function_type(TypeChecker* checker, kos_term* domain, kos_term* codomain);

#endif // KOS_TL_TYPE_CHECKER_H





