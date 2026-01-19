// compiler/include/compiler/cic_core.h
// Calculus of Inductive Constructions (CIC) 内核
// 基于 typechecker-zoo 的设计思路，实现完整的CIC类型系统
// 参考: https://github.com/sdiehl/typechecker-zoo/

#ifndef KOS_TL_CIC_CORE_H
#define KOS_TL_CIC_CORE_H

#include "compiler/parser.h"
#include "../../include/kos_core.h"
#include <stdbool.h>
#include <stddef.h>

// ========== CIC 内核术语类型 ==========

// CIC术语种类（扩展kos_term的kind）
typedef enum {
    // 基础类型（继承自kos_term）
    CIC_SORT,           // 排序（Sort）：U_i, Type_i, Prop
    CIC_VAR,            // 变量（Variable）
    CIC_CONST,          // 常量（Constant）
    CIC_APP,            // 应用（Application）：M N
    CIC_LAMBDA,         // Lambda抽象：λx:A.M
    CIC_PI,             // 依赖积类型：Πx:A.B
    CIC_SIGMA,          // 依赖和类型：Σx:A.B
    
    // CIC扩展：归纳类型
    CIC_INDUCTIVE,      // 归纳类型定义
    CIC_CONSTRUCTOR,    // 构造子（Constructor）
    CIC_ELIMINATOR,     // 消除子（Eliminator）：match/case
    CIC_FIX,            // 递归定义：fix f : A := M
    CIC_COFIX,          // 余递归定义：cofix f : A := M
    CIC_CASE,           // Case表达式：case M of ...
} cic_term_kind;

// ========== 归纳类型定义 ==========

// 构造子定义
typedef struct {
    char* name;              // 构造子名称
    kos_term* type;          // 构造子类型（通常是Π类型）
    size_t arity;            // 参数数量
} cic_constructor;

// 归纳类型定义
typedef struct {
    char* name;              // 归纳类型名称
    kos_term* type;          // 归纳类型本身（通常是Sort）
    kos_term* parameters;    // 类型参数（Π类型链）
    cic_constructor* constructors; // 构造子数组
    size_t constructor_count;      // 构造子数量
    size_t constructor_capacity;   // 构造子数组容量
    bool is_positive;        // 是否为严格正类型（用于递归检查）
} cic_inductive_def;

// ========== CIC 环境（Context） ==========

// 环境条目类型
typedef enum {
    CIC_ENTRY_VAR,      // 变量绑定：x : A
    CIC_ENTRY_DEF,      // 定义：x : A := M
    CIC_ENTRY_INDUCTIVE // 归纳类型定义
} cic_entry_kind;

// 环境条目
typedef struct cic_entry {
    cic_entry_kind kind;
    char* name;              // 名称
    kos_term* type;          // 类型
    kos_term* definition;    // 定义（对于CIC_ENTRY_DEF）
    cic_inductive_def* inductive; // 归纳类型定义（对于CIC_ENTRY_INDUCTIVE）
    struct cic_entry* next;  // 下一个条目（链表结构）
} cic_entry;

// CIC环境（Γ）
typedef struct {
    cic_entry* head;         // 环境条目链表（最近添加的在头部）
    size_t length;           // 环境长度
} cic_context;

// ========== CIC 内核状态 ==========

typedef struct {
    cic_context* ctx;        // 当前环境
    bool has_error;          // 是否有错误
    char* error_message;     // 错误消息
    ASTNode* error_node;     // 出错的AST节点
    int max_universe_level;  // 最大Universe层级
} cic_core;

// ========== CIC 内核接口 ==========

// 创建CIC内核
cic_core* cic_core_create(void);

// 释放CIC内核
void cic_core_free(cic_core* core);

// ========== 环境操作 ==========

// 创建空环境
cic_context* cic_context_create(void);

// 释放环境
void cic_context_free(cic_context* ctx);

// 添加变量到环境：Γ, x : A
cic_context* cic_context_add_var(cic_context* ctx, const char* name, kos_term* type);

// 添加定义到环境：Γ, x : A := M
cic_context* cic_context_add_def(cic_context* ctx, const char* name, kos_term* type, kos_term* definition);

// 添加归纳类型到环境
cic_context* cic_context_add_inductive(cic_context* ctx, cic_inductive_def* inductive);

// 查找环境中的条目
cic_entry* cic_context_lookup(cic_context* ctx, const char* name);

// ========== 类型检查（双向算法） ==========

// 检查模式（Check Mode）：给定类型，检查项是否符合
// Γ ⊢ M : A
bool cic_check(cic_core* core, kos_term* term, kos_term* type);

// 推断模式（Infer Mode）：推断项的类型
// Γ ⊢ M : ?
kos_term* cic_infer(cic_core* core, kos_term* term);

// 检查类型是否为有效的排序（Sort）
// Γ ⊢ A : s
bool cic_check_sort(cic_core* core, kos_term* type);

// 检查类型相等性（考虑转换）
// Γ ⊢ A ≡ B
bool cic_types_equal(cic_core* core, kos_term* type1, kos_term* type2);

// ========== 归约（Reduction） ==========

// β-归约：应用Lambda抽象
// (λx:A.M) N → M[x := N]
kos_term* cic_beta_reduce(cic_core* core, kos_term* lambda, kos_term* argument);

// ι-归约：模式匹配归约（用于归纳类型）
// match (C_i args) with ... | C_i x => M | ... → M[x := args]
kos_term* cic_iota_reduce(cic_core* core, kos_term* match_expr);

// 归约到弱头范式（Weak Head Normal Form）
kos_term* cic_whnf(cic_core* core, kos_term* term);

// 归约到范式（Normal Form）
kos_term* cic_nf(cic_core* core, kos_term* term);

// ========== 归纳类型操作 ==========

// 创建归纳类型定义
cic_inductive_def* cic_inductive_create(const char* name, kos_term* type);

// 释放归纳类型定义
void cic_inductive_free(cic_inductive_def* inductive);

// 添加构造子到归纳类型
bool cic_inductive_add_constructor(cic_inductive_def* inductive, const char* name, kos_term* type);

// 检查归纳类型定义的有效性
bool cic_check_inductive(cic_core* core, cic_inductive_def* inductive);

// 创建构造子应用
kos_term* cic_mk_constructor_app(cic_core* core, const char* constructor_name, kos_term** args, size_t arg_count);

// ========== 模式匹配 ==========

// Case表达式：case M of C₁ x₁ => M₁ | C₂ x₂ => M₂ | ...
typedef struct {
    kos_term* matched_term;      // 被匹配的项
    kos_term* matched_type;       // 被匹配项的类型（归纳类型）
    struct {
        char* constructor_name;   // 构造子名称
        char** pattern_vars;     // 模式变量数组
        size_t pattern_var_count; // 模式变量数量
        kos_term* body;          // 匹配体
    }* cases;                    // Case分支数组
    size_t case_count;           // Case分支数量
} cic_case_expr;

// 创建Case表达式
cic_case_expr* cic_case_create(kos_term* matched_term, kos_term* matched_type);

// 添加Case分支
bool cic_case_add_branch(cic_case_expr* case_expr, const char* constructor_name, 
                         const char** pattern_vars, size_t var_count, kos_term* body);

// 检查Case表达式的类型
kos_term* cic_check_case(cic_core* core, cic_case_expr* case_expr, kos_term* expected_type);

// ========== 递归定义 ==========

// Fix表达式：fix f : A := M（递归定义）
kos_term* cic_mk_fix(const char* name, kos_term* type, kos_term* body);

// CoFix表达式：cofix f : A := M（余递归定义）
kos_term* cic_mk_cofix(const char* name, kos_term* type, kos_term* body);

// 检查递归定义的合法性（确保类型是严格正的）
bool cic_check_fix(cic_core* core, kos_term* fix_term);

// ========== AST 转换 ==========

// 将AST节点转换为CIC术语
kos_term* cic_ast_to_term(cic_core* core, ASTNode* ast);

// 将CIC术语转换为AST节点（用于代码生成）
ASTNode* cic_term_to_ast(cic_core* core, kos_term* term);

// ========== Universe层级检查 ==========

// 检查Universe层级关系
bool cic_check_universe(cic_core* core, kos_term* type, int min_level);

// 计算类型的Universe层级
int cic_universe_level(cic_core* core, kos_term* type);

// ========== 错误处理 ==========

// 检查是否有错误
bool cic_has_error(cic_core* core);

// 获取错误消息
const char* cic_get_error(cic_core* core);

// 报告错误
void cic_error(cic_core* core, ASTNode* node, const char* message);

#endif // KOS_TL_CIC_CORE_H
