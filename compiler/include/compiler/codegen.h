// compiler/include/compiler/codegen.h
// KOS-TL 代码生成器
// 将类型检查后的 AST 转换为 C 代码

#ifndef KOS_TL_CODEGEN_H
#define KOS_TL_CODEGEN_H

#include "compiler/parser.h"
#include "compiler/type_checker.h"
#include <stdio.h>
#include <stdbool.h>

// 前向声明
typedef struct kos_term kos_term;

// ========== 代码生成器状态 ==========

typedef struct {
    FILE* output;               // 输出文件
    int indent_level;           // 当前缩进级别
    char* current_function;     // 当前正在生成的函数名
    bool in_function;           // 是否在函数内部
    int temp_var_counter;       // 临时变量计数器
    TypeChecker* type_checker;  // 类型检查器（用于类型信息）
} CodeGenState;

// ========== 代码生成器接口 ==========

// 创建代码生成器状态
CodeGenState* codegen_create(FILE* output, TypeChecker* type_checker);

// 释放代码生成器状态
void codegen_free(CodeGenState* state);

// 生成整个程序（模块）
bool codegen_program(CodeGenState* state, ASTNode* program);

// 生成模块
bool codegen_module(CodeGenState* state, ASTNode* module);

// 生成类型声明
bool codegen_type_decl(CodeGenState* state, ASTNode* type_decl);

// 生成函数定义
bool codegen_def(CodeGenState* state, ASTNode* def);

// 生成表达式
bool codegen_expr(CodeGenState* state, ASTNode* expr, kos_term* expected_type);

// 生成类型表达式（C类型声明）
bool codegen_type_expr(CodeGenState* state, ASTNode* type_ast);

// 生成kos_term类型（C代码）
bool codegen_kos_term_type(CodeGenState* state, kos_term* type);

// ========== 辅助函数 ==========

// 输出缩进
void codegen_indent(CodeGenState* state);

// 输出字符串（带转义）
void codegen_print_string(CodeGenState* state, const char* str);

// 输出格式化字符串
void codegen_printf(CodeGenState* state, const char* format, ...);

// 生成临时变量名
char* codegen_temp_var(CodeGenState* state);

// 生成C标识符（确保合法）
char* codegen_c_identifier(const char* name);

// 生成类型到C类型的映射
const char* codegen_type_to_c_type(kos_term* type);

// 生成KOS-TL构造到C API的映射
const char* codegen_kos_api_call(const char* construct, kos_term* type);

// ========== 代码生成辅助 ==========

// 生成文件头（包含、注释等）
void codegen_file_header(CodeGenState* state, const char* module_name);

// 生成文件尾
void codegen_file_footer(CodeGenState* state);

// 生成函数声明
void codegen_function_decl(CodeGenState* state, const char* name, kos_term* type);

// 生成函数定义开始
void codegen_function_start(CodeGenState* state, const char* name, kos_term* type);

// 生成函数定义结束
void codegen_function_end(CodeGenState* state);

// 生成变量声明
void codegen_variable_decl(CodeGenState* state, const char* name, kos_term* type);

// 生成返回语句
void codegen_return(CodeGenState* state, const char* value);

#endif // KOS_TL_CODEGEN_H





