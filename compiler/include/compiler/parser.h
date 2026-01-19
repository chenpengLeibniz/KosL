// compiler/include/compiler/parser.h
// KOS-TL 语法分析器
// 将 Token 流转换为抽象语法树 (AST)

#ifndef KOS_TL_PARSER_H
#define KOS_TL_PARSER_H

#include "compiler/lexer.h"
#include <stddef.h>
#include <stdbool.h>

// ========== AST 节点类型 ==========

typedef enum {
    // 顶层声明
    AST_MODULE,
    AST_IMPORT,
    AST_TYPE_DECL,
    AST_DEF,
    
    // 表达式
    AST_LAMBDA,           // λ x → e
    AST_APPLICATION,      // f x
    AST_VARIABLE,         // x
    AST_LITERAL,          // 字面量（数字、字符串）
    
    // 类型表达式
    AST_TYPE_ANNOTATION,  // e : T
    AST_FUNCTION_TYPE,    // A → B
    AST_PI_TYPE,          // Π (x : A) → B
    AST_SIGMA_TYPE,       // Σ (x : A) × B
    AST_UNIVERSE,         // Uᵢ, Typeᵢ
    AST_PROP,             // Prop
    
    // 模式匹配
    AST_MATCH,            // match e with ...
    AST_CASE,             // case pattern → expr
    AST_PATTERN,          // 模式
    
    // 条件表达式
    AST_IF,               // if e then e else e
    
    // 数据结构
    AST_PAIR,             // (a, b)
    AST_FST,              // fst
    AST_SND,              // snd
    
    // let 绑定
    AST_LET,              // let x = e in e'
    
    // 其他
    AST_SEQUENCE,         // 表达式序列
    AST_TUPLE,            // (e1, e2, ..., en)
} ast_node_kind;

// AST 节点结构
typedef struct ASTNode ASTNode;

struct ASTNode {
    ast_node_kind kind;
    Token* token;              // 关联的 Token（用于错误报告）
    
    // 数据（根据 kind 使用不同的字段）
    union {
        // 标识符和字面量
        struct {
            char* name;        // 变量名、类型名等
            char* value;        // 字面量值
        };
        
        // 二元操作
        struct {
            ASTNode* left;
            ASTNode* right;
        };
        
        // 函数应用和类型
        struct {
            ASTNode* function;
            ASTNode* argument;
        };
        
        // Lambda 和 Pi 类型
        struct {
            char* param_name;   // 参数名
            ASTNode* param_type; // 参数类型
            ASTNode* lambda_body; // 函数体
        };
        
        // Sigma 类型
        struct {
            char* first_name;
            ASTNode* first_type;
            ASTNode* sigma_second_type;
        };
        
        // Match 表达式
        struct {
            ASTNode* matched_expr;
            ASTNode** cases;    // 数组
            size_t case_count;
        };
        
        // If 表达式
        struct {
            ASTNode* condition;
            ASTNode* then_expr;
            ASTNode* else_expr;
        };
        
        // Let 绑定
        struct {
            char* var_name;
            ASTNode* var_value;
            ASTNode* let_body;
        };
        
        // 序列和元组
        struct {
            ASTNode** elements;
            size_t element_count;
        };
        
        // Module
        struct {
            char* module_name;
            ASTNode** declarations;
            size_t decl_count;
        };
    };
};

// 语法分析器状态
typedef struct {
    Lexer* lexer;
    Token* current_token;
    bool has_error;
    char* error_message;
    ASTNode* root;              // 解析的根节点
} Parser;

// ========== 语法分析器接口 ==========

// 创建语法分析器
Parser* parser_create(Lexer* lexer);

// 释放语法分析器（包括 AST）
void parser_free(Parser* parser);

// 解析整个程序（返回 AST 根节点）
ASTNode* parser_parse_program(Parser* parser);

// 解析模块声明
ASTNode* parser_parse_module(Parser* parser);

// 解析类型声明
ASTNode* parser_parse_type_decl(Parser* parser);

// 解析函数定义
ASTNode* parser_parse_def(Parser* parser);

// 解析表达式
ASTNode* parser_parse_expr(Parser* parser);

// 解析类型表达式
ASTNode* parser_parse_type(Parser* parser);

// 解析模式
ASTNode* parser_parse_pattern(Parser* parser);

// 解析 Lambda 表达式（内部使用）
ASTNode* parser_parse_lambda(Parser* parser);

// 解析 Let 表达式（内部使用）
ASTNode* parser_parse_let(Parser* parser);

// 解析 If 表达式（内部使用）
ASTNode* parser_parse_if(Parser* parser);

// 解析 Match 表达式（内部使用）
ASTNode* parser_parse_match(Parser* parser);

// 检查是否有错误
bool parser_has_error(Parser* parser);

// 获取错误消息
const char* parser_get_error(Parser* parser);

// ========== AST 操作 ==========

// 创建 AST 节点
ASTNode* ast_node_create(ast_node_kind kind, Token* token);

// 释放 AST 节点（递归释放子树）
void ast_node_free(ASTNode* node);

// 复制 AST 节点
ASTNode* ast_node_copy(ASTNode* node);

// 获取节点类型名称（用于调试）
const char* ast_node_kind_name(ast_node_kind kind);

// 打印 AST（用于调试）
void ast_print(ASTNode* node, int indent);

// ========== 辅助函数 ==========

// 期望特定的 Token 类型
bool parser_expect(Parser* parser, token_kind kind, const char* error_msg);

// 检查当前 Token 类型
bool parser_check(Parser* parser, token_kind kind);

// 消耗当前 Token 并移动到下一个
Token* parser_advance(Parser* parser);

// 获取当前 Token（不消耗）
Token* parser_peek(Parser* parser);

// 报告错误
void parser_error(Parser* parser, const char* message);

#endif // KOS_TL_PARSER_H

