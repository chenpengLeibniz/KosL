// compiler/include/compiler/lexer.h
// KOS-TL 词法分析器
// 将源代码转换为 Token 流

#ifndef KOS_TL_LEXER_H
#define KOS_TL_LEXER_H

#include <stddef.h>
#include <stdbool.h>

// ========== Token 类型定义 ==========

typedef enum {
    // 文件结束
    TOKEN_EOF,
    
    // 标识符和字面量
    TOKEN_IDENTIFIER,      // 标识符（变量名、类型名等）
    TOKEN_NUMBER,          // 数字字面量
    TOKEN_STRING,          // 字符串字面量
    
    // 关键字
    TOKEN_MODULE,          // module
    TOKEN_WHERE,           // where
    TOKEN_IMPORT,          // import
    TOKEN_TYPE,            // type
    TOKEN_DEF,             // def
    TOKEN_LET,             // let
    TOKEN_IN,              // in
    TOKEN_IF,              // if
    TOKEN_THEN,            // then
    TOKEN_ELSE,            // else
    TOKEN_MATCH,           // match
    TOKEN_WITH,            // with
    TOKEN_CASE,            // case
    TOKEN_OF,              // of
    TOKEN_AS,              // as
    
    // 类型构造关键字
    TOKEN_SIGMA,           // Σ
    TOKEN_PI,              // Π
    TOKEN_LAMBDA,          // λ
    TOKEN_ARROW,           // →
    TOKEN_DARROW,          // =>
    TOKEN_TIMES,           // ×
    
    // 逻辑操作符
    TOKEN_AND,             // ∧ 或 &&
    TOKEN_OR,              // ∨ 或 ||
    TOKEN_NOT,             // ¬ 或 !
    TOKEN_IMPLIES,         // ⇒ 或 ->
    
    // 比较操作符
    TOKEN_EQ,              // ==
    TOKEN_NE,              // !=
    TOKEN_LT,              // <
    TOKEN_GT,              // >
    TOKEN_LE,              // <=
    TOKEN_GE,              // >=
    
    // 算术操作符
    TOKEN_PLUS,            // +
    TOKEN_MINUS,           // -
    TOKEN_MUL,             // *
    TOKEN_DIV,             // /
    TOKEN_MOD,             // %
    
    // 标点符号
    TOKEN_LPAREN,          // (
    TOKEN_RPAREN,          // )
    TOKEN_LBRACE,          // {
    TOKEN_RBRACE,          // }
    TOKEN_LBRACKET,        // [
    TOKEN_RBRACKET,        // ]
    TOKEN_COMMA,           // ,
    TOKEN_SEMICOLON,       // ;
    TOKEN_COLON,           // :
    TOKEN_DOT,             // .
    TOKEN_UNDERSCORE,      // _
    TOKEN_PIPE,            // |
    
    // Universe 类型
    TOKEN_U,               // U (Universe 计算轴)
    TOKEN_TYPE_KW,         // Type (Universe 逻辑轴)
    TOKEN_PROP,            // Prop (命题类型)
    
    // 其他
    TOKEN_QUESTION,        // ?
    TOKEN_EXCLAM,          // !
    TOKEN_AT,              // @
    TOKEN_HASH,            // #
    
    // 注释（内部使用，不输出）
    TOKEN_COMMENT,         // 注释
    TOKEN_WHITESPACE       // 空白（内部使用）
} token_kind;

// Token 结构
typedef struct {
    token_kind kind;       // Token 类型
    char* value;           // Token 值（字符串）
    size_t length;         // 值长度
    int line;              // 行号（从1开始）
    int column;            // 列号（从1开始）
    int position;          // 在源文件中的位置
} Token;

// 词法分析器状态
typedef struct {
    const char* source;    // 源代码
    size_t source_length;  // 源代码长度
    size_t position;      // 当前位置
    int line;              // 当前行号
    int column;            // 当前列号
    Token* current_token;  // 当前 Token（缓存）
    bool has_error;        // 是否有错误
    char* error_message;  // 错误消息
    bool owns_source;      // 是否拥有 source 的所有权（需要释放）
} Lexer;

// ========== 词法分析器接口 ==========

// 创建词法分析器
// source: 源代码字符串（不需要释放，由调用者管理）
Lexer* lexer_create(const char* source);

// 释放词法分析器
void lexer_free(Lexer* lexer);

// 获取下一个 Token（不消耗，可多次调用返回相同 Token）
Token* lexer_peek(Lexer* lexer);

// 获取下一个 Token 并消耗（移动到下一个）
Token* lexer_next(Lexer* lexer);

// 检查是否到达文件末尾
bool lexer_is_eof(Lexer* lexer);

// 获取当前行号
int lexer_get_line(Lexer* lexer);

// 获取当前列号
int lexer_get_column(Lexer* lexer);

// 检查是否有错误
bool lexer_has_error(Lexer* lexer);

// 获取错误消息
const char* lexer_get_error(Lexer* lexer);

// ========== Token 操作 ==========

// 释放 Token（如果不再需要）
void token_free(Token* token);

// 复制 Token
Token* token_copy(Token* token);

// 获取 Token 类型名称（用于调试）
const char* token_kind_name(token_kind kind);

// 检查 Token 是否为关键字
bool token_is_keyword(Token* token);

// 检查 Token 是否为操作符
bool token_is_operator(Token* token);

// 检查 Token 是否为标点符号
bool token_is_punctuation(Token* token);

// ========== 工具函数 ==========

// 从文件读取源代码并创建词法分析器
Lexer* lexer_from_file(const char* filename);

// 获取所有 Token（用于调试和测试）
Token** lexer_tokenize_all(Lexer* lexer, size_t* count);

// 释放 Token 数组
void token_array_free(Token** tokens, size_t count);

#endif // KOS_TL_LEXER_H
















