// compiler/src/compiler/lexer.c
// KOS-TL 词法分析器实现

#include "compiler/lexer.h"
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <stdio.h>
#include <errno.h>

// strdup 的跨平台实现
#ifdef _WIN32
#define strdup _strdup
#else
// POSIX 系统通常有 strdup
#ifndef _POSIX_C_SOURCE
#define _POSIX_C_SOURCE 200809L
#endif
#endif

// ========== 关键字表 ==========

typedef struct {
    const char* keyword;
    token_kind kind;
} KeywordEntry;

static const KeywordEntry keyword_table[] = {
    {"module", TOKEN_MODULE},
    {"where", TOKEN_WHERE},
    {"import", TOKEN_IMPORT},
    {"type", TOKEN_TYPE},
    {"def", TOKEN_DEF},
    {"let", TOKEN_LET},
    {"in", TOKEN_IN},
    {"if", TOKEN_IF},
    {"then", TOKEN_THEN},
    {"else", TOKEN_ELSE},
    {"match", TOKEN_MATCH},
    {"with", TOKEN_WITH},
    {"case", TOKEN_CASE},
    {"of", TOKEN_OF},
    {"as", TOKEN_AS},
    {"Prop", TOKEN_PROP},
    {"Type", TOKEN_TYPE_KW},
    {"U", TOKEN_U},
    {NULL, TOKEN_EOF}
};

// ========== 辅助函数 ==========

// 检查是否为关键字
static token_kind lookup_keyword(const char* str, size_t len) {
    for (int i = 0; keyword_table[i].keyword != NULL; i++) {
        if (strlen(keyword_table[i].keyword) == len &&
            strncmp(keyword_table[i].keyword, str, len) == 0) {
            return keyword_table[i].kind;
        }
    }
    return TOKEN_IDENTIFIER;
}

// 检查字符是否为标识符起始字符
static bool is_identifier_start(char c) {
    return (c >= 'a' && c <= 'z') ||
           (c >= 'A' && c <= 'Z') ||
           c == '_' ||
           (c >= 0x80); // Unicode 字符
}

// 检查字符是否为标识符字符
static bool is_identifier_char(char c) {
    return is_identifier_start(c) ||
           (c >= '0' && c <= '9') ||
           c == '\'' || // Haskell 风格的单引号
           ((unsigned char)c >= 0x80 && (unsigned char)c <= 0xBF); // UTF-8 多字节字符的后续字节
}

// 检查字符是否为数字
static bool is_digit(char c) {
    return c >= '0' && c <= '9';
}

// 检查字符是否为十六进制数字
static bool is_hex_digit(char c) {
    return is_digit(c) ||
           (c >= 'a' && c <= 'f') ||
           (c >= 'A' && c <= 'F');
}

// 检查字符是否为空白字符
static bool is_whitespace(char c) {
    return c == ' ' || c == '\t' || c == '\r';
}

// 检查是否为行结束符
static bool is_newline(char c) {
    return c == '\n';
}

// 获取当前字符
static char lexer_current(Lexer* lexer) {
    if (lexer->position >= lexer->source_length) {
        return '\0';
    }
    return lexer->source[lexer->position];
}

// 获取下一个字符（不移动位置）
static char lexer_peek_char(Lexer* lexer, size_t offset) {
    size_t pos = lexer->position + offset;
    if (pos >= lexer->source_length) {
        return '\0';
    }
    return lexer->source[pos];
}

// 前进一个字符
static void lexer_advance(Lexer* lexer) {
    if (lexer->position < lexer->source_length) {
        if (is_newline(lexer->source[lexer->position])) {
            lexer->line++;
            lexer->column = 1;
        } else {
            lexer->column++;
        }
        lexer->position++;
    }
}

// 跳过空白字符
static void lexer_skip_whitespace(Lexer* lexer) {
    while (lexer->position < lexer->source_length) {
        char c = lexer_current(lexer);
        if (is_whitespace(c)) {
            lexer_advance(lexer);
        } else if (is_newline(c)) {
            lexer_advance(lexer);
        } else {
            break;
        }
    }
}

// 读取标识符或关键字
static Token* lex_identifier(Lexer* lexer) {
    size_t start = lexer->position;
    int start_line = lexer->line;
    int start_column = lexer->column;
    
    // 读取标识符字符
    while (lexer->position < lexer->source_length) {
        char c = lexer_current(lexer);
        
        // 如果是 UTF-8 多字节字符的第一个字节，需要特殊处理
        if ((unsigned char)c >= 0x80) {
            // UTF-8 编码规则：
            // - 0xE0-0xEF: 3 字节字符（如 ₁ = 0xE2 0x82 0x81）
            // - 0xF0-0xF7: 4 字节字符
            // - 0xC0-0xDF: 2 字节字符
            // - 0x80-0xBF: UTF-8 多字节字符的后续字节
            if ((unsigned char)c >= 0xE0 && (unsigned char)c <= 0xEF) {
                // 3 字节 UTF-8 字符，需要读取 3 个字节
                if (lexer->position + 2 < lexer->source_length) {
                    lexer_advance(lexer); // 第一个字节
                    lexer_advance(lexer); // 第二个字节
                    lexer_advance(lexer); // 第三个字节
                    continue;
                } else {
                    break; // 没有足够的字节，停止
                }
            } else if ((unsigned char)c >= 0xF0 && (unsigned char)c <= 0xF7) {
                // 4 字节 UTF-8 字符，需要读取 4 个字节
                if (lexer->position + 3 < lexer->source_length) {
                    lexer_advance(lexer); // 第一个字节
                    lexer_advance(lexer); // 第二个字节
                    lexer_advance(lexer); // 第三个字节
                    lexer_advance(lexer); // 第四个字节
                    continue;
                } else {
                    break; // 没有足够的字节，停止
                }
            } else if ((unsigned char)c >= 0xC0 && (unsigned char)c <= 0xDF) {
                // 2 字节 UTF-8 字符，需要读取 2 个字节
                if (lexer->position + 1 < lexer->source_length) {
                    lexer_advance(lexer); // 第一个字节
                    lexer_advance(lexer); // 第二个字节
                    continue;
                } else {
                    break; // 没有足够的字节，停止
                }
            } else if ((unsigned char)c >= 0x80 && (unsigned char)c <= 0xBF) {
                // UTF-8 多字节字符的后续字节，不应该单独出现
                // 这可能是错误，但为了容错，我们跳过它
                fprintf(stderr, "[LEXER DEBUG] Warning: Found UTF-8 continuation byte at position %zu\n", lexer->position);
                lexer_advance(lexer);
                continue;
            } else {
                // 其他 Unicode 字符，按单个字节处理
                lexer_advance(lexer);
                continue;
            }
        } else if (is_identifier_char(c)) {
            // ASCII 标识符字符
            lexer_advance(lexer);
        } else {
            // 不是标识符字符，停止
            break;
        }
    }
    
    size_t length = lexer->position - start;
    const char* str = lexer->source + start;
    
    // 检查是否为关键字
    token_kind kind = lookup_keyword(str, length);
    
    // 创建 Token
    Token* token = (Token*)malloc(sizeof(Token));
    if (!token) {
        lexer->has_error = true;
        lexer->error_message = "Memory allocation failed";
        return NULL;
    }
    
    token->kind = kind;
    token->value = (char*)malloc(length + 1);
    if (!token->value) {
        free(token);
        lexer->has_error = true;
        lexer->error_message = "Memory allocation failed";
        return NULL;
    }
    memcpy(token->value, str, length);
    token->value[length] = '\0';
    token->length = length;
    token->line = start_line;
    token->column = start_column;
    token->position = start;
    
    return token;
}

// 读取数字
static Token* lex_number(Lexer* lexer) {
    size_t start = lexer->position;
    int start_line = lexer->line;
    int start_column = lexer->column;
    
    // 读取整数部分
    while (lexer->position < lexer->source_length &&
           is_digit(lexer_current(lexer))) {
        lexer_advance(lexer);
    }
    
    // 检查是否有小数点
    if (lexer_current(lexer) == '.' &&
        is_digit(lexer_peek_char(lexer, 1))) {
        lexer_advance(lexer); // 跳过小数点
        while (lexer->position < lexer->source_length &&
               is_digit(lexer_current(lexer))) {
            lexer_advance(lexer);
        }
    }
    
    size_t length = lexer->position - start;
    const char* str = lexer->source + start;
    
    Token* token = (Token*)malloc(sizeof(Token));
    if (!token) {
        lexer->has_error = true;
        lexer->error_message = "Memory allocation failed";
        return NULL;
    }
    
    token->kind = TOKEN_NUMBER;
    token->value = (char*)malloc(length + 1);
    if (!token->value) {
        free(token);
        lexer->has_error = true;
        lexer->error_message = "Memory allocation failed";
        return NULL;
    }
    memcpy(token->value, str, length);
    token->value[length] = '\0';
    token->length = length;
    token->line = start_line;
    token->column = start_column;
    token->position = start;
    
    return token;
}

// 读取字符串字面量
static Token* lex_string(Lexer* lexer) {
    size_t start = lexer->position;
    int start_line = lexer->line;
    int start_column = lexer->column;
    
    lexer_advance(lexer); // 跳过开始引号
    
    // 读取字符串内容（处理转义）
    size_t value_start = lexer->position;
    while (lexer->position < lexer->source_length) {
        char c = lexer_current(lexer);
        if (c == '"') {
            break;
        }
        if (c == '\\' && lexer->position + 1 < lexer->source_length) {
            lexer_advance(lexer); // 跳过转义字符
        }
        if (is_newline(c)) {
            lexer->has_error = true;
            lexer->error_message = "Unterminated string literal";
            return NULL;
        }
        lexer_advance(lexer);
    }
    
    if (lexer->position >= lexer->source_length) {
        lexer->has_error = true;
        lexer->error_message = "Unterminated string literal";
        return NULL;
    }
    
    size_t value_length = lexer->position - value_start;
    lexer_advance(lexer); // 跳过结束引号
    
    Token* token = (Token*)malloc(sizeof(Token));
    if (!token) {
        lexer->has_error = true;
        lexer->error_message = "Memory allocation failed";
        return NULL;
    }
    
    token->kind = TOKEN_STRING;
    token->value = (char*)malloc(value_length + 1);
    if (!token->value) {
        free(token);
        lexer->has_error = true;
        lexer->error_message = "Memory allocation failed";
        return NULL;
    }
    memcpy(token->value, lexer->source + value_start, value_length);
    token->value[value_length] = '\0';
    token->length = value_length;
    token->line = start_line;
    token->column = start_column;
    token->position = start;
    
    return token;
}

// 读取单行注释
static void lex_single_line_comment(Lexer* lexer) {
    // 跳过 "--"
    lexer_advance(lexer);
    lexer_advance(lexer);
    
    // 跳过到行尾
    while (lexer->position < lexer->source_length &&
           !is_newline(lexer_current(lexer))) {
        lexer_advance(lexer);
    }
}

// 读取多行注释
static bool lex_multi_line_comment(Lexer* lexer) {
    // 跳过 "{-"
    lexer_advance(lexer);
    lexer_advance(lexer);
    
    int depth = 1;
    while (lexer->position < lexer->source_length && depth > 0) {
        if (lexer_current(lexer) == '{' &&
            lexer_peek_char(lexer, 1) == '-') {
            depth++;
            lexer_advance(lexer);
            lexer_advance(lexer);
        } else if (lexer_current(lexer) == '-' &&
                   lexer_peek_char(lexer, 1) == '}') {
            depth--;
            lexer_advance(lexer);
            lexer_advance(lexer);
        } else {
            lexer_advance(lexer);
        }
    }
    
    if (depth > 0) {
        lexer->has_error = true;
        lexer->error_message = "Unterminated multi-line comment";
        return false;
    }
    
    return true;
}

// 读取操作符或标点符号
static Token* lex_operator_or_punctuation(Lexer* lexer) {
    char c = lexer_current(lexer);
    int start_line = lexer->line;
    int start_column = lexer->column;
    size_t start_pos = lexer->position;
    
    Token* token = (Token*)malloc(sizeof(Token));
    if (!token) {
        lexer->has_error = true;
        lexer->error_message = "Memory allocation failed";
        return NULL;
    }
    
    token->line = start_line;
    token->column = start_column;
    token->position = start_pos;
    
    // 检查多字符操作符
    char next = lexer_peek_char(lexer, 1);
    
    switch (c) {
        case 0x2192: // → (Rightwards Arrow, Unicode)
            token->kind = TOKEN_ARROW;
            token->value = strdup("→");
            token->length = 1;
            lexer_advance(lexer);
            return token;
            
        case '-':
            if (next == '>') {
                token->kind = TOKEN_ARROW;
                token->value = strdup("->");
                token->length = 2;
                lexer_advance(lexer);
                lexer_advance(lexer);
                return token;
            }
            token->kind = TOKEN_MINUS;
            token->value = strdup("-");
            token->length = 1;
            lexer_advance(lexer);
            return token;
            
        case '=':
            if (next == '=') {
                token->kind = TOKEN_EQ;
                token->value = strdup("==");
                token->length = 2;
                lexer_advance(lexer);
                lexer_advance(lexer);
                return token;
            }
            if (next == '>') {
                token->kind = TOKEN_DARROW;
                token->value = strdup("=>");
                token->length = 2;
                lexer_advance(lexer);
                lexer_advance(lexer);
                return token;
            }
            token->kind = TOKEN_EQ;
            token->value = strdup("=");
            token->length = 1;
            lexer_advance(lexer);
            return token;
            
        case '!':
            if (next == '=') {
                token->kind = TOKEN_NE;
                token->value = strdup("!=");
                token->length = 2;
                lexer_advance(lexer);
                lexer_advance(lexer);
                return token;
            }
            token->kind = TOKEN_EXCLAM;
            token->value = strdup("!");
            token->length = 1;
            lexer_advance(lexer);
            return token;
            
        case '<':
            if (next == '=') {
                token->kind = TOKEN_LE;
                token->value = strdup("<=");
                token->length = 2;
                lexer_advance(lexer);
                lexer_advance(lexer);
                return token;
            }
            token->kind = TOKEN_LT;
            token->value = strdup("<");
            token->length = 1;
            lexer_advance(lexer);
            return token;
            
        case '>':
            if (next == '=') {
                token->kind = TOKEN_GE;
                token->value = strdup(">=");
                token->length = 2;
                lexer_advance(lexer);
                lexer_advance(lexer);
                return token;
            }
            token->kind = TOKEN_GT;
            token->value = strdup(">");
            token->length = 1;
            lexer_advance(lexer);
            return token;
            
        case '&':
            if (next == '&') {
                token->kind = TOKEN_AND;
                token->value = strdup("&&");
                token->length = 2;
                lexer_advance(lexer);
                lexer_advance(lexer);
                return token;
            }
            // 单个 & 不是有效操作符
            free(token);
            lexer->has_error = true;
            lexer->error_message = "Unexpected character '&'";
            return NULL;
            
        case '|':
            if (next == '|') {
                token->kind = TOKEN_OR;
                token->value = strdup("||");
                token->length = 2;
                lexer_advance(lexer);
                lexer_advance(lexer);
                return token;
            }
            token->kind = TOKEN_PIPE;
            token->value = strdup("|");
            token->length = 1;
            lexer_advance(lexer);
            return token;
            
        case '+':
            token->kind = TOKEN_PLUS;
            token->value = strdup("+");
            token->length = 1;
            lexer_advance(lexer);
            return token;
            
        case '*':
            token->kind = TOKEN_MUL;
            token->value = strdup("*");
            token->length = 1;
            lexer_advance(lexer);
            return token;
            
        case '/':
            token->kind = TOKEN_DIV;
            token->value = strdup("/");
            token->length = 1;
            lexer_advance(lexer);
            return token;
            
        case '%':
            token->kind = TOKEN_MOD;
            token->value = strdup("%");
            token->length = 1;
            lexer_advance(lexer);
            return token;
            
        case '(':
            token->kind = TOKEN_LPAREN;
            token->value = strdup("(");
            token->length = 1;
            lexer_advance(lexer);
            return token;
            
        case ')':
            token->kind = TOKEN_RPAREN;
            token->value = strdup(")");
            token->length = 1;
            lexer_advance(lexer);
            return token;
            
        case '{':
            token->kind = TOKEN_LBRACE;
            token->value = strdup("{");
            token->length = 1;
            lexer_advance(lexer);
            return token;
            
        case '}':
            token->kind = TOKEN_RBRACE;
            token->value = strdup("}");
            token->length = 1;
            lexer_advance(lexer);
            return token;
            
        case '[':
            token->kind = TOKEN_LBRACKET;
            token->value = strdup("[");
            token->length = 1;
            lexer_advance(lexer);
            return token;
            
        case ']':
            token->kind = TOKEN_RBRACKET;
            token->value = strdup("]");
            token->length = 1;
            lexer_advance(lexer);
            return token;
            
        case ',':
            token->kind = TOKEN_COMMA;
            token->value = strdup(",");
            token->length = 1;
            lexer_advance(lexer);
            return token;
            
        case ';':
            token->kind = TOKEN_SEMICOLON;
            token->value = strdup(";");
            token->length = 1;
            lexer_advance(lexer);
            return token;
            
        case ':':
            token->kind = TOKEN_COLON;
            token->value = strdup(":");
            token->length = 1;
            lexer_advance(lexer);
            return token;
            
        case '.':
            token->kind = TOKEN_DOT;
            token->value = strdup(".");
            token->length = 1;
            lexer_advance(lexer);
            return token;
            
        case '_':
            token->kind = TOKEN_UNDERSCORE;
            token->value = strdup("_");
            token->length = 1;
            lexer_advance(lexer);
            return token;
            
        case '?':
            token->kind = TOKEN_QUESTION;
            token->value = strdup("?");
            token->length = 1;
            lexer_advance(lexer);
            return token;
            
        case '@':
            token->kind = TOKEN_AT;
            token->value = strdup("@");
            token->length = 1;
            lexer_advance(lexer);
            return token;
            
        case '#':
            token->kind = TOKEN_HASH;
            token->value = strdup("#");
            token->length = 1;
            lexer_advance(lexer);
            return token;
            
        // Unicode 操作符
        case 0x03A3: // Σ (Greek Capital Letter Sigma)
        case 0x2211: // ∑ (N-Ary Summation)
            token->kind = TOKEN_SIGMA;
            token->value = strdup("Σ");
            token->length = 1;
            lexer_advance(lexer);
            return token;
            
        case 0x03A0: // Π (Greek Capital Letter Pi)
        case 0x220F: // ∏ (N-Ary Product)
            token->kind = TOKEN_PI;
            token->value = strdup("Π");
            token->length = 1;
            lexer_advance(lexer);
            return token;
            
        case 0x03BB: // λ (Greek Small Letter Lambda)
            token->kind = TOKEN_LAMBDA;
            token->value = strdup("λ");
            token->length = 1;
            lexer_advance(lexer);
            return token;
            
        case 0x00D7: // × (Multiplication Sign)
            token->kind = TOKEN_TIMES;
            token->value = strdup("×");
            token->length = 1;
            lexer_advance(lexer);
            return token;
            
        default:
            free(token);
            lexer->has_error = true;
            char error_msg[64];
            snprintf(error_msg, sizeof(error_msg), "Unexpected character: '%c' (0x%02X)", c, (unsigned char)c);
            lexer->error_message = strdup(error_msg);
            return NULL;
    }
}

// ========== 公共接口实现 ==========

Lexer* lexer_create(const char* source) {
    if (!source) {
        return NULL;
    }
    
    Lexer* lexer = (Lexer*)malloc(sizeof(Lexer));
    if (!lexer) {
        return NULL;
    }
    
    lexer->source = source;
    lexer->source_length = strlen(source);
    lexer->position = 0;
    lexer->line = 1;
    lexer->column = 1;
    lexer->current_token = NULL;
    lexer->has_error = false;
    lexer->error_message = NULL;
    lexer->owns_source = false;  // lexer_create 不拥有 source 的所有权
    
    return lexer;
}

void lexer_free(Lexer* lexer) {
    if (!lexer) {
        return;
    }
    
    if (lexer->current_token) {
        token_free(lexer->current_token);
    }
    
    if (lexer->error_message) {
        free(lexer->error_message);
    }
    
    // 如果 lexer 拥有 source 的所有权，释放它
    if (lexer->owns_source && lexer->source) {
        free((void*)lexer->source);
    }
    
    free(lexer);
}

Token* lexer_peek(Lexer* lexer) {
    if (!lexer || lexer->has_error) {
        return NULL;
    }
    
    // 如果已经有缓存的 Token，直接返回
    if (lexer->current_token) {
        return lexer->current_token;
    }
    
    // 否则读取下一个 Token
    return lexer_next(lexer);
}

Token* lexer_next(Lexer* lexer) {
    if (!lexer || lexer->has_error) {
        return NULL;
    }
    
    // 如果已经有缓存的 Token，释放它
    if (lexer->current_token) {
        token_free(lexer->current_token);
        lexer->current_token = NULL;
    }
    
    // 跳过空白和注释
    while (lexer->position < lexer->source_length) {
        lexer_skip_whitespace(lexer);
        
        if (lexer->position >= lexer->source_length) {
            break;
        }
        
        char c = lexer_current(lexer);
        
        // 检查单行注释
        if (c == '-' && lexer_peek_char(lexer, 1) == '-') {
            lex_single_line_comment(lexer);
            continue;
        }
        
        // 检查多行注释
        if (c == '{' && lexer_peek_char(lexer, 1) == '-') {
            if (!lex_multi_line_comment(lexer)) {
                return NULL;
            }
            continue;
        }
        
        break;
    }
    
    // 检查是否到达文件末尾
    if (lexer->position >= lexer->source_length) {
        Token* eof = (Token*)malloc(sizeof(Token));
        if (!eof) {
            lexer->has_error = true;
            lexer->error_message = "Memory allocation failed";
            return NULL;
        }
        eof->kind = TOKEN_EOF;
        eof->value = NULL;
        eof->length = 0;
        eof->line = lexer->line;
        eof->column = lexer->column;
        eof->position = lexer->position;
        lexer->current_token = eof;
        return eof;
    }
    
    // 读取 Token
    char c = lexer_current(lexer);
    Token* token = NULL;
    
    // 调试：记录当前位置
    if ((unsigned char)c >= 0x80) {
        fprintf(stderr, "[LEXER DEBUG] At position %zu, found UTF-8 byte: 0x%02X\n", 
                lexer->position, (unsigned char)c);
    }
    
    if (is_identifier_start(c)) {
        token = lex_identifier(lexer);
    } else if (is_digit(c)) {
        token = lex_number(lexer);
    } else if (c == '"') {
        token = lex_string(lexer);
    } else {
        token = lex_operator_or_punctuation(lexer);
    }
    
    if (token) {
        lexer->current_token = token;
        // 调试：记录生成的 token
        if (token->kind == TOKEN_IDENTIFIER && token->value && strlen(token->value) > 0) {
            unsigned char first_byte = (unsigned char)token->value[0];
            if (first_byte >= 0x80) {
                fprintf(stderr, "[LEXER DEBUG] Generated identifier token with UTF-8: %s (length=%zu, position=%zu->%zu)\n",
                        token->value, token->length, token->position, lexer->position);
            }
        }
    } else {
        // 调试：如果 token 是 NULL，记录原因
        fprintf(stderr, "[LEXER DEBUG] Failed to generate token at position %zu, char: 0x%02X\n",
                lexer->position, (unsigned char)c);
    }
    
    return token;
}

bool lexer_is_eof(Lexer* lexer) {
    if (!lexer) {
        return true;
    }
    
    Token* token = lexer_peek(lexer);
    return token && token->kind == TOKEN_EOF;
}

int lexer_get_line(Lexer* lexer) {
    return lexer ? lexer->line : 0;
}

int lexer_get_column(Lexer* lexer) {
    return lexer ? lexer->column : 0;
}

bool lexer_has_error(Lexer* lexer) {
    return lexer && lexer->has_error;
}

const char* lexer_get_error(Lexer* lexer) {
    if (!lexer || !lexer->has_error) {
        return NULL;
    }
    return lexer->error_message;
}

// ========== Token 操作实现 ==========

void token_free(Token* token) {
    if (!token) {
        return;
    }
    
    if (token->value) {
        free(token->value);
    }
    
    free(token);
}

Token* token_copy(Token* token) {
    if (!token) {
        return NULL;
    }
    
    Token* copy = (Token*)malloc(sizeof(Token));
    if (!copy) {
        return NULL;
    }
    
    copy->kind = token->kind;
    copy->length = token->length;
    copy->line = token->line;
    copy->column = token->column;
    copy->position = token->position;
    
    if (token->value) {
        copy->value = (char*)malloc(token->length + 1);
        if (!copy->value) {
            free(copy);
            return NULL;
        }
        memcpy(copy->value, token->value, token->length + 1);
    } else {
        copy->value = NULL;
    }
    
    return copy;
}

const char* token_kind_name(token_kind kind) {
    switch (kind) {
        case TOKEN_EOF: return "EOF";
        case TOKEN_IDENTIFIER: return "IDENTIFIER";
        case TOKEN_NUMBER: return "NUMBER";
        case TOKEN_STRING: return "STRING";
        case TOKEN_MODULE: return "MODULE";
        case TOKEN_WHERE: return "WHERE";
        case TOKEN_IMPORT: return "IMPORT";
        case TOKEN_TYPE: return "TYPE";
        case TOKEN_DEF: return "DEF";
        case TOKEN_LET: return "LET";
        case TOKEN_IN: return "IN";
        case TOKEN_IF: return "IF";
        case TOKEN_THEN: return "THEN";
        case TOKEN_ELSE: return "ELSE";
        case TOKEN_MATCH: return "MATCH";
        case TOKEN_WITH: return "WITH";
        case TOKEN_SIGMA: return "SIGMA";
        case TOKEN_PI: return "PI";
        case TOKEN_LAMBDA: return "LAMBDA";
        case TOKEN_ARROW: return "ARROW";
        case TOKEN_PLUS: return "PLUS";
        case TOKEN_MINUS: return "MINUS";
        case TOKEN_MUL: return "MUL";
        case TOKEN_DIV: return "DIV";
        case TOKEN_EQ: return "EQ";
        case TOKEN_NE: return "NE";
        case TOKEN_LT: return "LT";
        case TOKEN_GT: return "GT";
        case TOKEN_LPAREN: return "LPAREN";
        case TOKEN_RPAREN: return "RPAREN";
        case TOKEN_COMMA: return "COMMA";
        case TOKEN_COLON: return "COLON";
        case TOKEN_DOT: return "DOT";
        case TOKEN_PROP: return "PROP";
        case TOKEN_U: return "U";
        default: return "UNKNOWN";
    }
}

bool token_is_keyword(Token* token) {
    if (!token) {
        return false;
    }
    
    return token->kind >= TOKEN_MODULE && token->kind <= TOKEN_AS;
}

bool token_is_operator(Token* token) {
    if (!token) {
        return false;
    }
    
    return (token->kind >= TOKEN_SIGMA && token->kind <= TOKEN_MOD) ||
           (token->kind >= TOKEN_EQ && token->kind <= TOKEN_GE);
}

bool token_is_punctuation(Token* token) {
    if (!token) {
        return false;
    }
    
    return token->kind >= TOKEN_LPAREN && token->kind <= TOKEN_PIPE;
}

// ========== 工具函数实现 ==========

Lexer* lexer_from_file(const char* filename) {
    FILE* file = fopen(filename, "rb");
    if (!file) {
        return NULL;
    }
    
    // 获取文件大小
    fseek(file, 0, SEEK_END);
    long size = ftell(file);
    fseek(file, 0, SEEK_SET);
    
    // 读取文件内容
    char* buffer = (char*)malloc(size + 1);
    if (!buffer) {
        fclose(file);
        return NULL;
    }
    
    size_t read = fread(buffer, 1, size, file);
    buffer[read] = '\0';
    fclose(file);
    
    // 创建词法分析器
    Lexer* lexer = lexer_create(buffer);
    if (!lexer) {
        free(buffer);
        return NULL;
    }
    
    // 标记 lexer 拥有 buffer 的所有权，释放时会自动释放
    lexer->owns_source = true;
    
    return lexer;
}

Token** lexer_tokenize_all(Lexer* lexer, size_t* count) {
    if (!lexer || !count) {
        return NULL;
    }
    
    size_t capacity = 256;
    size_t size = 0;
    Token** tokens = (Token**)malloc(capacity * sizeof(Token*));
    if (!tokens) {
        return NULL;
    }
    
    while (!lexer_is_eof(lexer) && !lexer_has_error(lexer)) {
        Token* token = lexer_next(lexer);
        if (!token) {
            break;
        }
        
        if (token->kind == TOKEN_EOF) {
            tokens[size++] = token;
            break;
        }
        
        tokens[size++] = token_copy(token);
        
        if (size >= capacity) {
            capacity *= 2;
            Token** new_tokens = (Token**)realloc(tokens, capacity * sizeof(Token*));
            if (!new_tokens) {
                // 释放已分配的 tokens
                for (size_t i = 0; i < size; i++) {
                    token_free(tokens[i]);
                }
                free(tokens);
                return NULL;
            }
            tokens = new_tokens;
        }
    }
    
    *count = size;
    return tokens;
}

void token_array_free(Token** tokens, size_t count) {
    if (!tokens) {
        return;
    }
    
    for (size_t i = 0; i < count; i++) {
        token_free(tokens[i]);
    }
    
    free(tokens);
}

