// compiler/src/compiler/parser.c
// KOS-TL 语法分析器实现

#include "compiler/parser.h"
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

// ========== 前向声明 ==========

// 这些函数在头文件中已声明，这里只是前向声明
ASTNode* parser_parse_lambda(Parser* parser);
ASTNode* parser_parse_let(Parser* parser);
ASTNode* parser_parse_if(Parser* parser);
ASTNode* parser_parse_match(Parser* parser);
ASTNode* parser_parse_pi_type(Parser* parser);
ASTNode* parser_parse_sigma_type(Parser* parser);
// 内部函数
static ASTNode* parse_atom(Parser* parser);
static ASTNode* parse_application(Parser* parser);
static ASTNode* parse_type_annotation(Parser* parser);

// ========== 辅助函数实现 ==========

static void parser_error_internal(Parser* parser, const char* message) {
    if (!parser) {
        return;
    }
    
    parser->has_error = true;
    if (parser->error_message) {
        free(parser->error_message);
    }
    
    if (parser->current_token) {
        char* error_msg = (char*)malloc(256);
        if (error_msg) {
            snprintf(error_msg, 256, "%s at line %d, column %d: %s",
                    message,
                    parser->current_token->line,
                    parser->current_token->column,
                    parser->current_token->value ? parser->current_token->value : "");
            parser->error_message = error_msg;
        } else {
            parser->error_message = strdup(message);
        }
    } else {
        parser->error_message = strdup(message);
    }
}

Token* parser_advance(Parser* parser) {
    if (!parser || !parser->lexer) {
        return NULL;
    }
    
    parser->current_token = lexer_next(parser->lexer);
    return parser->current_token;
}

Token* parser_peek(Parser* parser) {
    if (!parser || !parser->lexer) {
        return NULL;
    }
    
    if (!parser->current_token) {
        parser->current_token = lexer_peek(parser->lexer);
    }
    return parser->current_token;
}

bool parser_check(Parser* parser, token_kind kind) {
    Token* token = parser_peek(parser);
    return token && token->kind == kind;
}

bool parser_expect(Parser* parser, token_kind kind, const char* error_msg) {
    Token* token = parser_peek(parser);
    if (!token || token->kind != kind) {
        parser_error_internal(parser, error_msg);
        return false;
    }
    parser_advance(parser);
    return true;
}

// ========== AST 节点操作 ==========

ASTNode* ast_node_create(ast_node_kind kind, Token* token) {
    ASTNode* node = (ASTNode*)calloc(1, sizeof(ASTNode));
    if (!node) {
        return NULL;
    }
    
    node->kind = kind;
    node->token = token ? token_copy(token) : NULL;
    
    return node;
}

void ast_node_free(ASTNode* node) {
    if (!node) {
        return;
    }
    
    if (node->token) {
        token_free(node->token);
    }
    
    switch (node->kind) {
        case AST_LAMBDA:
        case AST_PI_TYPE:
            if (node->param_name) free(node->param_name);
            if (node->param_type) ast_node_free(node->param_type);
            if (node->lambda_body) ast_node_free(node->lambda_body);
            break;
            
        case AST_TYPE_DECL:
            if (node->name) free(node->name);
            if (node->param_type) ast_node_free(node->param_type); // 类型表达式
            break;
            
        case AST_DEF:
            if (node->name) free(node->name);
            if (node->param_type) ast_node_free(node->param_type); // 类型注解
            if (node->lambda_body) ast_node_free(node->lambda_body); // 函数体
            break;
            
        case AST_SIGMA_TYPE:
            if (node->first_name) free(node->first_name);
            if (node->first_type) ast_node_free(node->first_type);
            if (node->sigma_second_type) ast_node_free(node->sigma_second_type);
            break;
            
        case AST_APPLICATION:
        case AST_FUNCTION_TYPE:
        case AST_TYPE_ANNOTATION:
            if (node->function) ast_node_free(node->function);
            if (node->argument) ast_node_free(node->argument);
            break;
            
        case AST_MATCH:
            if (node->matched_expr) ast_node_free(node->matched_expr);
            if (node->cases) {
                for (size_t i = 0; i < node->case_count; i++) {
                    ast_node_free(node->cases[i]);
                }
                free(node->cases);
            }
            break;
            
        case AST_IF:
            if (node->condition) ast_node_free(node->condition);
            if (node->then_expr) ast_node_free(node->then_expr);
            if (node->else_expr) ast_node_free(node->else_expr);
            break;
            
        case AST_LET:
            if (node->var_name) free(node->var_name);
            if (node->var_value) ast_node_free(node->var_value);
            if (node->let_body) ast_node_free(node->let_body);
            break;
            
        case AST_MODULE:
            if (node->module_name) free(node->module_name);
            if (node->declarations) {
                for (size_t i = 0; i < node->decl_count; i++) {
                    ast_node_free(node->declarations[i]);
                }
                free(node->declarations);
            }
            break;
            
        case AST_SEQUENCE:
        case AST_TUPLE:
            if (node->elements) {
                for (size_t i = 0; i < node->element_count; i++) {
                    ast_node_free(node->elements[i]);
                }
                free(node->elements);
            }
            break;
            
        case AST_VARIABLE:
            if (node->name) free(node->name);
            break;
            
        case AST_LITERAL:
            if (node->value) free(node->value);
            break;
            
        default:
            break;
    }
    
    free(node);
}

const char* ast_node_kind_name(ast_node_kind kind) {
    switch (kind) {
        case AST_MODULE: return "MODULE";
        case AST_IMPORT: return "IMPORT";
        case AST_TYPE_DECL: return "TYPE_DECL";
        case AST_DEF: return "DEF";
        case AST_LAMBDA: return "LAMBDA";
        case AST_APPLICATION: return "APPLICATION";
        case AST_VARIABLE: return "VARIABLE";
        case AST_LITERAL: return "LITERAL";
        case AST_TYPE_ANNOTATION: return "TYPE_ANNOTATION";
        case AST_FUNCTION_TYPE: return "FUNCTION_TYPE";
        case AST_PI_TYPE: return "PI_TYPE";
        case AST_SIGMA_TYPE: return "SIGMA_TYPE";
        case AST_UNIVERSE: return "UNIVERSE";
        case AST_PROP: return "PROP";
        case AST_MATCH: return "MATCH";
        case AST_CASE: return "CASE";
        case AST_PATTERN: return "PATTERN";
        case AST_IF: return "IF";
        case AST_PAIR: return "PAIR";
        case AST_LET: return "LET";
        default: return "UNKNOWN";
    }
}

void ast_print(ASTNode* node, int indent) {
    if (!node) {
        return;
    }
    
    for (int i = 0; i < indent; i++) {
        printf("  ");
    }
    
    printf("%s", ast_node_kind_name(node->kind));
    
    switch (node->kind) {
        case AST_VARIABLE:
            if (node->name) {
                printf(": %s", node->name);
            }
            break;
            
        case AST_LITERAL:
            if (node->value) {
                printf(": %s", node->value);
            }
            break;
            
        case AST_MODULE:
            if (node->module_name) {
                printf(": %s", node->module_name);
            }
            break;
            
        default:
            break;
    }
    
    printf("\n");
    
    // 递归打印子节点
    switch (node->kind) {
        case AST_LAMBDA:
        case AST_PI_TYPE:
            if (node->param_type) ast_print(node->param_type, indent + 1);
            if (node->lambda_body) ast_print(node->lambda_body, indent + 1);
            break;
            
        case AST_APPLICATION:
        case AST_FUNCTION_TYPE:
        case AST_TYPE_ANNOTATION:
            if (node->function) ast_print(node->function, indent + 1);
            if (node->argument) ast_print(node->argument, indent + 1);
            break;
            
        case AST_IF:
            if (node->condition) ast_print(node->condition, indent + 1);
            if (node->then_expr) ast_print(node->then_expr, indent + 1);
            if (node->else_expr) ast_print(node->else_expr, indent + 1);
            break;
            
        case AST_LET:
            if (node->var_value) ast_print(node->var_value, indent + 1);
            if (node->let_body) ast_print(node->let_body, indent + 1);
            break;
            
        case AST_MATCH:
            if (node->matched_expr) ast_print(node->matched_expr, indent + 1);
            if (node->cases) {
                for (size_t i = 0; i < node->case_count; i++) {
                    ast_print(node->cases[i], indent + 1);
                }
            }
            break;
            
        case AST_MODULE:
            if (node->declarations) {
                for (size_t i = 0; i < node->decl_count; i++) {
                    ast_print(node->declarations[i], indent + 1);
                }
            }
            break;
            
        default:
            break;
    }
}

// ========== 语法分析器创建和释放 ==========

Parser* parser_create(Lexer* lexer) {
    if (!lexer) {
        return NULL;
    }
    
    Parser* parser = (Parser*)calloc(1, sizeof(Parser));
    if (!parser) {
        return NULL;
    }
    
    parser->lexer = lexer;
    parser->current_token = NULL;
    parser->has_error = false;
    parser->error_message = NULL;
    parser->root = NULL;
    
    // 读取第一个 Token
    parser_advance(parser);
    
    return parser;
}

void parser_free(Parser* parser) {
    if (!parser) {
        return;
    }
    
    // 注意：不释放 root，因为 root 通常由调用者管理（通过 parser_parse_program 的返回值）
    // 如果调用者已经释放了 program，再次释放会导致双重释放
    // parser->root 只是保存了指向 program 的指针，不拥有所有权
    
    if (parser->error_message) {
        free(parser->error_message);
    }
    
    // 注意：不释放 lexer，由调用者管理
    free(parser);
}

// ========== 表达式解析（优先级从低到高） ==========

// 解析原子表达式（最高优先级）
static ASTNode* parse_atom(Parser* parser) {
    Token* token = parser_peek(parser);
    if (!token) {
        parser_error_internal(parser, "Unexpected end of file");
        return NULL;
    }
    
    ASTNode* node = NULL;
    
    switch (token->kind) {
        case TOKEN_IDENTIFIER: {
            node = ast_node_create(AST_VARIABLE, token);
            node->name = strdup(token->value);
            parser_advance(parser);
            break;
        }
        
        case TOKEN_NUMBER:
        case TOKEN_STRING: {
            node = ast_node_create(AST_LITERAL, token);
            node->value = strdup(token->value);
            parser_advance(parser);
            break;
        }
        
        case TOKEN_LPAREN: {
            parser_advance(parser); // 跳过 '('
            node = parser_parse_expr(parser);
            if (!parser_expect(parser, TOKEN_RPAREN, "Expected ')'")) {
                if (node) ast_node_free(node);
                return NULL;
            }
            break;
        }
        
        case TOKEN_LAMBDA: {
            return parser_parse_lambda(parser);
        }
        
        case TOKEN_LET: {
            return parser_parse_let(parser);
        }
        
        case TOKEN_IF: {
            return parser_parse_if(parser);
        }
        
        case TOKEN_MATCH: {
            return parser_parse_match(parser);
        }
        
        default:
            parser_error_internal(parser, "Unexpected token in expression");
            return NULL;
    }
    
    return node;
}

// 解析函数应用（左结合）
static ASTNode* parse_application(Parser* parser) {
    ASTNode* left = parse_atom(parser);
    if (!left) {
        return NULL;
    }
    
    // 继续解析应用
    while (true) {
        Token* token = parser_peek(parser);
        if (!token) {
            break;
        }
        
        // 检查是否是应用（下一个是原子表达式）
        if (token->kind == TOKEN_IDENTIFIER ||
            token->kind == TOKEN_NUMBER ||
            token->kind == TOKEN_STRING ||
            token->kind == TOKEN_LPAREN ||
            token->kind == TOKEN_LAMBDA ||
            token->kind == 0x03BB) {
            
            ASTNode* right = parse_atom(parser);
            if (!right) {
                break;
            }
            
            ASTNode* app = ast_node_create(AST_APPLICATION, token);
            app->function = left;
            app->argument = right;
            left = app;
        } else {
            break;
        }
    }
    
    return left;
}

// 解析类型注解
static ASTNode* parse_type_annotation(Parser* parser) {
    ASTNode* expr = parse_application(parser);
    if (!expr) {
        return NULL;
    }
    
    if (parser_check(parser, TOKEN_COLON)) {
        parser_advance(parser);
        ASTNode* type = parser_parse_type(parser);
        if (!type) {
            ast_node_free(expr);
            return NULL;
        }
        
        ASTNode* annotated = ast_node_create(AST_TYPE_ANNOTATION, parser->current_token);
        annotated->function = expr;
        annotated->argument = type;
        return annotated;
    }
    
    return expr;
}

// 解析 Lambda 表达式
ASTNode* parser_parse_lambda(Parser* parser) {
    Token* lambda_token = parser_peek(parser);
    parser_advance(parser); // 跳过 λ 或 lambda
    
    // 解析参数
    if (!parser_expect(parser, TOKEN_LPAREN, "Expected '(' after lambda")) {
        return NULL;
    }
    
    if (!parser_expect(parser, TOKEN_IDENTIFIER, "Expected parameter name")) {
        return NULL;
    }
    
    Token* param_token = parser->current_token;
    char* param_name = strdup(param_token->value);
    
    ASTNode* param_type = NULL;
    if (parser_check(parser, TOKEN_COLON)) {
        parser_advance(parser);
        param_type = parser_parse_type(parser);
        if (!param_type) {
            free(param_name);
            return NULL;
        }
    }
    
    if (!parser_expect(parser, TOKEN_RPAREN, "Expected ')' after parameter")) {
        free(param_name);
        if (param_type) ast_node_free(param_type);
        return NULL;
    }
    
    // 解析箭头
    if (!parser_check(parser, TOKEN_ARROW) && 
        !parser_check(parser, 0x2192)) { // Unicode →
        parser_error_internal(parser, "Expected '→' or '->' after lambda parameter");
        free(param_name);
        if (param_type) ast_node_free(param_type);
        return NULL;
    }
    parser_advance(parser);
    
    // 解析函数体
    ASTNode* body = parser_parse_expr(parser);
    if (!body) {
        free(param_name);
        if (param_type) ast_node_free(param_type);
        return NULL;
    }
    
    ASTNode* lambda = ast_node_create(AST_LAMBDA, lambda_token);
    lambda->param_name = param_name;
    lambda->param_type = param_type;
    lambda->lambda_body = body;
    
    return lambda;
}

// 解析 Let 表达式
ASTNode* parser_parse_let(Parser* parser) {
    Token* let_token = parser_peek(parser);
    parser_advance(parser); // 跳过 let
    
    if (!parser_expect(parser, TOKEN_IDENTIFIER, "Expected variable name after 'let'")) {
        return NULL;
    }
    
    Token* var_token = parser->current_token;
    char* var_name = strdup(var_token->value);
    
    if (!parser_expect(parser, TOKEN_EQ, "Expected '=' after variable name")) {
        free(var_name);
        return NULL;
    }
    
    ASTNode* var_value = parser_parse_expr(parser);
    if (!var_value) {
        free(var_name);
        return NULL;
    }
    
    if (!parser_expect(parser, TOKEN_IN, "Expected 'in' after let binding")) {
        free(var_name);
        ast_node_free(var_value);
        return NULL;
    }
    
    ASTNode* body = parser_parse_expr(parser);
    if (!body) {
        free(var_name);
        ast_node_free(var_value);
        return NULL;
    }
    
    ASTNode* let = ast_node_create(AST_LET, let_token);
    let->var_name = var_name;
    let->var_value = var_value;
    let->let_body = body;
    
    return let;
}

// 解析 If 表达式
ASTNode* parser_parse_if(Parser* parser) {
    Token* if_token = parser_peek(parser);
    parser_advance(parser); // 跳过 if
    
    ASTNode* condition = parser_parse_expr(parser);
    if (!condition) {
        return NULL;
    }
    
    if (!parser_expect(parser, TOKEN_THEN, "Expected 'then' after condition")) {
        ast_node_free(condition);
        return NULL;
    }
    
    ASTNode* then_expr = parser_parse_expr(parser);
    if (!then_expr) {
        ast_node_free(condition);
        return NULL;
    }
    
    if (!parser_expect(parser, TOKEN_ELSE, "Expected 'else' after then branch")) {
        ast_node_free(condition);
        ast_node_free(then_expr);
        return NULL;
    }
    
    ASTNode* else_expr = parser_parse_expr(parser);
    if (!else_expr) {
        ast_node_free(condition);
        ast_node_free(then_expr);
        return NULL;
    }
    
    ASTNode* if_expr = ast_node_create(AST_IF, if_token);
    if_expr->condition = condition;
    if_expr->then_expr = then_expr;
    if_expr->else_expr = else_expr;
    
    return if_expr;
}

// 解析 Match 表达式
ASTNode* parser_parse_match(Parser* parser) {
    Token* match_token = parser_peek(parser);
    parser_advance(parser); // 跳过 match
    
    ASTNode* matched_expr = parser_parse_expr(parser);
    if (!matched_expr) {
        return NULL;
    }
    
    if (!parser_expect(parser, TOKEN_WITH, "Expected 'with' after match expression")) {
        ast_node_free(matched_expr);
        return NULL;
    }
    
    // 解析 case 列表
    size_t capacity = 4;
    size_t count = 0;
    ASTNode** cases = (ASTNode**)malloc(capacity * sizeof(ASTNode*));
    if (!cases) {
        ast_node_free(matched_expr);
        return NULL;
    }
    
    while (true) {
        if (parser_check(parser, TOKEN_PIPE) || parser_check(parser, TOKEN_CASE)) {
            if (parser_check(parser, TOKEN_PIPE)) {
                parser_advance(parser);
            } else {
                parser_advance(parser); // 跳过 case
            }
            
            ASTNode* pattern = parser_parse_pattern(parser);
            if (!pattern) {
                // 释放已解析的 cases
                for (size_t i = 0; i < count; i++) {
                    ast_node_free(cases[i]);
                }
                free(cases);
                ast_node_free(matched_expr);
                return NULL;
            }
            
            if (!parser_expect(parser, TOKEN_DARROW, "Expected '=>' after pattern")) {
                ast_node_free(pattern);
                for (size_t i = 0; i < count; i++) {
                    ast_node_free(cases[i]);
                }
                free(cases);
                ast_node_free(matched_expr);
                return NULL;
            }
            
            ASTNode* expr = parser_parse_expr(parser);
            if (!expr) {
                ast_node_free(pattern);
                for (size_t i = 0; i < count; i++) {
                    ast_node_free(cases[i]);
                }
                free(cases);
                ast_node_free(matched_expr);
                return NULL;
            }
            
            // 创建 case 节点
            ASTNode* case_node = ast_node_create(AST_CASE, parser->current_token);
            case_node->left = pattern;
            case_node->right = expr;
            
            if (count >= capacity) {
                capacity *= 2;
                ASTNode** new_cases = (ASTNode**)realloc(cases, capacity * sizeof(ASTNode*));
                if (!new_cases) {
                    ast_node_free(case_node);
                    for (size_t i = 0; i < count; i++) {
                        ast_node_free(cases[i]);
                    }
                    free(cases);
                    ast_node_free(matched_expr);
                    return NULL;
                }
                cases = new_cases;
            }
            
            cases[count++] = case_node;
        } else {
            break;
        }
    }
    
    ASTNode* match = ast_node_create(AST_MATCH, match_token);
    match->matched_expr = matched_expr;
    match->cases = cases;
    match->case_count = count;
    
    return match;
}

// 解析模式
ASTNode* parser_parse_pattern(Parser* parser) {
    Token* token = parser_peek(parser);
    if (!token) {
        parser_error_internal(parser, "Unexpected end of file in pattern");
        return NULL;
    }
    
    ASTNode* pattern = NULL;
    
    switch (token->kind) {
        case TOKEN_IDENTIFIER:
        case TOKEN_UNDERSCORE: {
            pattern = ast_node_create(AST_PATTERN, token);
            pattern->name = strdup(token->value);
            parser_advance(parser);
            break;
        }
        
        case TOKEN_LPAREN: {
            parser_advance(parser);
            // 解析元组模式
            pattern = ast_node_create(AST_PATTERN, token);
            // TODO: 实现元组模式解析
            if (!parser_expect(parser, TOKEN_RPAREN, "Expected ')' in pattern")) {
                ast_node_free(pattern);
                return NULL;
            }
            break;
        }
        
        default:
            parser_error_internal(parser, "Unexpected token in pattern");
            return NULL;
    }
    
    return pattern;
}

// 主表达式解析函数
ASTNode* parser_parse_expr(Parser* parser) {
    return parse_type_annotation(parser);
}

// ========== 类型解析 ==========

ASTNode* parser_parse_type(Parser* parser) {
    Token* token = parser_peek(parser);
    if (!token) {
        parser_error_internal(parser, "Unexpected end of file in type");
        return NULL;
    }
    
    ASTNode* type = NULL;
    
    switch (token->kind) {
        case TOKEN_U: {
            type = ast_node_create(AST_UNIVERSE, token);
            parser_advance(parser);
            // 解析下标（U₁, U₂, ...）
            // 检查下一个 token 是否是标识符（可能是下标字符）
            Token* next_token = parser_peek(parser);
            if (next_token && next_token->kind == TOKEN_IDENTIFIER && next_token->value) {
                size_t value_len = strlen(next_token->value);
                // 检查是否是下标字符（Unicode 下标数字范围：U+2080-U+2089）
                // UTF-8 编码：₁ = 0xE2 0x82 0x81, ₂ = 0xE2 0x82 0x82, etc.
                bool is_subscript = false;
                if (value_len >= 3 && 
                    (unsigned char)next_token->value[0] == 0xE2 &&
                    (unsigned char)next_token->value[1] == 0x82 &&
                    (unsigned char)next_token->value[2] >= 0x80 &&
                    (unsigned char)next_token->value[2] <= 0x89) {
                    is_subscript = true;
                } else if (value_len == 1 && (unsigned char)next_token->value[0] >= 0x80) {
                    // 可能是其他 Unicode 字符，也跳过（容错处理）
                    is_subscript = true;
                }
                
                if (is_subscript) {
                    // 是下标字符，跳过它
                    fprintf(stderr, "[PARSER DEBUG] Skipping subscript character: %s\n", next_token->value);
                    parser_advance(parser);
                    // 确保词法分析器正确前进
                    // 如果下一个 token 仍然是 NULL，可能是词法分析器的问题
                    Token* after_subscript = parser_peek(parser);
                    if (!after_subscript && parser->lexer && 
                        parser->lexer->position < parser->lexer->source_length) {
                        // 词法分析器还没有到达文件末尾，但返回了 NULL
                        // 这可能是词法分析器的 bug，尝试强制读取下一个 token
                        fprintf(stderr, "[PARSER DEBUG] Lexer stuck, forcing advance...\n");
                        // 直接调用 lexer_next 来强制前进
                        lexer_next(parser->lexer);
                        after_subscript = parser_peek(parser);
                        if (after_subscript) {
                            fprintf(stderr, "[PARSER DEBUG] Forced token: kind=%d, value=%s\n",
                                    after_subscript->kind, after_subscript->value ? after_subscript->value : "(null)");
                        }
                    } else if (after_subscript) {
                        fprintf(stderr, "[PARSER DEBUG] After subscript, next token: kind=%d, value=%s\n",
                                after_subscript->kind, after_subscript->value ? after_subscript->value : "(null)");
                    }
                }
            }
            break;
        }
        
        case TOKEN_TYPE_KW: {
            type = ast_node_create(AST_UNIVERSE, token);
            parser_advance(parser);
            // 解析下标（Type₁, Type₂, ...）
            Token* next_token = parser_peek(parser);
            if (next_token && next_token->kind == TOKEN_IDENTIFIER && next_token->value) {
                size_t value_len = strlen(next_token->value);
                // 检查是否是下标字符
                if (value_len >= 3 && 
                    (unsigned char)next_token->value[0] == 0xE2 &&
                    (unsigned char)next_token->value[1] == 0x82 &&
                    (unsigned char)next_token->value[2] >= 0x80 &&
                    (unsigned char)next_token->value[2] <= 0x89) {
                    parser_advance(parser);
                } else if (value_len == 1 && (unsigned char)next_token->value[0] >= 0x80) {
                    parser_advance(parser);
                }
            }
            break;
        }
        
        case TOKEN_PROP: {
            type = ast_node_create(AST_PROP, token);
            parser_advance(parser);
            break;
        }
        
        case TOKEN_IDENTIFIER: {
            type = ast_node_create(AST_VARIABLE, token);
            type->name = strdup(token->value);
            parser_advance(parser);
            break;
        }
        
        case TOKEN_PI: {
            return parser_parse_pi_type(parser);
        }
        
        case TOKEN_SIGMA: {
            return parser_parse_sigma_type(parser);
        }
        
        case TOKEN_LPAREN: {
            parser_advance(parser);
            type = parser_parse_type(parser);
            if (!type) {
                return NULL;
            }
            
            // 检查是否是函数类型
            if (parser_check(parser, TOKEN_ARROW) || parser_check(parser, 0x2192)) {
                parser_advance(parser);
                ASTNode* return_type = parser_parse_type(parser);
                if (!return_type) {
                    ast_node_free(type);
                    return NULL;
                }
                
                ASTNode* func_type = ast_node_create(AST_FUNCTION_TYPE, token);
                func_type->function = type;
                func_type->argument = return_type;
                type = func_type;
            }
            
            if (!parser_expect(parser, TOKEN_RPAREN, "Expected ')' in type")) {
                ast_node_free(type);
                return NULL;
            }
            break;
        }
        
        default:
            parser_error_internal(parser, "Unexpected token in type");
            return NULL;
    }
    
    // 解析函数类型（右结合）
    while (parser_check(parser, TOKEN_ARROW)) {
        parser_advance(parser);
        ASTNode* return_type = parser_parse_type(parser);
        if (!return_type) {
            ast_node_free(type);
            return NULL;
        }
        
        ASTNode* func_type = ast_node_create(AST_FUNCTION_TYPE, token);
        func_type->function = type;
        func_type->argument = return_type;
        type = func_type;
    }
    
    return type;
}

// 解析 Pi 类型
ASTNode* parser_parse_pi_type(Parser* parser) {
    Token* pi_token = parser_peek(parser);
    parser_advance(parser); // 跳过 Π
    
    if (!parser_expect(parser, TOKEN_LPAREN, "Expected '(' after Π")) {
        return NULL;
    }
    
    if (!parser_expect(parser, TOKEN_IDENTIFIER, "Expected parameter name in Π type")) {
        return NULL;
    }
    
    Token* param_token = parser->current_token;
    char* param_name = strdup(param_token->value);
    
    if (!parser_expect(parser, TOKEN_COLON, "Expected ':' in Π type")) {
        free(param_name);
        return NULL;
    }
    
    ASTNode* param_type = parser_parse_type(parser);
    if (!param_type) {
        free(param_name);
        return NULL;
    }
    
    if (!parser_expect(parser, TOKEN_RPAREN, "Expected ')' after Π parameter")) {
        free(param_name);
        ast_node_free(param_type);
        return NULL;
    }
    
    if (!parser_check(parser, TOKEN_ARROW)) {
        parser_error_internal(parser, "Expected '→' or '->' after Π parameter");
        free(param_name);
        ast_node_free(param_type);
        return NULL;
    }
    parser_advance(parser);
    
    ASTNode* body_type = parser_parse_type(parser);
    if (!body_type) {
        free(param_name);
        ast_node_free(param_type);
        return NULL;
    }
    
    ASTNode* pi_type = ast_node_create(AST_PI_TYPE, pi_token);
    pi_type->param_name = param_name;
    pi_type->param_type = param_type;
    pi_type->lambda_body = body_type;
    
    return pi_type;
}

// 解析 Sigma 类型
ASTNode* parser_parse_sigma_type(Parser* parser) {
    Token* sigma_token = parser_peek(parser);
    parser_advance(parser); // 跳过 Σ
    
    if (!parser_expect(parser, TOKEN_LPAREN, "Expected '(' after Σ")) {
        return NULL;
    }
    
    if (!parser_expect(parser, TOKEN_IDENTIFIER, "Expected first component name in Σ type")) {
        return NULL;
    }
    
    Token* first_token = parser->current_token;
    char* first_name = strdup(first_token->value);
    
    if (!parser_expect(parser, TOKEN_COLON, "Expected ':' in Σ type")) {
        free(first_name);
        return NULL;
    }
    
    ASTNode* first_type = parser_parse_type(parser);
    if (!first_type) {
        free(first_name);
        return NULL;
    }
    
    if (!parser_expect(parser, TOKEN_RPAREN, "Expected ')' after first Σ component")) {
        free(first_name);
        ast_node_free(first_type);
        return NULL;
    }
    
    if (!parser_check(parser, TOKEN_TIMES)) {
        parser_error_internal(parser, "Expected '×' after first Σ component");
        free(first_name);
        ast_node_free(first_type);
        return NULL;
    }
    parser_advance(parser);
    
    ASTNode* second_type = parser_parse_type(parser);
    if (!second_type) {
        free(first_name);
        ast_node_free(first_type);
        return NULL;
    }
    
    ASTNode* sigma_type = ast_node_create(AST_SIGMA_TYPE, sigma_token);
    sigma_type->first_name = first_name;
    sigma_type->first_type = first_type;
    sigma_type->sigma_second_type = second_type;
    
    return sigma_type;
}

// ========== 声明解析 ==========

// 解析类型声明
ASTNode* parser_parse_type_decl(Parser* parser) {
    Token* type_token = parser_peek(parser);
    parser_advance(parser); // 跳过 type
    
    // 在调用 parser_expect 之前，先 peek 获取标识符 token 的值
    Token* name_token = parser_peek(parser);
    if (!name_token || name_token->kind != TOKEN_IDENTIFIER) {
        parser_error_internal(parser, "Expected type name after 'type'");
        return NULL;
    }
    
    // 保存类型名称（在 parser_expect 消耗 token 之前）
    char* type_name = strdup(name_token->value);
    
    // 现在调用 parser_expect 来验证并前进
    if (!parser_expect(parser, TOKEN_IDENTIFIER, "Expected type name after 'type'")) {
        free(type_name);
        return NULL;
    }
    
    ASTNode* type_expr = NULL;
    if (parser_check(parser, TOKEN_COLON)) {
        parser_advance(parser);
        // 保存错误状态
        bool had_error = parser->has_error;
        const char* old_error = parser->error_message ? strdup(parser->error_message) : NULL;
        
        type_expr = parser_parse_type(parser);
        
        // 调试：检查解析类型表达式后的 token
        Token* after_type_token = parser_peek(parser);
        if (after_type_token) {
            fprintf(stderr, "[PARSER DEBUG] After type expression, next token: kind=%d, value=%s, line=%d\n",
                    after_type_token->kind, after_type_token->value ? after_type_token->value : "(null)",
                    after_type_token->line);
        } else {
            fprintf(stderr, "[PARSER DEBUG] After type expression, next token is NULL (EOF?)\n");
            // 如果 token 是 NULL，可能是词法分析器的问题
            // 检查词法分析器是否真的到达了文件末尾
            if (parser->lexer) {
                fprintf(stderr, "[PARSER DEBUG] Lexer position: %zu/%zu, line: %d\n",
                        parser->lexer->position, parser->lexer->source_length, parser->lexer->line);
            }
        }
        
        // 如果类型表达式解析失败，清除错误状态，继续解析
        // 这样可以继续解析后续的声明
        if (!type_expr && parser->has_error) {
            fprintf(stderr, "[PARSER DEBUG] Type expression parsing failed: %s\n", 
                    parser->error_message ? parser->error_message : "(no message)");
            // 类型表达式解析失败，但不影响类型声明的创建
            // 清除错误状态，让解析器继续
            parser->has_error = had_error;
            if (parser->error_message) {
                free(parser->error_message);
                parser->error_message = NULL;
            }
            if (old_error) {
                // 恢复之前的错误（如果有）
                parser->error_message = old_error;
                parser->has_error = (old_error != NULL);
            } else {
                parser->has_error = false;
            }
        } else if (type_expr) {
            fprintf(stderr, "[PARSER DEBUG] Type expression parsed successfully\n");
        }
        
        // 清理 old_error（如果不再需要）
        if (old_error && !parser->has_error) {
            free((void*)old_error);
        }
    }
    
    ASTNode* decl = ast_node_create(AST_TYPE_DECL, type_token);
    decl->name = type_name;
    
    // 存储类型表达式（使用param_type字段）
    decl->param_type = type_expr;
    
    return decl;
}

// 解析函数定义
ASTNode* parser_parse_def(Parser* parser) {
    Token* def_token = parser_peek(parser);
    parser_advance(parser); // 跳过 def
    
    if (!parser_expect(parser, TOKEN_IDENTIFIER, "Expected function name after 'def'")) {
        return NULL;
    }
    
    Token* name_token = parser->current_token;
    char* func_name = strdup(name_token->value);
    
    ASTNode* type_expr = NULL;
    if (parser_check(parser, TOKEN_COLON)) {
        parser_advance(parser);
        type_expr = parser_parse_type(parser);
        if (!type_expr) {
            free(func_name);
            return NULL;
        }
    }
    
    if (!parser_expect(parser, TOKEN_EQ, "Expected '=' in function definition")) {
        free(func_name);
        if (type_expr) ast_node_free(type_expr);
        return NULL;
    }
    
    ASTNode* body = parser_parse_expr(parser);
    if (!body) {
        free(func_name);
        if (type_expr) ast_node_free(type_expr);
        return NULL;
    }
    
    ASTNode* def = ast_node_create(AST_DEF, def_token);
    def->name = func_name;
    
    // 存储类型表达式和函数体
    // 使用union字段：param_type存储类型表达式，lambda_body存储函数体
    def->param_type = type_expr;
    def->lambda_body = body;
    
    return def;
}

// 解析模块
ASTNode* parser_parse_module(Parser* parser) {
    Token* module_token = parser_peek(parser);
    parser_advance(parser); // 跳过 module
    
    if (!parser_expect(parser, TOKEN_IDENTIFIER, "Expected module name after 'module'")) {
        return NULL;
    }
    
    Token* name_token = parser->current_token;
    char* module_name = strdup(name_token->value);
    
    if (!parser_expect(parser, TOKEN_WHERE, "Expected 'where' after module name")) {
        free(module_name);
        return NULL;
    }
    
    // 解析模块体（声明列表）
    size_t capacity = 16;
    size_t count = 0;
    ASTNode** declarations = (ASTNode**)malloc(capacity * sizeof(ASTNode*));
    if (!declarations) {
        free(module_name);
        return NULL;
    }
    
    while (!parser_check(parser, TOKEN_EOF)) {
        Token* token = parser_peek(parser);
        if (!token) {
            break;
        }
        
        // 调试：打印遇到的 token
        fprintf(stderr, "[PARSER DEBUG] Token kind: %d, value: %s\n", 
                token->kind, token->value ? token->value : "(null)");
        
        ASTNode* decl = NULL;
        
        switch (token->kind) {
            case TOKEN_TYPE:
                fprintf(stderr, "[PARSER DEBUG] Parsing type declaration...\n");
                decl = parser_parse_type_decl(parser);
                if (decl && decl->name) {
                    fprintf(stderr, "[PARSER DEBUG] Successfully parsed type: %s\n", decl->name);
                } else {
                    fprintf(stderr, "[PARSER DEBUG] Failed to parse type declaration\n");
                }
                break;
                
            case TOKEN_DEF:
                decl = parser_parse_def(parser);
                break;
                
            case TOKEN_IMPORT:
                // TODO: 实现 import 解析
                // 跳过 import 关键字
                parser_advance(parser);
                // 跳过 import 后面的标识符（模块名）
                if (parser_check(parser, TOKEN_IDENTIFIER)) {
                    parser_advance(parser);
                }
                // 继续解析，不结束循环
                break;
                
            default:
                // 遇到非声明 Token，结束模块解析
                goto done;
        }
        
        if (decl) {
            if (count >= capacity) {
                capacity *= 2;
                ASTNode** new_decls = (ASTNode**)realloc(declarations, capacity * sizeof(ASTNode*));
                if (!new_decls) {
                    ast_node_free(decl);
                    for (size_t i = 0; i < count; i++) {
                        ast_node_free(declarations[i]);
                    }
                    free(declarations);
                    free(module_name);
                    return NULL;
                }
                declarations = new_decls;
            }
            declarations[count++] = decl;
            fprintf(stderr, "[PARSER DEBUG] Added declaration %zu\n", count);
        } else {
            // 如果解析失败但没有错误，可能是遇到了非声明 token
            // 检查是否有错误
            if (parser->has_error) {
                fprintf(stderr, "[PARSER DEBUG] Parser has error, stopping: %s\n", 
                        parser->error_message ? parser->error_message : "(no message)");
                break;
            } else {
                fprintf(stderr, "[PARSER DEBUG] No declaration parsed, but no error. Token kind: %d\n", 
                        token ? token->kind : -1);
            }
        }
        
        if (parser->has_error) {
            fprintf(stderr, "[PARSER DEBUG] Parser error detected, breaking loop\n");
            break;
        }
        
        // 调试：检查是否到达文件末尾
        Token* next_token = parser_peek(parser);
        if (!next_token) {
            fprintf(stderr, "[PARSER DEBUG] Next token is NULL, checking lexer state...\n");
            if (parser->lexer) {
                fprintf(stderr, "[PARSER DEBUG] Lexer position: %zu/%zu, line: %d\n",
                        parser->lexer->position, parser->lexer->source_length, parser->lexer->line);
                // 如果词法分析器还没有到达文件末尾，尝试强制读取下一个 token
                if (parser->lexer->position < parser->lexer->source_length) {
                    fprintf(stderr, "[PARSER DEBUG] Lexer not at EOF, forcing next token...\n");
                    parser_advance(parser);
                    next_token = parser_peek(parser);
                    if (next_token) {
                        fprintf(stderr, "[PARSER DEBUG] Forced token: kind=%d, value=%s\n",
                                next_token->kind, next_token->value ? next_token->value : "(null)");
                    }
                }
            }
        }
        if (!next_token || next_token->kind == TOKEN_EOF) {
            fprintf(stderr, "[PARSER DEBUG] Reached EOF, stopping\n");
            break;
        }
    }
    
done:
    fprintf(stderr, "[PARSER DEBUG] Module parsing done. Total declarations: %zu\n", count);
    ASTNode* module = ast_node_create(AST_MODULE, module_token);
    module->module_name = module_name;
    module->declarations = declarations;
    module->decl_count = count;
    
    return module;
}

// 解析整个程序
ASTNode* parser_parse_program(Parser* parser) {
    if (!parser) {
        return NULL;
    }
    
    Token* token = parser_peek(parser);
    if (!token) {
        parser_error_internal(parser, "Empty program");
        return NULL;
    }
    
    if (token->kind == TOKEN_MODULE) {
        parser->root = parser_parse_module(parser);
    } else {
        // 如果没有 module 声明，解析为表达式序列
        // TODO: 实现表达式序列解析
        parser_error_internal(parser, "Expected 'module' declaration at start of program");
        return NULL;
    }
    
    return parser->root;
}

// ========== 公共接口 ==========

bool parser_has_error(Parser* parser) {
    return parser && parser->has_error;
}

const char* parser_get_error(Parser* parser) {
    if (!parser || !parser->has_error) {
        return NULL;
    }
    return parser->error_message;
}

// 报告错误（公共接口）
void parser_error(Parser* parser, const char* message) {
    parser_error_internal(parser, message);
}

