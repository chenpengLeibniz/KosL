// compiler/tests/test_lexer.c
// 词法分析器测试程序

#include "compiler/lexer.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

// 测试辅助函数
static void test_token(Token* token, token_kind expected_kind, const char* expected_value) {
    (void)expected_kind; // 在 release 模式下可能未使用
    assert(token != NULL);
    assert(token->kind == expected_kind);
    if (expected_value) {
        assert(token->value != NULL);
        assert(strcmp(token->value, expected_value) == 0);
    }
    printf("✓ Token: %s", token_kind_name(token->kind));
    if (token->value) {
        printf(" = '%s'", token->value);
    }
    printf(" (line %d, col %d)\n", token->line, token->column);
}

// 测试基本 Token
static void test_basic_tokens() {
    printf("\n=== 测试基本 Token ===\n");
    
    const char* source = "module test where";
    Lexer* lexer = lexer_create(source);
    assert(lexer != NULL);
    
    Token* token;
    
    token = lexer_next(lexer);
    test_token(token, TOKEN_MODULE, "module");
    
    token = lexer_next(lexer);
    test_token(token, TOKEN_IDENTIFIER, "test");
    
    token = lexer_next(lexer);
    test_token(token, TOKEN_WHERE, "where");
    
    token = lexer_next(lexer);
    test_token(token, TOKEN_EOF, NULL);
    
    lexer_free(lexer);
    printf("✓ 基本 Token 测试通过\n");
}

// 测试数字
static void test_numbers() {
    printf("\n=== 测试数字 ===\n");
    
    const char* source = "123 456.789 0";
    Lexer* lexer = lexer_create(source);
    
    Token* token;
    
    token = lexer_next(lexer);
    test_token(token, TOKEN_NUMBER, "123");
    
    token = lexer_next(lexer);
    test_token(token, TOKEN_NUMBER, "456.789");
    
    token = lexer_next(lexer);
    test_token(token, TOKEN_NUMBER, "0");
    
    lexer_free(lexer);
    printf("✓ 数字测试通过\n");
}

// 测试字符串
static void test_strings() {
    printf("\n=== 测试字符串 ===\n");
    
    const char* source = "\"hello\" \"world\"";
    Lexer* lexer = lexer_create(source);
    
    Token* token;
    
    token = lexer_next(lexer);
    test_token(token, TOKEN_STRING, "hello");
    
    token = lexer_next(lexer);
    test_token(token, TOKEN_STRING, "world");
    
    lexer_free(lexer);
    printf("✓ 字符串测试通过\n");
}

// 测试操作符
static void test_operators() {
    printf("\n=== 测试操作符 ===\n");
    
    const char* source = "→ Σ Π λ × + - * / == != < > <= >=";
    Lexer* lexer = lexer_create(source);
    
    Token* token;
    
    token = lexer_next(lexer);
    test_token(token, TOKEN_ARROW, "->");
    
    token = lexer_next(lexer);
    test_token(token, TOKEN_SIGMA, "Σ");
    
    token = lexer_next(lexer);
    test_token(token, TOKEN_PI, "Π");
    
    token = lexer_next(lexer);
    test_token(token, TOKEN_LAMBDA, "λ");
    
    token = lexer_next(lexer);
    test_token(token, TOKEN_TIMES, "×");
    
    token = lexer_next(lexer);
    test_token(token, TOKEN_PLUS, "+");
    
    token = lexer_next(lexer);
    test_token(token, TOKEN_MINUS, "-");
    
    token = lexer_next(lexer);
    test_token(token, TOKEN_MUL, "*");
    
    token = lexer_next(lexer);
    test_token(token, TOKEN_DIV, "/");
    
    token = lexer_next(lexer);
    test_token(token, TOKEN_EQ, "==");
    
    token = lexer_next(lexer);
    test_token(token, TOKEN_NE, "!=");
    
    token = lexer_next(lexer);
    test_token(token, TOKEN_LT, "<");
    
    token = lexer_next(lexer);
    test_token(token, TOKEN_GT, ">");
    
    token = lexer_next(lexer);
    test_token(token, TOKEN_LE, "<=");
    
    token = lexer_next(lexer);
    test_token(token, TOKEN_GE, ">=");
    
    lexer_free(lexer);
    printf("✓ 操作符测试通过\n");
}

// 测试注释
static void test_comments() {
    printf("\n=== 测试注释 ===\n");
    
    const char* source = "-- 单行注释\n"
                         "type Test -- 行尾注释\n"
                         "{- 多行注释 -}\n"
                         "def x = 1";
    Lexer* lexer = lexer_create(source);
    
    Token* token;
    
    token = lexer_next(lexer);
    test_token(token, TOKEN_TYPE, "type");
    
    token = lexer_next(lexer);
    test_token(token, TOKEN_IDENTIFIER, "Test");
    
    token = lexer_next(lexer);
    test_token(token, TOKEN_DEF, "def");
    
    token = lexer_next(lexer);
    test_token(token, TOKEN_IDENTIFIER, "x");
    
    token = lexer_next(lexer);
    test_token(token, TOKEN_EQ, "=");
    
    token = lexer_next(lexer);
    test_token(token, TOKEN_NUMBER, "1");
    
    lexer_free(lexer);
    printf("✓ 注释测试通过\n");
}

// 测试完整程序
static void test_full_program() {
    printf("\n=== 测试完整程序 ===\n");
    
    const char* source = 
        "module Finance where\n"
        "\n"
        "type AccountID : U₁\n"
        "\n"
        "def hello : AccountID → String\n"
        "  = λ(x : AccountID) → \"Hello\"";
    
    Lexer* lexer = lexer_create(source);
    size_t count = 0;
    Token** tokens = lexer_tokenize_all(lexer, &count);
    
    printf("总共识别了 %zu 个 Token:\n", count);
    for (size_t i = 0; i < count; i++) {
        Token* token = tokens[i];
        printf("  [%zu] %s", i, token_kind_name(token->kind));
        if (token->value) {
            printf(" = '%s'", token->value);
        }
        printf(" (line %d, col %d)\n", token->line, token->column);
    }
    
    token_array_free(tokens, count);
    lexer_free(lexer);
    printf("✓ 完整程序测试通过\n");
}

int main(int argc, char* argv[]) {
    (void)argc;
    (void)argv;
    printf("KOS-TL 词法分析器测试\n");
    printf("====================\n");
    
    test_basic_tokens();
    test_numbers();
    test_strings();
    test_operators();
    test_comments();
    test_full_program();
    
    printf("\n====================\n");
    printf("所有测试通过！\n");
    
    return 0;
}

