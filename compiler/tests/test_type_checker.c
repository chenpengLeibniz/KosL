// compiler/tests/test_type_checker.c
// 类型检查器测试程序

#include "compiler/type_checker.h"
#include "compiler/parser.h"
#include "compiler/lexer.h"
#include <stdio.h>
#include <stdlib.h>

// 测试简单表达式类型检查
static void test_simple_expr() {
    printf("\n=== 测试简单表达式类型检查 ===\n");
    
    const char* source = "let x = 42 in x";
    Lexer* lexer = lexer_create(source);
    Parser* parser = parser_create(lexer);
    
    ASTNode* expr = parser_parse_expr(parser);
    if (!expr) {
        printf("✗ 解析失败: %s\n", parser_get_error(parser));
        parser_free(parser);
        lexer_free(lexer);
        return;
    }
    
    TypeChecker* checker = type_checker_create();
    if (!checker) {
        printf("✗ 创建类型检查器失败\n");
        ast_node_free(expr);
        parser_free(parser);
        lexer_free(lexer);
        return;
    }
    
    // 推断类型
    kos_term* inferred_type = type_checker_infer_expr(checker, expr);
    if (inferred_type) {
        printf("✓ 类型推断成功\n");
        kos_term_free(inferred_type);
    } else {
        printf("✗ 类型推断失败: %s\n", type_checker_get_error(checker));
    }
    
    type_checker_free(checker);
    ast_node_free(expr);
    parser_free(parser);
    lexer_free(lexer);
}

// 测试Lambda表达式类型检查
static void test_lambda_expr() {
    printf("\n=== 测试Lambda表达式类型检查 ===\n");
    
    const char* source = "lambda (x : Int) -> x";
    Lexer* lexer = lexer_create(source);
    Parser* parser = parser_create(lexer);
    
    ASTNode* expr = parser_parse_expr(parser);
    if (!expr) {
        printf("✗ 解析失败: %s\n", parser_get_error(parser));
        parser_free(parser);
        lexer_free(lexer);
        return;
    }
    
    TypeChecker* checker = type_checker_create();
    if (!checker) {
        printf("✗ 创建类型检查器失败\n");
        ast_node_free(expr);
        parser_free(parser);
        lexer_free(lexer);
        return;
    }
    
    // 推断类型
    kos_term* inferred_type = type_checker_infer_expr(checker, expr);
    if (inferred_type) {
        printf("✓ Lambda类型推断成功\n");
        kos_term_free(inferred_type);
    } else {
        printf("✗ Lambda类型推断失败: %s\n", type_checker_get_error(checker));
    }
    
    type_checker_free(checker);
    ast_node_free(expr);
    parser_free(parser);
    lexer_free(lexer);
}

// 测试类型表达式检查
static void test_type_expr() {
    printf("\n=== 测试类型表达式检查 ===\n");
    
    const char* source = "U1";
    Lexer* lexer = lexer_create(source);
    Parser* parser = parser_create(lexer);
    
    ASTNode* type_ast = parser_parse_type(parser);
    if (!type_ast) {
        printf("✗ 类型解析失败: %s\n", parser_get_error(parser));
        parser_free(parser);
        lexer_free(lexer);
        return;
    }
    
    TypeChecker* checker = type_checker_create();
    if (!checker) {
        printf("✗ 创建类型检查器失败\n");
        ast_node_free(type_ast);
        parser_free(parser);
        lexer_free(lexer);
        return;
    }
    
    // 检查类型表达式
    kos_term* type = type_checker_ast_to_type(checker, type_ast);
    if (type) {
        printf("✓ 类型表达式检查成功\n");
        kos_term_free(type);
    } else {
        printf("✗ 类型表达式检查失败: %s\n", type_checker_get_error(checker));
    }
    
    type_checker_free(checker);
    ast_node_free(type_ast);
    parser_free(parser);
    lexer_free(lexer);
}

int main(int argc, char* argv[]) {
    (void)argc;
    (void)argv;
    
    printf("KOS-TL 类型检查器测试\n");
    printf("====================\n");
    
    test_simple_expr();
    test_lambda_expr();
    test_type_expr();
    
    printf("\n====================\n");
    printf("测试完成！\n");
    
    return 0;
}





