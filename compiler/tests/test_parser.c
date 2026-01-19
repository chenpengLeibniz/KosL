// compiler/tests/test_parser.c
// 语法分析器测试程序

#include "compiler/parser.h"
#include "compiler/lexer.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int main(int argc, char* argv[]) {
    (void)argc;
    (void)argv;
    
    printf("KOS-TL 语法分析器测试\n");
    printf("====================\n\n");
    
    // 测试简单表达式
    const char* source1 = "let x = 42 in x + 1";
    printf("测试 1: 简单 Let 表达式\n");
    printf("源代码: %s\n", source1);
    
    Lexer* lexer1 = lexer_create(source1);
    Parser* parser1 = parser_create(lexer1);
    
    ASTNode* ast1 = parser_parse_expr(parser1);
    if (ast1) {
        printf("✓ 解析成功\n");
        printf("AST:\n");
        ast_print(ast1, 0);
    } else {
        printf("✗ 解析失败: %s\n", parser_get_error(parser1));
    }
    
    parser_free(parser1);
    lexer_free(lexer1);
    printf("\n");
    
    // 测试 Lambda 表达式
    const char* source2 = "lambda (x : Int) -> x + 1";
    printf("测试 2: Lambda 表达式\n");
    printf("源代码: %s\n", source2);
    
    Lexer* lexer2 = lexer_create(source2);
    Parser* parser2 = parser_create(lexer2);
    
    ASTNode* ast2 = parser_parse_expr(parser2);
    if (ast2) {
        printf("✓ 解析成功\n");
        printf("AST:\n");
        ast_print(ast2, 0);
    } else {
        printf("✗ 解析失败: %s\n", parser_get_error(parser2));
    }
    
    parser_free(parser2);
    lexer_free(lexer2);
    printf("\n");
    
    // 测试简单模块
    const char* source3 = 
        "module Test where\n"
        "type MyType : U1\n"
        "def hello : MyType -> String\n"
        "  = lambda (x : MyType) -> \"Hello\"";
    
    printf("测试 3: 简单模块\n");
    printf("源代码:\n%s\n", source3);
    
    Lexer* lexer3 = lexer_create(source3);
    Parser* parser3 = parser_create(lexer3);
    
    ASTNode* ast3 = parser_parse_program(parser3);
    if (ast3) {
        printf("✓ 解析成功\n");
        printf("AST:\n");
        ast_print(ast3, 0);
    } else {
        printf("✗ 解析失败: %s\n", parser_get_error(parser3));
    }
    
    parser_free(parser3);
    lexer_free(lexer3);
    printf("\n");
    
    printf("====================\n");
    printf("测试完成！\n");
    
    return 0;
}














