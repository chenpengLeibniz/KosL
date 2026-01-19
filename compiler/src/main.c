// compiler/src/main.c
// KOS-TL 编译器主程序

#include "compiler/lexer.h"
#include "compiler/parser.h"
#include "compiler/type_checker.h"
#include "compiler/codegen.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// 打印使用说明
static void print_usage(const char* program_name) {
    printf("KOS-TL 编译器\n");
    printf("用法: %s [选项] <输入文件> [输出文件]\n", program_name);
    printf("\n");
    printf("选项:\n");
    printf("  -o <文件>    指定输出文件（默认：stdout）\n");
    printf("  -t           只进行类型检查，不生成代码\n");
    printf("  -h, --help   显示此帮助信息\n");
    printf("  -v, --version 显示版本信息\n");
    printf("\n");
    printf("示例:\n");
    printf("  %s example.kos -o example.c\n", program_name);
    printf("  %s example.kos -t\n", program_name);
}

// 打印版本信息
static void print_version(void) {
    printf("KOS-TL 编译器 v0.1.0\n");
    printf("基于KOS-TL类型逻辑系统\n");
}

// 编译文件
static int compile_file(const char* input_file, const char* output_file, bool type_check_only) {
    fprintf(stderr, "[DEBUG] 开始编译文件: %s\n", input_file);
    fflush(stderr);
    
    // 读取输入文件
    fprintf(stderr, "[DEBUG] 步骤1: 读取文件...\n");
    fflush(stderr);
    Lexer* lexer = lexer_from_file(input_file);
    if (!lexer) {
        fprintf(stderr, "错误: 无法读取文件 '%s'\n", input_file);
        return 1;
    }
    fprintf(stderr, "[DEBUG] 文件读取成功\n");
    fflush(stderr);
    
    // 词法分析
    fprintf(stderr, "[DEBUG] 步骤2: 检查词法分析错误...\n");
    fflush(stderr);
    if (lexer_has_error(lexer)) {
        fprintf(stderr, "词法分析错误: %s\n", lexer_get_error(lexer));
        lexer_free(lexer);
        return 1;
    }
    fprintf(stderr, "[DEBUG] 词法分析检查通过\n");
    fflush(stderr);
    
    // 语法分析
    fprintf(stderr, "[DEBUG] 步骤3: 创建语法分析器...\n");
    fflush(stderr);
    Parser* parser = parser_create(lexer);
    if (!parser) {
        fprintf(stderr, "错误: 无法创建语法分析器\n");
        lexer_free(lexer);
        return 1;
    }
    fprintf(stderr, "[DEBUG] 语法分析器创建成功\n");
    fflush(stderr);
    
    fprintf(stderr, "[DEBUG] 步骤4: 解析程序...\n");
    fflush(stderr);
    ASTNode* program = parser_parse_program(parser);
    if (!program) {
        fprintf(stderr, "语法分析错误: %s\n", parser_get_error(parser));
        parser_free(parser);
        lexer_free(lexer);
        return 1;
    }
    fprintf(stderr, "[DEBUG] 程序解析成功\n");
    fflush(stderr);
    
    // 类型检查
    fprintf(stderr, "[DEBUG] 步骤5: 创建类型检查器...\n");
    fflush(stderr);
    TypeChecker* type_checker = type_checker_create();
    if (!type_checker) {
        fprintf(stderr, "错误: 无法创建类型检查器\n");
        ast_node_free(program);
        parser_free(parser);
        lexer_free(lexer);
        return 1;
    }
    fprintf(stderr, "[DEBUG] 类型检查器创建成功\n");
    fflush(stderr);
    
    fprintf(stderr, "[DEBUG] 步骤6: 进行类型检查...\n");
    fflush(stderr);
    bool type_check_ok = type_checker_check_program(type_checker, program);
    if (!type_check_ok) {
        fprintf(stderr, "警告: 类型检查错误: %s\n", type_checker_get_error(type_checker));
        fprintf(stderr, "继续生成代码（可能不完整）...\n");
        fflush(stderr);
        // 不返回，继续生成代码
    } else {
        printf("✓ 类型检查通过\n");
        fflush(stdout);
    }
    
    // 如果只需要类型检查，直接返回
    if (type_check_only) {
        fprintf(stderr, "[DEBUG] 只进行类型检查，开始清理...\n");
        fflush(stderr);
        fprintf(stderr, "[DEBUG] 准备释放类型检查器...\n");
        fflush(stderr);
        type_checker_free(type_checker);
        fprintf(stderr, "[DEBUG] 类型检查器已释放\n");
        fflush(stderr);
        fprintf(stderr, "[DEBUG] 准备释放AST...\n");
        fflush(stderr);
        ast_node_free(program);
        fprintf(stderr, "[DEBUG] AST已释放\n");
        fflush(stderr);
        fprintf(stderr, "[DEBUG] 准备释放解析器...\n");
        fflush(stderr);
        parser_free(parser);
        fprintf(stderr, "[DEBUG] 解析器已释放\n");
        fflush(stderr);
        fprintf(stderr, "[DEBUG] 准备释放词法分析器...\n");
        fflush(stderr);
        lexer_free(lexer);
        fprintf(stderr, "[DEBUG] 词法分析器已释放\n");
        fflush(stderr);
        fprintf(stderr, "[DEBUG] 清理完成\n");
        fflush(stderr);
        return 0;
    }
    
    // 代码生成
    fprintf(stderr, "[DEBUG] 步骤7: 准备代码生成...\n");
    fflush(stderr);
    FILE* output = stdout;
    if (output_file) {
        output = fopen(output_file, "w");
        if (!output) {
            fprintf(stderr, "错误: 无法打开输出文件 '%s'\n", output_file);
            type_checker_free(type_checker);
            ast_node_free(program);
            parser_free(parser);
            lexer_free(lexer);
            return 1;
        }
    }
    fprintf(stderr, "[DEBUG] 输出文件已打开\n");
    fflush(stderr);
    
    fprintf(stderr, "[DEBUG] 步骤8: 创建代码生成器...\n");
    fflush(stderr);
    CodeGenState* codegen = codegen_create(output, type_checker);
    if (!codegen) {
        fprintf(stderr, "错误: 无法创建代码生成器\n");
        if (output_file) {
            fclose(output);
        }
        type_checker_free(type_checker);
        ast_node_free(program);
        parser_free(parser);
        lexer_free(lexer);
        return 1;
    }
    fprintf(stderr, "[DEBUG] 代码生成器创建成功\n");
    fflush(stderr);
    
    fprintf(stderr, "[DEBUG] 步骤9: 生成代码...\n");
    fflush(stderr);
    bool codegen_ok = codegen_program(codegen, program);
    if (!codegen_ok) {
        fprintf(stderr, "错误: 代码生成失败\n");
        codegen_free(codegen);
        if (output_file) {
            fclose(output);
        }
        type_checker_free(type_checker);
        ast_node_free(program);
        parser_free(parser);
        lexer_free(lexer);
        return 1;
    }
    
    printf("✓ 代码生成完成\n");
    if (output_file) {
        printf("输出文件: %s\n", output_file);
    }
    fflush(stdout);
    
    // 清理
    fprintf(stderr, "[DEBUG] 步骤10: 开始清理资源...\n");
    fflush(stderr);
    codegen_free(codegen);
    fprintf(stderr, "[DEBUG] 代码生成器已释放\n");
    fflush(stderr);
    if (output_file) {
        fclose(output);
        fprintf(stderr, "[DEBUG] 输出文件已关闭\n");
        fflush(stderr);
    }
    type_checker_free(type_checker);
    fprintf(stderr, "[DEBUG] 类型检查器已释放\n");
    fflush(stderr);
    ast_node_free(program);
    fprintf(stderr, "[DEBUG] AST已释放\n");
    fflush(stderr);
    parser_free(parser);
    fprintf(stderr, "[DEBUG] 解析器已释放\n");
    fflush(stderr);
    lexer_free(lexer);
    fprintf(stderr, "[DEBUG] 词法分析器已释放\n");
    fflush(stderr);
    fprintf(stderr, "[DEBUG] 所有资源清理完成\n");
    fflush(stderr);
    
    return 0;
}

int main(int argc, char* argv[]) {
    const char* input_file = NULL;
    const char* output_file = NULL;
    bool type_check_only = false;
    
    // 解析命令行参数
    for (int i = 1; i < argc; i++) {
        if (strcmp(argv[i], "-h") == 0 || strcmp(argv[i], "--help") == 0) {
            print_usage(argv[0]);
            return 0;
        } else if (strcmp(argv[i], "-v") == 0 || strcmp(argv[i], "--version") == 0) {
            print_version();
            return 0;
        } else if (strcmp(argv[i], "-o") == 0) {
            if (i + 1 < argc) {
                output_file = argv[++i];
            } else {
                fprintf(stderr, "错误: -o 选项需要指定输出文件\n");
                print_usage(argv[0]);
                return 1;
            }
        } else if (strcmp(argv[i], "-t") == 0) {
            type_check_only = true;
        } else if (argv[i][0] != '-') {
            if (!input_file) {
                input_file = argv[i];
            } else {
                fprintf(stderr, "错误: 只能指定一个输入文件\n");
                print_usage(argv[0]);
                return 1;
            }
        } else {
            fprintf(stderr, "错误: 未知选项 '%s'\n", argv[i]);
            print_usage(argv[0]);
            return 1;
        }
    }
    
    // 检查输入文件
    if (!input_file) {
        fprintf(stderr, "错误: 未指定输入文件\n");
        print_usage(argv[0]);
        return 1;
    }
    
    // 编译
    return compile_file(input_file, output_file, type_check_only);
}





