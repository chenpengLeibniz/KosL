// compiler/src/compiler/codegen.c
// KOS-TL 代码生成器实现

#include "compiler/codegen.h"
#include "../../include/kos_core.h"
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <ctype.h>

// ========== 代码生成器状态管理 ==========

CodeGenState* codegen_create(FILE* output, TypeChecker* type_checker) {
    if (!output) {
        return NULL;
    }
    
    CodeGenState* state = (CodeGenState*)calloc(1, sizeof(CodeGenState));
    if (!state) {
        return NULL;
    }
    
    state->output = output;
    state->indent_level = 0;
    state->current_function = NULL;
    state->in_function = false;
    state->temp_var_counter = 0;
    state->type_checker = type_checker;
    
    return state;
}

void codegen_free(CodeGenState* state) {
    if (!state) {
        return;
    }
    
    if (state->current_function) {
        free(state->current_function);
    }
    
    free(state);
}

// ========== 辅助函数 ==========

void codegen_indent(CodeGenState* state) {
    if (!state || !state->output) {
        return;
    }
    
    for (int i = 0; i < state->indent_level; i++) {
        fprintf(state->output, "    ");
    }
}

void codegen_printf(CodeGenState* state, const char* format, ...) {
    if (!state || !state->output) {
        return;
    }
    
    va_list args;
    va_start(args, format);
    vfprintf(state->output, format, args);
    va_end(args);
}

void codegen_print_string(CodeGenState* state, const char* str) {
    if (!state || !str) {
        return;
    }
    
    codegen_printf(state, "\"");
    for (const char* p = str; *p; p++) {
        switch (*p) {
            case '\n': codegen_printf(state, "\\n"); break;
            case '\t': codegen_printf(state, "\\t"); break;
            case '\r': codegen_printf(state, "\\r"); break;
            case '\\': codegen_printf(state, "\\\\"); break;
            case '"': codegen_printf(state, "\\\""); break;
            default: codegen_printf(state, "%c", *p); break;
        }
    }
    codegen_printf(state, "\"");
}

char* codegen_temp_var(CodeGenState* state) {
    if (!state) {
        return NULL;
    }
    
    char* name = (char*)malloc(32);
    if (!name) {
        return NULL;
    }
    
    snprintf(name, 32, "_tmp_%d", state->temp_var_counter++);
    return name;
}

char* codegen_c_identifier(const char* name) {
    if (!name) {
        return NULL;
    }
    
    // 确保C标识符合法
    size_t len = strlen(name);
    char* result = (char*)malloc(len + 1);
    if (!result) {
        return NULL;
    }
    
    // 第一个字符必须是字母或下划线
    if (isalpha(name[0]) || name[0] == '_') {
        result[0] = name[0];
    } else {
        result[0] = '_';
    }
    
    // 后续字符可以是字母、数字或下划线
    for (size_t i = 1; i < len; i++) {
        if (isalnum(name[i]) || name[i] == '_') {
            result[i] = name[i];
        } else {
            result[i] = '_';
        }
    }
    
    result[len] = '\0';
    return result;
}

const char* codegen_type_to_c_type(kos_term* type) {
    if (!type) {
        return "kos_term*";
    }
    
    // 所有KOS-TL类型都映射到kos_term*
    return "kos_term*";
}

// ========== 文件生成 ==========

void codegen_file_header(CodeGenState* state, const char* module_name) {
    if (!state || !state->output) {
        return;
    }
    
    codegen_printf(state, "// 自动生成的C代码\n");
    codegen_printf(state, "// 模块: %s\n", module_name ? module_name : "Unknown");
    codegen_printf(state, "// 此文件由KOS-TL编译器自动生成，请勿手动编辑\n");
    codegen_printf(state, "\n");
    codegen_printf(state, "#include \"kos_core.h\"\n");
    codegen_printf(state, "#include \"kos_runtime.h\"\n");
    codegen_printf(state, "#include <stdlib.h>\n");
    codegen_printf(state, "#include <string.h>\n");
    codegen_printf(state, "\n");
}

void codegen_file_footer(CodeGenState* state) {
    (void)state;
    // 文件尾可以添加清理代码等
}

// ========== 函数生成 ==========

void codegen_function_decl(CodeGenState* state, const char* name, kos_term* type) {
    if (!state || !name) {
        return;
    }
    
    char* c_name = codegen_c_identifier(name);
    if (!c_name) {
        return;
    }
    
    const char* c_type = codegen_type_to_c_type(type);
    
    codegen_printf(state, "%s %s(", c_type, c_name);
    
    // TODO: 生成参数列表（根据类型）
    codegen_printf(state, "void");
    
    codegen_printf(state, ");\n");
    
    // c_name 总是新分配的内存，需要释放
    free(c_name);
}

void codegen_function_start(CodeGenState* state, const char* name, kos_term* type) {
    if (!state || !name) {
        return;
    }
    
    char* c_name = codegen_c_identifier(name);
    if (!c_name) {
        return;
    }
    
    const char* c_type = codegen_type_to_c_type(type);
    
    codegen_printf(state, "\n");
    codegen_printf(state, "%s %s(", c_type, c_name);
    
    // TODO: 生成参数列表
    codegen_printf(state, "void");
    
    codegen_printf(state, ") {\n");
    
    state->indent_level++;
    state->in_function = true;
    
    if (state->current_function) {
        free(state->current_function);
    }
    state->current_function = strdup(c_name);
    
    // c_name 总是新分配的内存，需要释放
    free(c_name);
}

void codegen_function_end(CodeGenState* state) {
    if (!state) {
        return;
    }
    
    state->indent_level--;
    codegen_indent(state);
    codegen_printf(state, "}\n");
    
    state->in_function = false;
    if (state->current_function) {
        free(state->current_function);
        state->current_function = NULL;
    }
}

// ========== 表达式代码生成 ==========

bool codegen_expr(CodeGenState* state, ASTNode* expr, kos_term* expected_type) {
    if (!state || !expr) {
        return false;
    }
    
    switch (expr->kind) {
        case AST_VARIABLE: {
            if (!expr->name) {
                return false;
            }
            char* c_name = codegen_c_identifier(expr->name);
            if (!c_name) {
                return false;
            }
            codegen_printf(state, "%s", c_name);
            // c_name 总是新分配的内存，需要释放
            free(c_name);
            return true;
        }
        
        case AST_LITERAL: {
            if (expr->value) {
                // 检查是否为数字字面量
                bool is_number = true;
                for (const char* p = expr->value; *p; p++) {
                    if (!((*p >= '0' && *p <= '9') || *p == '.' || *p == '-' || *p == '+')) {
                        is_number = false;
                        break;
                    }
                }
                
                if (is_number && strlen(expr->value) > 0) {
                    // 数字字面量
                    codegen_printf(state, "%s", expr->value);
                } else {
                    // 字符串字面量
                    codegen_print_string(state, expr->value);
                }
                return true;
            }
            return false;
        }
        
        case AST_LAMBDA: {
            // Lambda表达式：生成匿名函数
            if (expr->param_name && expr->param_type && expr->lambda_body) {
                codegen_printf(state, "(/* lambda %s */ ", expr->param_name);
                // 生成函数体
                if (!codegen_expr(state, expr->lambda_body, expected_type)) {
                    return false;
                }
                codegen_printf(state, ")");
            } else {
                codegen_printf(state, "/* lambda */ NULL");
            }
            return true;
        }
        
        case AST_APPLICATION: {
            // 函数应用：f x 或 f(x)
            // 检查是否是字段访问（如 tx.amt）
            if (expr->function && expr->function->kind == AST_VARIABLE && 
                expr->argument && expr->argument->kind == AST_VARIABLE) {
                // 可能是字段访问，但需要检查是否有 DOT 操作符
                // 这里先按函数应用处理
            }
            
            if (!codegen_expr(state, expr->function, NULL)) {
                return false;
            }
            codegen_printf(state, "(");
            if (!codegen_expr(state, expr->argument, NULL)) {
                return false;
            }
            codegen_printf(state, ")");
            return true;
        }
        
        case AST_TYPE_ANNOTATION: {
            // 类型注解：生成表达式，忽略类型注解
            return codegen_expr(state, expr->function, expected_type);
        }
        
        case AST_LET: {
            // let x = e1 in e2
            if (!expr->var_name) {
                return false;
            }
            codegen_indent(state);
            const char* c_type = codegen_type_to_c_type(expected_type);
            char* var_name = codegen_c_identifier(expr->var_name);
            if (!var_name) {
                return false;
            }
            codegen_printf(state, "%s %s = ", c_type, var_name);
            
            if (!codegen_expr(state, expr->var_value, NULL)) {
                free(var_name);
                return false;
            }
            codegen_printf(state, ";\n");
            
            if (!codegen_expr(state, expr->let_body, expected_type)) {
                free(var_name);
                return false;
            }
            
            // var_name 总是新分配的内存，需要释放
            free(var_name);
            return true;
        }
        
        case AST_IF: {
            // if e1 then e2 else e3
            codegen_printf(state, "(");
            if (!codegen_expr(state, expr->condition, NULL)) {
                return false;
            }
            codegen_printf(state, " ? ");
            if (!codegen_expr(state, expr->then_expr, expected_type)) {
                return false;
            }
            codegen_printf(state, " : ");
            if (!codegen_expr(state, expr->else_expr, expected_type)) {
                return false;
            }
            codegen_printf(state, ")");
            return true;
        }
        
        case AST_MATCH: {
            // match expr with cases
            codegen_printf(state, "/* match expression */ ");
            if (expr->matched_expr) {
                if (!codegen_expr(state, expr->matched_expr, NULL)) {
                    return false;
                }
            }
            return true;
        }
        
        case AST_PAIR: {
            // (a, b)
            codegen_printf(state, "/* pair */ ");
            return true;
        }
        
        default:
            codegen_printf(state, "/* unsupported expression type: %d */", expr->kind);
            return false;
    }
}

// ========== 类型声明生成 ==========

bool codegen_type_decl(CodeGenState* state, ASTNode* type_decl) {
    if (!state || !type_decl || type_decl->kind != AST_TYPE_DECL) {
        return false;
    }
    
    // 检查类型名称 - 直接使用 name 字段
    const char* type_name = type_decl->name;
    if (!type_name) {
        type_name = "UnknownType";
    }
    
    // 调试：检查名称是否有效
    if (strlen(type_name) == 0 || strcmp(type_name, ":") == 0) {
        // 如果名称无效，使用默认名称
        type_name = "UnknownType";
    }
    
    char* c_name = codegen_c_identifier(type_name);
    if (!c_name) {
        return false;
    }
    
    codegen_printf(state, "// Type declaration: %s\n", type_name);
    
    // 如果有类型表达式（如 : U₁ 或 : Type₁），生成类型定义
    if (type_decl->param_type) {
        codegen_printf(state, "kos_term* %s_type = ", c_name);
        
        kos_term* type_term = NULL;
        if (state->type_checker) {
            type_term = type_checker_ast_to_type(state->type_checker, type_decl->param_type);
        }
        
        if (type_term) {
            int level = 1;
            if (type_term->kind == KOS_U || type_term->kind == KOS_TYPE) {
                if (type_term->data.universe.level > 0) {
                    level = type_term->data.universe.level;
                }
            }
            
            if (type_term->kind == KOS_U) {
                codegen_printf(state, "kos_mk_universe_computational(%d);\n", level);
            } else if (type_term->kind == KOS_TYPE) {
                codegen_printf(state, "kos_mk_universe_logical(%d);\n", level);
            } else if (type_term->kind == KOS_SIGMA) {
                // Σ 类型：生成依赖和类型
                // Σ(x:A).B 转换为 kos_mk_sigma(domain, body)
                if (type_term->data.sigma.domain && type_term->data.sigma.body) {
                    codegen_printf(state, "kos_mk_sigma(");
                    // TODO: 生成 domain 和 body 的代码
                    codegen_printf(state, "NULL, NULL");
                    codegen_printf(state, ");\n");
                } else {
                    codegen_printf(state, "/* TODO: Generate Σ type */\n");
                    codegen_printf(state, "kos_mk_universe_computational(1);\n");
                }
            } else if (type_term->kind == KOS_PI) {
                // Π 类型：生成依赖积类型
                // Π(x:A).B 转换为 kos_mk_pi(domain, body)
                if (type_term->data.pi.domain && type_term->data.pi.body) {
                    codegen_printf(state, "kos_mk_pi(");
                    // TODO: 生成 domain 和 body 的代码
                    codegen_printf(state, "NULL, NULL");
                    codegen_printf(state, ");\n");
                } else {
                    codegen_printf(state, "/* TODO: Generate Π type */\n");
                    codegen_printf(state, "kos_mk_universe_computational(1);\n");
                }
            } else {
                codegen_printf(state, "kos_mk_universe_computational(1);\n");
            }
            kos_term_free(type_term);
        } else {
            codegen_printf(state, "kos_mk_universe_computational(1);\n");
        }
    } else {
        // 没有类型表达式，生成默认类型
        codegen_printf(state, "kos_term* %s_type = kos_mk_universe_computational(1);\n", c_name);
    }
    
    free(c_name);
    return true;
}

// ========== 函数定义生成 ==========

bool codegen_def(CodeGenState* state, ASTNode* def) {
    if (!state || !def || def->kind != AST_DEF) {
        return false;
    }
    
    if (!def->name) {
        return false;
    }
    
    // 推断函数类型（如果有类型检查器）
    kos_term* func_type = NULL;
    if (state->type_checker) {
        func_type = type_checker_infer_expr(state->type_checker, def);
        // 如果类型推断失败，继续使用 NULL（表示未知类型）
    }
    
    // 生成函数定义
    codegen_function_start(state, def->name, func_type);
    
    // 生成函数体
    if (def->lambda_body) {
        codegen_indent(state);
        codegen_printf(state, "return ");
        if (!codegen_expr(state, def->lambda_body, func_type)) {
            codegen_function_end(state);
            if (func_type) {
                kos_term_free(func_type);
            }
            return false;
        }
        codegen_printf(state, ";\n");
    } else {
        codegen_indent(state);
        codegen_printf(state, "// Function body not found\n");
        codegen_indent(state);
        codegen_printf(state, "return NULL;\n");
    }
    
    codegen_function_end(state);
    
    if (func_type) {
        kos_term_free(func_type);
    }
    
    return true;
}

// ========== 模块生成 ==========

bool codegen_module(CodeGenState* state, ASTNode* module) {
    if (!state || !module || module->kind != AST_MODULE) {
        return false;
    }
    
    const char* module_name = module->module_name ? module->module_name : "Unknown";
    
    // 生成文件头
    codegen_file_header(state, module_name);
    
    // 生成所有声明
    if (module->declarations && module->decl_count > 0) {
        codegen_printf(state, "// ========== 类型声明 ==========\n\n");
        for (size_t i = 0; i < module->decl_count; i++) {
            ASTNode* decl = module->declarations[i];
            if (!decl) {
                continue;
            }
            
            switch (decl->kind) {
                case AST_TYPE_DECL:
                    if (!codegen_type_decl(state, decl)) {
                        codegen_printf(state, "// Failed to generate type declaration\n");
                    }
                    codegen_printf(state, "\n");
                    break;
                    
                case AST_DEF:
                    if (!codegen_def(state, decl)) {
                        codegen_printf(state, "// Failed to generate function definition\n");
                    }
                    codegen_printf(state, "\n");
                    break;
                    
                default:
                    codegen_indent(state);
                    codegen_printf(state, "// Unsupported declaration type: %d\n", decl->kind);
                    break;
            }
        }
    } else {
        codegen_printf(state, "// No declarations found in module\n");
    }
    
    // 生成文件尾
    codegen_file_footer(state);
    
    return true;
}

// ========== 程序生成 ==========

bool codegen_program(CodeGenState* state, ASTNode* program) {
    if (!state || !program) {
        return false;
    }
    
    if (program->kind == AST_MODULE) {
        return codegen_module(state, program);
    }
    
    // 如果不是模块，生成表达式序列
    codegen_file_header(state, NULL);
    codegen_indent(state);
    codegen_printf(state, "int main(void) {\n");
    state->indent_level++;
    
    codegen_indent(state);
    if (!codegen_expr(state, program, NULL)) {
        state->indent_level--;
        return false;
    }
    codegen_printf(state, ";\n");
    
    state->indent_level--;
    codegen_indent(state);
    codegen_printf(state, "return 0;\n");
    codegen_indent(state);
    codegen_printf(state, "}\n");
    
    return true;
}

