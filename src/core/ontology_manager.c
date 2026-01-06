// src/core/ontology_manager.c
// 类型本体管理器：基于类型构造的类型系统实现
// 所有类型定义都通过类型构造器（Π、Σ、Sum等）构造

#include "../../include/kos_ontology.h"
#include "../../include/kos_core.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <stdbool.h>
#include <stdarg.h>

#define INITIAL_CAPACITY 16

// ========== JSON解析辅助函数（用于反序列化） ==========

// 辅助函数：跳过空白字符
static const char* skip_whitespace(const char* str) {
    while (*str == ' ' || *str == '\t' || *str == '\n' || *str == '\r') {
        str++;
    }
    return str;
}

// 辅助函数：解析JSON字符串（带转义）
static const char* parse_json_string(const char* str, char** out_str) {
    str = skip_whitespace(str);
    if (*str != '"') {
        return NULL;
    }
    str++; // 跳过开始引号
    
    size_t len = 0;
    const char* start = str;
    while (*str != '\0' && *str != '"') {
        if (*str == '\\') {
            str++; // 跳过转义字符
            if (*str == '\0') break;
        }
        str++;
        len++;
    }
    
    if (*str != '"') {
        return NULL;
    }
    
    // 分配内存并复制字符串（处理转义）
    char* result = (char*)malloc(len + 1);
    if (!result) {
        return NULL;
    }
    
    size_t pos = 0;
    const char* p = start;
    while (p < str) {
        if (*p == '\\' && p + 1 < str) {
            p++;
            switch (*p) {
                case '"': result[pos++] = '"'; break;
                case '\\': result[pos++] = '\\'; break;
                case 'n': result[pos++] = '\n'; break;
                case 'r': result[pos++] = '\r'; break;
                case 't': result[pos++] = '\t'; break;
                default: result[pos++] = *p; break;
            }
            p++;
        } else {
            result[pos++] = *p++;
        }
    }
    result[pos] = '\0';
    
    *out_str = result;
    return str + 1; // 跳过结束引号
}

// 辅助函数：解析JSON数字
static const char* parse_json_number(const char* str, int* out_num) {
    str = skip_whitespace(str);
    int num = 0;
    int sign = 1;
    
    if (*str == '-') {
        sign = -1;
        str++;
    }
    
    while (*str >= '0' && *str <= '9') {
        num = num * 10 + (*str - '0');
        str++;
    }
    
    *out_num = num * sign;
    return str;
}

// 辅助函数：解析JSON布尔值
static const char* parse_json_bool(const char* str, bool* out_bool) {
    str = skip_whitespace(str);
    if (strncmp(str, "true", 4) == 0) {
        *out_bool = true;
        return str + 4;
    } else if (strncmp(str, "false", 5) == 0) {
        *out_bool = false;
        return str + 5;
    }
    return NULL;
}

// 辅助函数：解析null
static const char* parse_json_null(const char* str) {
    str = skip_whitespace(str);
    if (strncmp(str, "null", 4) == 0) {
        return str + 4;
    }
    return NULL;
}

// 辅助函数：提取JSON对象（找到匹配的结束大括号）
static const char* extract_json_object(const char* json_str, char** out_obj) {
    json_str = skip_whitespace(json_str);
    if (*json_str != '{') {
        return NULL;
    }
    
    int depth = 0;
    bool in_string = false;
    const char* start = json_str;
    const char* p = json_str;
    
    while (*p != '\0') {
        if (*p == '"' && (p == start || *(p-1) != '\\')) {
            in_string = !in_string;
        } else if (!in_string) {
            if (*p == '{') {
                depth++;
            } else if (*p == '}') {
                depth--;
                if (depth == 0) {
                    // 找到匹配的结束大括号
                    size_t len = p - start + 1;
                    *out_obj = (char*)malloc(len + 1);
                    if (!*out_obj) {
                        return NULL;
                    }
                    memcpy(*out_obj, start, len);
                    (*out_obj)[len] = '\0';
                    return p + 1;
                }
            }
        }
        p++;
    }
    
    return NULL;
}

// 创建空的本体
TypeOntology* kos_ontology_create(const char* domain_name) {
    if (!domain_name) {
        return NULL;
    }
    
    TypeOntology* ontology = (TypeOntology*)calloc(1, sizeof(TypeOntology));
    if (!ontology) {
        return NULL;
    }
    
    strncpy(ontology->domain_name, domain_name, sizeof(ontology->domain_name) - 1);
    ontology->domain_name[sizeof(ontology->domain_name) - 1] = '\0';
    
    ontology->type_definitions = NULL;
    ontology->type_count = 0;
    ontology->capacity = 0;
    
    return ontology;
}

// 释放本体（递归释放所有资源）
void kos_ontology_free(TypeOntology* ontology) {
    if (!ontology) {
        return;
    }
    
    // 释放所有类型定义
    if (ontology->type_definitions) {
        for (size_t i = 0; i < ontology->type_count; i++) {
            TypeDefinition* def = &ontology->type_definitions[i];
            
            // 释放名称
            if (def->name) {
                free(def->name);
            }
            
            // 释放类型定义（kos_term*）
            if (def->type_def) {
                kos_term_free(def->type_def);
            }
            
            // 释放上下文
            if (def->ctx) {
                kos_term_free(def->ctx);
            }
        }
        free(ontology->type_definitions);
    }
    
    free(ontology);
}

// 内部函数：扩展容量
static int expand_capacity(TypeOntology* ontology) {
    if (!ontology) {
        return -1;
    }
    
    size_t new_capacity = ontology->capacity == 0 ? INITIAL_CAPACITY : ontology->capacity * 2;
    TypeDefinition* new_array = (TypeDefinition*)realloc(
        ontology->type_definitions, new_capacity * sizeof(TypeDefinition));
    
    if (!new_array) {
        return -1;
    }
    
    ontology->type_definitions = new_array;
    ontology->capacity = new_capacity;
    
    return 0;
}

// 添加类型定义
int kos_ontology_add_type_definition(TypeOntology* ontology,
                                     const char* name,
                                     kos_term* type_def,
                                     kos_term* ctx) {
    if (!ontology || !name || !type_def) {
        return -1;
    }
    
    // 检查是否已存在
    if (kos_ontology_find_type_definition(ontology, name)) {
        return -1;  // 已存在
    }
    
    // 扩展容量（如果需要）
    if (ontology->type_count >= ontology->capacity) {
        if (expand_capacity(ontology) != 0) {
            return -1;
        }
    }
    
    TypeDefinition* new_def = &ontology->type_definitions[ontology->type_count];
    
    // 复制名称
    new_def->name = (char*)malloc(strlen(name) + 1);
    if (!new_def->name) {
        return -1;
    }
    strcpy(new_def->name, name);
    
    // 复制类型定义（深拷贝）
    new_def->type_def = kos_term_copy(type_def);
    if (!new_def->type_def) {
        free(new_def->name);
        return -1;
    }
    
    // 复制上下文（如果提供）
    if (ctx) {
        new_def->ctx = kos_term_copy(ctx);
        if (!new_def->ctx) {
            free(new_def->name);
            kos_term_free(new_def->type_def);
            return -1;
        }
    } else {
        new_def->ctx = NULL;
    }
    
    ontology->type_count++;
    return 0;
}

// 查找类型定义（返回类型定义的 kos_term*）
kos_term* kos_ontology_find_type_definition(TypeOntology* ontology, const char* name) {
    TypeDefinition* info = kos_ontology_get_type_definition_info(ontology, name);
    return info ? info->type_def : NULL;
}

// 获取类型定义的完整信息
TypeDefinition* kos_ontology_get_type_definition_info(TypeOntology* ontology, const char* name) {
    if (!ontology || !name) {
        return NULL;
    }
    
    for (size_t i = 0; i < ontology->type_count; i++) {
        if (strcmp(ontology->type_definitions[i].name, name) == 0) {
            return &ontology->type_definitions[i];
        }
    }
    
    return NULL;
}

// 删除类型定义
int kos_ontology_remove_type_definition(TypeOntology* ontology, const char* name) {
    if (!ontology || !name) {
        return -1;
    }
    
    for (size_t i = 0; i < ontology->type_count; i++) {
        if (strcmp(ontology->type_definitions[i].name, name) == 0) {
            TypeDefinition* def = &ontology->type_definitions[i];
            
            // 释放资源
            if (def->name) {
                free(def->name);
            }
            if (def->type_def) {
                kos_term_free(def->type_def);
            }
            if (def->ctx) {
                kos_term_free(def->ctx);
            }
            
            // 移动后续元素
            for (size_t j = i; j < ontology->type_count - 1; j++) {
                ontology->type_definitions[j] = ontology->type_definitions[j + 1];
            }
            
            ontology->type_count--;
            return 0;
        }
    }
    
    return -1;  // 未找到
}

// 更新类型定义
int kos_ontology_update_type_definition(TypeOntology* ontology,
                                        const char* name,
                                        kos_term* new_type_def,
                                        kos_term* new_ctx) {
    TypeDefinition* def = kos_ontology_get_type_definition_info(ontology, name);
    if (!def || !new_type_def) {
        return -1;
    }
    
    // 释放旧的类型定义
    if (def->type_def) {
        kos_term_free(def->type_def);
    }
    if (def->ctx) {
        kos_term_free(def->ctx);
    }
    
    // 设置新的类型定义
    def->type_def = kos_term_copy(new_type_def);
    if (!def->type_def) {
        return -1;
    }
    
    // 设置新的上下文
    if (new_ctx) {
        def->ctx = kos_term_copy(new_ctx);
        if (!def->ctx) {
            kos_term_free(def->type_def);
            def->type_def = NULL;
            return -1;
        }
    } else {
        def->ctx = NULL;
    }
    
    return 0;
}

// 根据类型定义构造类型实例
kos_term* kos_ontology_mk_type_instance(TypeOntology* ontology,
                                        const char* type_name,
                                        kos_term* data_term,
                                        kos_term* ctx) {
    if (!ontology || !type_name || !data_term) {
        return NULL;
    }
    
    // 步骤1：查找类型定义
    kos_term* type_def = kos_ontology_find_type_definition(ontology, type_name);
    if (!type_def) {
        return NULL;  // 类型定义不存在
    }
    
    // 步骤2：类型检查验证
    // 使用提供的上下文，如果没有则使用类型定义的上下文
    kos_term* check_ctx = ctx;
    TypeDefinition* def_info = kos_ontology_get_type_definition_info(ontology, type_name);
    if (!check_ctx && def_info && def_info->ctx) {
        check_ctx = def_info->ctx;
    }
    
    bool valid = kos_check(check_ctx, data_term, type_def);
    if (!valid) {
        return NULL;  // 类型检查失败
    }
    
    // 步骤3：构造实例
    // 对于Σ类型，data_term应该已经是 <data, proof> 对
    // 对于Π类型，data_term应该是λ抽象
    // 对于Sum类型，data_term应该是 inl 或 inr
    // 对于基础类型，data_term本身就是实例
    
    // 返回验证通过的实例（深拷贝）
    return kos_term_copy(data_term);
}

// 验证实例是否符合类型定义（通过类型检查）
bool kos_ontology_validate_instance(TypeOntology* ontology,
                                    kos_term* instance,
                                    const char* type_name,
                                    kos_term* ctx) {
    if (!ontology || !instance || !type_name) {
        return false;
    }
    
    // 查找类型定义
    kos_term* type_def = kos_ontology_find_type_definition(ontology, type_name);
    if (!type_def) {
        return false;  // 类型定义不存在
    }
    
    // 使用提供的上下文，如果没有则使用类型定义的上下文
    kos_term* check_ctx = ctx;
    TypeDefinition* def_info = kos_ontology_get_type_definition_info(ontology, type_name);
    if (!check_ctx && def_info && def_info->ctx) {
        check_ctx = def_info->ctx;
    }
    
    // 类型检查验证
    return kos_check(check_ctx, instance, type_def);
}

// 类型构造辅助函数：构造依赖和类型（Σ类型）
kos_term* kos_ontology_mk_sigma_type(kos_term* domain, kos_term* body) {
    // 这是对 kos_mk_sigma 的简单封装，提供更语义化的接口
    return kos_mk_sigma(domain, body);
}

// 类型构造辅助函数：构造依赖积类型（Π类型）
kos_term* kos_ontology_mk_pi_type(kos_term* domain, kos_term* body) {
    // 这是对 kos_mk_pi 的简单封装，提供更语义化的接口
    return kos_mk_pi(domain, body);
}

// 类型构造辅助函数：构造和类型（Sum类型）
kos_term* kos_ontology_mk_sum_type(kos_term* left_type, kos_term* right_type) {
    // 这是对 kos_mk_sum 的简单封装，提供更语义化的接口
    return kos_mk_sum(left_type, right_type);
}

// ========== 持久化存储 ==========
// 注意：这些函数需要实现JSON序列化/反序列化
// 由于kos_term的序列化功能已经在storage.c中实现，这里可以复用

// 保存本体到文件（JSON格式）
int kos_ontology_save_to_file(TypeOntology* ontology, const char* filename) {
    if (!ontology || !filename) {
        return -1;
    }
    
    // 序列化本体为JSON字符串
    char* json_str = kos_ontology_serialize(ontology);
    if (!json_str) {
        return -1;
    }
    
    // 写入文件
    FILE* file = fopen(filename, "w");
    if (!file) {
        free(json_str);
        return -1;
    }
    
    fprintf(file, "%s", json_str);
    fclose(file);
    free(json_str);
    
    return 0;
}

// 从文件加载本体
TypeOntology* kos_ontology_load_from_file(const char* filename) {
    if (!filename) {
        return NULL;
    }
    
    // 读取文件
    FILE* file = fopen(filename, "r");
    if (!file) {
        return NULL;
    }
    
    // 获取文件大小
    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    fseek(file, 0, SEEK_SET);
    
    if (file_size < 0) {
        fclose(file);
        return NULL;
    }
    
    // 读取文件内容
    char* json_str = (char*)malloc(file_size + 1);
    if (!json_str) {
        fclose(file);
        return NULL;
    }
    
    size_t read_size = fread(json_str, 1, file_size, file);
    fclose(file);
    json_str[read_size] = '\0';
    
    // 反序列化
    TypeOntology* ontology = kos_ontology_deserialize(json_str);
    free(json_str);
    
    return ontology;
}

// 辅助函数：扩展缓冲区
static char* expand_buffer(char* buffer, size_t* buffer_size, size_t required_size) {
    size_t new_size = *buffer_size;
    while (new_size < required_size) {
        new_size *= 2;
        if (new_size < *buffer_size) {  // 溢出检查
            free(buffer);
            return NULL;
        }
    }
    
    char* new_buffer = (char*)realloc(buffer, new_size);
    if (!new_buffer) {
        free(buffer);
        return NULL;
    }
    
    *buffer_size = new_size;
    return new_buffer;
}

// 辅助函数：安全追加字符串到缓冲区
static int safe_snprintf_append(char** buffer, size_t* buffer_size, size_t* pos, const char* format, ...) {
    va_list args;
    va_start(args, format);
    
    // 检查是否需要扩展缓冲区
    int needed = vsnprintf(NULL, 0, format, args);
    if (needed < 0) {
        va_end(args);
        return -1;
    }
    
    while (*pos + needed + 1 >= *buffer_size) {
        *buffer = expand_buffer(*buffer, buffer_size, *pos + needed + 1);
        if (!*buffer) {
            va_end(args);
            return -1;
        }
    }
    
    va_end(args);
    va_start(args, format);
    int written = vsnprintf(*buffer + *pos, *buffer_size - *pos, format, args);
    va_end(args);
    
    if (written > 0) {
        *pos += written;
    }
    
    return written;
}

// 序列化本体为JSON字符串
char* kos_ontology_serialize(TypeOntology* ontology) {
    if (!ontology) {
        return NULL;
    }
    
    // 使用动态缓冲区来构建JSON字符串（初始大小根据类型数量估算）
    size_t buffer_size = 8192;  // 初始8KB，会动态扩展
    char* buffer = (char*)malloc(buffer_size);
    if (!buffer) {
        return NULL;
    }
    
    size_t pos = 0;
    int written;
    
    // 开始对象
    while (1) {
        written = snprintf(buffer + pos, buffer_size - pos, "{\n");
        if (written < 0) { free(buffer); return NULL; }
        if (written < (int)(buffer_size - pos)) { pos += written; break; }
        buffer = expand_buffer(buffer, &buffer_size, pos + written + 1);
        if (!buffer) return NULL;
    }
    
    while (1) {
        written = snprintf(buffer + pos, buffer_size - pos, "  \"domain_name\": \"%s\",\n", ontology->domain_name);
        if (written < 0) { free(buffer); return NULL; }
        if (written < (int)(buffer_size - pos)) { pos += written; break; }
        buffer = expand_buffer(buffer, &buffer_size, pos + written + 1);
        if (!buffer) return NULL;
    }
    
    while (1) {
        written = snprintf(buffer + pos, buffer_size - pos, "  \"type_count\": %zu,\n", ontology->type_count);
        if (written < 0) { free(buffer); return NULL; }
        if (written < (int)(buffer_size - pos)) { pos += written; break; }
        buffer = expand_buffer(buffer, &buffer_size, pos + written + 1);
        if (!buffer) return NULL;
    }
    
    while (1) {
        written = snprintf(buffer + pos, buffer_size - pos, "  \"type_definitions\": [\n");
        if (written < 0) { free(buffer); return NULL; }
        if (written < (int)(buffer_size - pos)) { pos += written; break; }
        buffer = expand_buffer(buffer, &buffer_size, pos + written + 1);
        if (!buffer) return NULL;
    }
    
    // 序列化每个类型定义
    for (size_t i = 0; i < ontology->type_count; i++) {
        TypeDefinition* def = &ontology->type_definitions[i];
        
        while (1) {
            written = snprintf(buffer + pos, buffer_size - pos, "    {\n");
            if (written < 0) { free(buffer); return NULL; }
            if (written < (int)(buffer_size - pos)) { pos += written; break; }
            buffer = expand_buffer(buffer, &buffer_size, pos + written + 1);
            if (!buffer) return NULL;
        }
        
        while (1) {
            written = snprintf(buffer + pos, buffer_size - pos, "      \"name\": \"%s\",\n", def->name);
            if (written < 0) { free(buffer); return NULL; }
            if (written < (int)(buffer_size - pos)) { pos += written; break; }
            buffer = expand_buffer(buffer, &buffer_size, pos + written + 1);
            if (!buffer) return NULL;
        }
        
        // 序列化类型定义（kos_term*）
        kos_serialized* type_def_serialized = kos_term_serialize(def->type_def);
        if (type_def_serialized && type_def_serialized->data) {
            size_t type_def_len = strlen(type_def_serialized->data);
            while (pos + type_def_len + 50 >= buffer_size) {
                buffer = expand_buffer(buffer, &buffer_size, pos + type_def_len + 100);
                if (!buffer) {
                    kos_serialized_free(type_def_serialized);
                    return NULL;
                }
            }
            written = snprintf(buffer + pos, buffer_size - pos, "      \"type_def\": %s,\n", type_def_serialized->data);
            if (written > 0) pos += written;
            kos_serialized_free(type_def_serialized);
        } else {
            while (1) {
                written = snprintf(buffer + pos, buffer_size - pos, "      \"type_def\": null,\n");
                if (written < 0) { free(buffer); return NULL; }
                if (written < (int)(buffer_size - pos)) { pos += written; break; }
                buffer = expand_buffer(buffer, &buffer_size, pos + written + 1);
                if (!buffer) return NULL;
            }
        }
        
        // 序列化上下文
        if (def->ctx) {
            kos_serialized* ctx_serialized = kos_term_serialize(def->ctx);
            if (ctx_serialized && ctx_serialized->data) {
                size_t ctx_len = strlen(ctx_serialized->data);
                while (pos + ctx_len + 50 >= buffer_size) {
                    buffer = expand_buffer(buffer, &buffer_size, pos + ctx_len + 100);
                    if (!buffer) {
                        kos_serialized_free(ctx_serialized);
                        return NULL;
                    }
                }
                written = snprintf(buffer + pos, buffer_size - pos, "      \"ctx\": %s\n", ctx_serialized->data);
                if (written > 0) pos += written;
                kos_serialized_free(ctx_serialized);
            } else {
                while (1) {
                    written = snprintf(buffer + pos, buffer_size - pos, "      \"ctx\": null\n");
                    if (written < 0) { free(buffer); return NULL; }
                    if (written < (int)(buffer_size - pos)) { pos += written; break; }
                    buffer = expand_buffer(buffer, &buffer_size, pos + written + 1);
                    if (!buffer) return NULL;
                }
            }
        } else {
            while (1) {
                written = snprintf(buffer + pos, buffer_size - pos, "      \"ctx\": null\n");
                if (written < 0) { free(buffer); return NULL; }
                if (written < (int)(buffer_size - pos)) { pos += written; break; }
                buffer = expand_buffer(buffer, &buffer_size, pos + written + 1);
                if (!buffer) return NULL;
            }
        }
        
        while (1) {
            written = snprintf(buffer + pos, buffer_size - pos, "    }");
            if (written < 0) { free(buffer); return NULL; }
            if (written < (int)(buffer_size - pos)) { pos += written; break; }
            buffer = expand_buffer(buffer, &buffer_size, pos + written + 1);
            if (!buffer) return NULL;
        }
        
        if (i < ontology->type_count - 1) {
            while (1) {
                written = snprintf(buffer + pos, buffer_size - pos, ",");
                if (written < 0) { free(buffer); return NULL; }
                if (written < (int)(buffer_size - pos)) { pos += written; break; }
                buffer = expand_buffer(buffer, &buffer_size, pos + written + 1);
                if (!buffer) return NULL;
            }
        }
        
        while (1) {
            written = snprintf(buffer + pos, buffer_size - pos, "\n");
            if (written < 0) { free(buffer); return NULL; }
            if (written < (int)(buffer_size - pos)) { pos += written; break; }
            buffer = expand_buffer(buffer, &buffer_size, pos + written + 1);
            if (!buffer) return NULL;
        }
    }
    
    while (1) {
        written = snprintf(buffer + pos, buffer_size - pos, "  ]\n");
        if (written < 0) { free(buffer); return NULL; }
        if (written < (int)(buffer_size - pos)) { pos += written; break; }
        buffer = expand_buffer(buffer, &buffer_size, pos + written + 1);
        if (!buffer) return NULL;
    }
    
    while (1) {
        written = snprintf(buffer + pos, buffer_size - pos, "}\n");
        if (written < 0) { free(buffer); return NULL; }
        if (written < (int)(buffer_size - pos)) { pos += written; break; }
        buffer = expand_buffer(buffer, &buffer_size, pos + written + 1);
        if (!buffer) return NULL;
    }
    
    return buffer;
}

// 辅助函数：从JSON字符串中提取字符串值
static const char* extract_json_string_value(const char* json_str, const char* key, char** out_value) {
    char key_pattern[256];
    snprintf(key_pattern, sizeof(key_pattern), "\"%s\"", key);
    
    const char* key_pos = strstr(json_str, key_pattern);
    if (!key_pos) {
        return NULL;
    }
    
    // 跳过键名和冒号
    const char* value_start = key_pos + strlen(key_pattern);
    while (*value_start == ' ' || *value_start == '\t' || *value_start == '\n' || *value_start == '\r') {
        value_start++;
    }
    if (*value_start != ':') {
        return NULL;
    }
    value_start++;
    while (*value_start == ' ' || *value_start == '\t' || *value_start == '\n' || *value_start == '\r') {
        value_start++;
    }
    
    // 解析字符串值
    if (*value_start != '"') {
        return NULL;
    }
    
    const char* after_value = parse_json_string(value_start, out_value);
    return after_value;
}

// 辅助函数：从JSON字符串中提取数字值
static const char* extract_json_number_value(const char* json_str, const char* key, size_t* out_value) {
    char key_pattern[256];
    snprintf(key_pattern, sizeof(key_pattern), "\"%s\"", key);
    
    const char* key_pos = strstr(json_str, key_pattern);
    if (!key_pos) {
        return NULL;
    }
    
    // 跳过键名和冒号
    const char* value_start = key_pos + strlen(key_pattern);
    while (*value_start == ' ' || *value_start == '\t' || *value_start == '\n' || *value_start == '\r') {
        value_start++;
    }
    if (*value_start != ':') {
        return NULL;
    }
    value_start++;
    while (*value_start == ' ' || *value_start == '\t' || *value_start == '\n' || *value_start == '\r') {
        value_start++;
    }
    
    // 解析数字值
    int num = 0;
    const char* after_value = parse_json_number(value_start, &num);
    *out_value = (size_t)num;
    return after_value;
}

// 辅助函数：提取JSON数组中的元素（找到下一个数组元素）
static const char* find_next_array_element(const char* start_pos, char** out_element) {
    (void)start_pos; // 参数未使用，保留接口一致性
    const char* p = start_pos;
    p = skip_whitespace(p);
    
    if (*p == ']') {
        return NULL; // 数组结束
    }
    
    if (*p == ',') {
        p++;
        p = skip_whitespace(p);
    }
    
    // 提取元素（可能是对象、字符串、null等）
    if (*p == '{') {
        // 提取对象
        return extract_json_object(p, out_element);
    } else if (*p == '"') {
        // 提取字符串
        return parse_json_string(p, out_element);
    } else if (strncmp(p, "null", 4) == 0) {
        // null值
        *out_element = NULL;
        return p + 4;
    } else {
        // 其他情况，尝试提取到下一个逗号或]
        const char* end = p;
        while (*end != '\0' && *end != ',' && *end != ']' && *end != '}') {
            end++;
        }
        size_t len = end - p;
        *out_element = (char*)malloc(len + 1);
        if (!*out_element) {
            return NULL;
        }
        memcpy(*out_element, p, len);
        (*out_element)[len] = '\0';
        return end;
    }
}

// 辅助函数：查找JSON对象中键的值（返回值的起始位置）
static const char* find_json_value(const char* json_str, const char* key) {
    char key_pattern[256];
    snprintf(key_pattern, sizeof(key_pattern), "\"%s\"", key);
    
    const char* key_pos = strstr(json_str, key_pattern);
    if (!key_pos) {
        return NULL;
    }
    
    // 跳过键名和冒号
    const char* value_start = key_pos + strlen(key_pattern);
    value_start = skip_whitespace(value_start);
    if (*value_start == ':') {
        value_start++;
        value_start = skip_whitespace(value_start);
        return value_start;
    }
    
    return NULL;
}

// 从JSON字符串反序列化本体
TypeOntology* kos_ontology_deserialize(const char* json_str) {
    if (!json_str) {
        return NULL;
    }
    
    // 1. 提取domain_name
    char* domain_name = NULL;
    extract_json_string_value(json_str, "domain_name", &domain_name);
    if (!domain_name) {
        return NULL;
    }
    
    // 2. 创建本体
    TypeOntology* ontology = kos_ontology_create(domain_name);
    free(domain_name);
    if (!ontology) {
        return NULL;
    }
    
    // 3. 提取type_count（可选，用于验证）
    size_t type_count = 0;
    extract_json_number_value(json_str, "type_count", &type_count);
    
    // 4. 查找type_definitions数组
    const char* array_start = strstr(json_str, "\"type_definitions\"");
    if (!array_start) {
        kos_ontology_free(ontology);
        return NULL;
    }
    
    // 跳过键名和冒号，找到数组开始标记
    array_start = strchr(array_start, '[');
    if (!array_start) {
        kos_ontology_free(ontology);
        return NULL;
    }
    array_start++; // 跳过 '['
    
    // 5. 解析数组中的每个类型定义
    const char* current_pos = array_start;
    while (current_pos && *current_pos != '\0') {
        current_pos = skip_whitespace(current_pos);
        if (*current_pos == ']') {
            break; // 数组结束
        }
        
        // 提取类型定义对象
        char* type_def_obj = NULL;
        const char* next_pos = extract_json_object(current_pos, &type_def_obj);
        if (!type_def_obj) {
            break;
        }
        
        // 解析类型定义
        char* name = NULL;
        extract_json_string_value(type_def_obj, "name", &name);
        
        // 提取type_def（可能是对象或null）
        const char* type_def_value = find_json_value(type_def_obj, "type_def");
        kos_term* type_def = NULL;
        if (type_def_value && parse_json_null(type_def_value) == NULL) {
            type_def = kos_term_deserialize(type_def_value);
        }
        
        // 提取ctx（可能是对象或null）
        const char* ctx_value = find_json_value(type_def_obj, "ctx");
        kos_term* ctx = NULL;
        if (ctx_value && parse_json_null(ctx_value) == NULL) {
            ctx = kos_term_deserialize(ctx_value);
        }
        
        // 添加类型定义
        if (name && type_def) {
            kos_ontology_add_type_definition(ontology, name, type_def, ctx);
        }
        
        // 清理
        free(name);
        if (type_def) {
            kos_term_free(type_def);
        }
        if (ctx) {
            kos_term_free(ctx);
        }
        free(type_def_obj);
        
        // 移动到下一个元素
        current_pos = next_pos;
        if (current_pos) {
            current_pos = skip_whitespace(current_pos);
            if (*current_pos == ',') {
                current_pos++;
            }
        }
    }
    
    return ontology;
}

