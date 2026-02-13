// src/core/ontology_manager.c
// 类型本体管理器：基于类型构造的类型系统实现
// 所有类型定义都通过类型构造器（Π、Σ、Sum等）构造

#include "../../include/kos_ontology.h"
#include "../../include/kos_core.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

#define INITIAL_CAPACITY 16

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
// Core 层约束：type_def 必须为良构类型，否则拒绝注册
int kos_ontology_add_type_definition(TypeOntology* ontology,
                                     const char* name,
                                     kos_term* type_def,
                                     kos_term* ctx) {
    if (!ontology || !name || !type_def) {
        return -1;
    }
    if (!kos_type_wellformed(type_def)) {
        return -1;  /* 非法类型：不能建构的类型不得注册 */
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

// 序列化本体为JSON字符串
char* kos_ontology_serialize(TypeOntology* ontology) {
    if (!ontology) {
        return NULL;
    }
    
    // 简化的JSON序列化实现
    // 实际实现应该使用更完善的JSON库，这里提供基本框架
    
    size_t buffer_size = 4096;
    char* buffer = (char*)malloc(buffer_size);
    if (!buffer) {
        return NULL;
    }
    
    int pos = 0;
    
    // 开始对象
    pos += snprintf(buffer + pos, buffer_size - pos, "{\n");
    pos += snprintf(buffer + pos, buffer_size - pos, "  \"domain_name\": \"%s\",\n", ontology->domain_name);
    pos += snprintf(buffer + pos, buffer_size - pos, "  \"type_count\": %zu,\n", ontology->type_count);
    pos += snprintf(buffer + pos, buffer_size - pos, "  \"type_definitions\": [\n");
    
    // 序列化每个类型定义
    for (size_t i = 0; i < ontology->type_count; i++) {
        TypeDefinition* def = &ontology->type_definitions[i];
        
        pos += snprintf(buffer + pos, buffer_size - pos, "    {\n");
        pos += snprintf(buffer + pos, buffer_size - pos, "      \"name\": \"%s\",\n", def->name);
        
        // 序列化类型定义（kos_term*）
        // 这里需要调用 kos_term_serialize
        // 为了简化，这里只是占位符
        pos += snprintf(buffer + pos, buffer_size - pos, "      \"type_def\": \"<serialized_term>\",\n");
        
        if (def->ctx) {
            pos += snprintf(buffer + pos, buffer_size - pos, "      \"ctx\": \"<serialized_term>\"\n");
        } else {
            pos += snprintf(buffer + pos, buffer_size - pos, "      \"ctx\": null\n");
        }
        
        pos += snprintf(buffer + pos, buffer_size - pos, "    }");
        if (i < ontology->type_count - 1) {
            pos += snprintf(buffer + pos, buffer_size - pos, ",");
        }
        pos += snprintf(buffer + pos, buffer_size - pos, "\n");
    }
    
    pos += snprintf(buffer + pos, buffer_size - pos, "  ]\n");
    pos += snprintf(buffer + pos, buffer_size - pos, "}\n");
    
    return buffer;
}

// 从JSON字符串反序列化本体
TypeOntology* kos_ontology_deserialize(const char* json_str) {
    // 这是一个简化的实现
    // 实际实现应该使用更完善的JSON解析库
    // 这里只是占位符，需要完整实现JSON解析和kos_term反序列化
    
    // TODO: 实现完整的JSON解析
    // 1. 解析JSON对象
    // 2. 提取domain_name和type_count
    // 3. 解析type_definitions数组
    // 4. 对每个类型定义，反序列化type_def和ctx（使用kos_term_deserialize）
    // 5. 调用kos_ontology_add_type_definition添加类型定义
    
    // 暂时返回NULL，表示未实现
    (void)json_str;  // 避免未使用参数警告
    return NULL;
}































