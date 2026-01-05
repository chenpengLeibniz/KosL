// src/core/ontology_manager.c
// 类型本体管理器：提供类型本体的CRUD和持久化功能

#include "../../include/kos_ontology.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

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
    
    ontology->atomic_types = NULL;
    ontology->atomic_count = 0;
    ontology->predicate_types = NULL;
    ontology->predicate_count = 0;
    ontology->event_types = NULL;
    ontology->event_count = 0;
    
    return ontology;
}

// 释放本体
void kos_ontology_free(TypeOntology* ontology) {
    if (!ontology) {
        return;
    }
    
    // 释放原子类型
    if (ontology->atomic_types) {
        for (size_t i = 0; i < ontology->atomic_count; i++) {
            if (ontology->atomic_types[i].constraints) {
                kos_term_free(ontology->atomic_types[i].constraints);
            }
        }
        free(ontology->atomic_types);
    }
    
    // 释放谓词类型
    if (ontology->predicate_types) {
        for (size_t i = 0; i < ontology->predicate_count; i++) {
            if (ontology->predicate_types[i].body) {
                kos_term_free(ontology->predicate_types[i].body);
            }
            // 释放参数类型字符串
            for (int j = 0; j < ontology->predicate_types[i].param_count; j++) {
                if (ontology->predicate_types[i].param_types[j]) {
                    free(ontology->predicate_types[i].param_types[j]);
                }
            }
        }
        free(ontology->predicate_types);
    }
    
    // 释放事件类型
    if (ontology->event_types) {
        for (size_t i = 0; i < ontology->event_count; i++) {
            if (ontology->event_types[i].preconditions) {
                kos_term_free(ontology->event_types[i].preconditions);
            }
            // 释放字段名称和类型字符串
            for (int j = 0; j < ontology->event_types[i].field_count; j++) {
                if (ontology->event_types[i].field_names[j]) {
                    free(ontology->event_types[i].field_names[j]);
                }
                if (ontology->event_types[i].field_types[j]) {
                    free(ontology->event_types[i].field_types[j]);
                }
            }
        }
        free(ontology->event_types);
    }
    
    free(ontology);
}

// 添加原子类型
int kos_ontology_add_atomic_type(TypeOntology* ontology,
                                 const char* name,
                                 const char* base_type,
                                 kos_term* constraints) {
    if (!ontology || !name || !base_type) {
        return -1;
    }
    
    // 检查是否已存在
    if (kos_ontology_find_atomic_type(ontology, name)) {
        return -1; // 已存在
    }
    
    // 扩展数组
    size_t new_count = ontology->atomic_count + 1;
    AtomicTypeDef* new_array = (AtomicTypeDef*)realloc(
        ontology->atomic_types, new_count * sizeof(AtomicTypeDef));
    if (!new_array) {
        return -1;
    }
    
    ontology->atomic_types = new_array;
    AtomicTypeDef* new_type = &ontology->atomic_types[ontology->atomic_count];
    
    strncpy(new_type->name, name, sizeof(new_type->name) - 1);
    new_type->name[sizeof(new_type->name) - 1] = '\0';
    
    strncpy(new_type->base_type, base_type, sizeof(new_type->base_type) - 1);
    new_type->base_type[sizeof(new_type->base_type) - 1] = '\0';
    
    new_type->constraints = constraints;
    
    ontology->atomic_count = new_count;
    return 0;
}

// 查找原子类型
AtomicTypeDef* kos_ontology_find_atomic_type(TypeOntology* ontology, const char* name) {
    if (!ontology || !name) {
        return NULL;
    }
    
    for (size_t i = 0; i < ontology->atomic_count; i++) {
        if (strcmp(ontology->atomic_types[i].name, name) == 0) {
            return &ontology->atomic_types[i];
        }
    }
    
    return NULL;
}

// 删除原子类型
int kos_ontology_remove_atomic_type(TypeOntology* ontology, const char* name) {
    if (!ontology || !name) {
        return -1;
    }
    
    for (size_t i = 0; i < ontology->atomic_count; i++) {
        if (strcmp(ontology->atomic_types[i].name, name) == 0) {
            // 释放约束
            if (ontology->atomic_types[i].constraints) {
                kos_term_free(ontology->atomic_types[i].constraints);
            }
            
            // 移动后续元素
            for (size_t j = i; j < ontology->atomic_count - 1; j++) {
                ontology->atomic_types[j] = ontology->atomic_types[j + 1];
            }
            
            ontology->atomic_count--;
            
            // 缩小数组
            if (ontology->atomic_count > 0) {
                AtomicTypeDef* new_array = (AtomicTypeDef*)realloc(
                    ontology->atomic_types,
                    ontology->atomic_count * sizeof(AtomicTypeDef));
                if (new_array) {
                    ontology->atomic_types = new_array;
                }
            } else {
                free(ontology->atomic_types);
                ontology->atomic_types = NULL;
            }
            
            return 0;
        }
    }
    
    return -1; // 未找到
}

// 更新原子类型
int kos_ontology_update_atomic_type(TypeOntology* ontology,
                                    const char* name,
                                    const char* new_base_type,
                                    kos_term* new_constraints) {
    AtomicTypeDef* type = kos_ontology_find_atomic_type(ontology, name);
    if (!type) {
        return -1;
    }
    
    if (new_base_type) {
        strncpy(type->base_type, new_base_type, sizeof(type->base_type) - 1);
        type->base_type[sizeof(type->base_type) - 1] = '\0';
    }
    
    if (new_constraints) {
        if (type->constraints) {
            kos_term_free(type->constraints);
        }
        type->constraints = new_constraints;
    }
    
    return 0;
}

// 添加谓词类型
int kos_ontology_add_predicate_type(TypeOntology* ontology,
                                    const char* name,
                                    const char** param_types,
                                    int param_count,
                                    kos_term* body) {
    if (!ontology || !name || param_count < 0 || param_count > 16) {
        return -1;
    }
    
    // 检查是否已存在
    if (kos_ontology_find_predicate_type(ontology, name)) {
        return -1;
    }
    
    // 扩展数组
    size_t new_count = ontology->predicate_count + 1;
    PredicateTypeDef* new_array = (PredicateTypeDef*)realloc(
        ontology->predicate_types, new_count * sizeof(PredicateTypeDef));
    if (!new_array) {
        return -1;
    }
    
    ontology->predicate_types = new_array;
    PredicateTypeDef* new_pred = &ontology->predicate_types[ontology->predicate_count];
    
    strncpy(new_pred->name, name, sizeof(new_pred->name) - 1);
    new_pred->name[sizeof(new_pred->name) - 1] = '\0';
    
    new_pred->param_count = param_count;
    for (int i = 0; i < param_count; i++) {
        if (param_types[i]) {
            new_pred->param_types[i] = (char*)malloc(strlen(param_types[i]) + 1);
            if (new_pred->param_types[i]) {
                strcpy(new_pred->param_types[i], param_types[i]);
            }
        } else {
            new_pred->param_types[i] = NULL;
        }
    }
    
    new_pred->body = body;
    
    ontology->predicate_count = new_count;
    return 0;
}

// 查找谓词类型
PredicateTypeDef* kos_ontology_find_predicate_type(TypeOntology* ontology, const char* name) {
    if (!ontology || !name) {
        return NULL;
    }
    
    for (size_t i = 0; i < ontology->predicate_count; i++) {
        if (strcmp(ontology->predicate_types[i].name, name) == 0) {
            return &ontology->predicate_types[i];
        }
    }
    
    return NULL;
}

// 删除谓词类型
int kos_ontology_remove_predicate_type(TypeOntology* ontology, const char* name) {
    if (!ontology || !name) {
        return -1;
    }
    
    for (size_t i = 0; i < ontology->predicate_count; i++) {
        if (strcmp(ontology->predicate_types[i].name, name) == 0) {
            // 释放资源
            if (ontology->predicate_types[i].body) {
                kos_term_free(ontology->predicate_types[i].body);
            }
            for (int j = 0; j < ontology->predicate_types[i].param_count; j++) {
                if (ontology->predicate_types[i].param_types[j]) {
                    free(ontology->predicate_types[i].param_types[j]);
                }
            }
            
            // 移动后续元素
            for (size_t j = i; j < ontology->predicate_count - 1; j++) {
                ontology->predicate_types[j] = ontology->predicate_types[j + 1];
            }
            
            ontology->predicate_count--;
            
            // 缩小数组
            if (ontology->predicate_count > 0) {
                PredicateTypeDef* new_array = (PredicateTypeDef*)realloc(
                    ontology->predicate_types,
                    ontology->predicate_count * sizeof(PredicateTypeDef));
                if (new_array) {
                    ontology->predicate_types = new_array;
                }
            } else {
                free(ontology->predicate_types);
                ontology->predicate_types = NULL;
            }
            
            return 0;
        }
    }
    
    return -1;
}

// 添加事件类型
int kos_ontology_add_event_type(TypeOntology* ontology,
                                const char* name,
                                const char** field_names,
                                const char** field_types,
                                int field_count,
                                kos_term* preconditions) {
    if (!ontology || !name || field_count < 0 || field_count > 16) {
        return -1;
    }
    
    // 检查是否已存在
    if (kos_ontology_find_event_type(ontology, name)) {
        return -1;
    }
    
    // 扩展数组
    size_t new_count = ontology->event_count + 1;
    EventTypeDef* new_array = (EventTypeDef*)realloc(
        ontology->event_types, new_count * sizeof(EventTypeDef));
    if (!new_array) {
        return -1;
    }
    
    ontology->event_types = new_array;
    EventTypeDef* new_event = &ontology->event_types[ontology->event_count];
    
    strncpy(new_event->name, name, sizeof(new_event->name) - 1);
    new_event->name[sizeof(new_event->name) - 1] = '\0';
    
    new_event->field_count = field_count;
    for (int i = 0; i < field_count; i++) {
        if (field_names[i]) {
            new_event->field_names[i] = (char*)malloc(strlen(field_names[i]) + 1);
            if (new_event->field_names[i]) {
                strcpy(new_event->field_names[i], field_names[i]);
            }
        } else {
            new_event->field_names[i] = NULL;
        }
        
        if (field_types[i]) {
            new_event->field_types[i] = (char*)malloc(strlen(field_types[i]) + 1);
            if (new_event->field_types[i]) {
                strcpy(new_event->field_types[i], field_types[i]);
            }
        } else {
            new_event->field_types[i] = NULL;
        }
    }
    
    new_event->preconditions = preconditions;
    
    ontology->event_count = new_count;
    return 0;
}

// 查找事件类型
EventTypeDef* kos_ontology_find_event_type(TypeOntology* ontology, const char* name) {
    if (!ontology || !name) {
        return NULL;
    }
    
    for (size_t i = 0; i < ontology->event_count; i++) {
        if (strcmp(ontology->event_types[i].name, name) == 0) {
            return &ontology->event_types[i];
        }
    }
    
    return NULL;
}

// 删除事件类型
int kos_ontology_remove_event_type(TypeOntology* ontology, const char* name) {
    if (!ontology || !name) {
        return -1;
    }
    
    for (size_t i = 0; i < ontology->event_count; i++) {
        if (strcmp(ontology->event_types[i].name, name) == 0) {
            // 释放资源
            if (ontology->event_types[i].preconditions) {
                kos_term_free(ontology->event_types[i].preconditions);
            }
            for (int j = 0; j < ontology->event_types[i].field_count; j++) {
                if (ontology->event_types[i].field_names[j]) {
                    free(ontology->event_types[i].field_names[j]);
                }
                if (ontology->event_types[i].field_types[j]) {
                    free(ontology->event_types[i].field_types[j]);
                }
            }
            
            // 移动后续元素
            for (size_t j = i; j < ontology->event_count - 1; j++) {
                ontology->event_types[j] = ontology->event_types[j + 1];
            }
            
            ontology->event_count--;
            
            // 缩小数组
            if (ontology->event_count > 0) {
                EventTypeDef* new_array = (EventTypeDef*)realloc(
                    ontology->event_types,
                    ontology->event_count * sizeof(EventTypeDef));
                if (new_array) {
                    ontology->event_types = new_array;
                }
            } else {
                free(ontology->event_types);
                ontology->event_types = NULL;
            }
            
            return 0;
        }
    }
    
    return -1;
}

// 保存本体到文件（JSON格式）
int kos_ontology_save_to_file(TypeOntology* ontology, const char* filename) {
    if (!ontology || !filename) {
        return -1;
    }
    
    char* json = kos_ontology_serialize(ontology);
    if (!json) {
        return -1;
    }
    
    FILE* file = fopen(filename, "w");
    if (!file) {
        free(json);
        return -1;
    }
    
    size_t len = strlen(json);
    size_t written = fwrite(json, 1, len, file);
    fclose(file);
    free(json);
    
    return (written == len) ? 0 : -1;
}

// 从文件加载本体
TypeOntology* kos_ontology_load_from_file(const char* filename) {
    if (!filename) {
        return NULL;
    }
    
    FILE* file = fopen(filename, "r");
    if (!file) {
        return NULL;
    }
    
    // 获取文件大小
    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    fseek(file, 0, SEEK_SET);
    
    if (file_size <= 0) {
        fclose(file);
        return NULL;
    }
    
    // 读取文件内容
    char* buffer = (char*)malloc(file_size + 1);
    if (!buffer) {
        fclose(file);
        return NULL;
    }
    
    size_t read = fread(buffer, 1, file_size, file);
    fclose(file);
    
    if (read != (size_t)file_size) {
        free(buffer);
        return NULL;
    }
    
    buffer[file_size] = '\0';
    
    // 反序列化
    TypeOntology* ontology = kos_ontology_deserialize(buffer);
    free(buffer);
    
    return ontology;
}

// 序列化本体为JSON字符串（简化实现）
char* kos_ontology_serialize(TypeOntology* ontology) {
    if (!ontology) {
        return NULL;
    }
    
    // 简化实现：使用固定大小的缓冲区
    // 实际实现应该动态分配或使用流式JSON库
    size_t buffer_size = 64 * 1024; // 64KB
    char* json = (char*)malloc(buffer_size);
    if (!json) {
        return NULL;
    }
    
    size_t pos = 0;
    
    // 开始JSON对象
    pos += snprintf(json + pos, buffer_size - pos, "{\n");
    pos += snprintf(json + pos, buffer_size - pos, "  \"domain\": \"%s\",\n", ontology->domain_name);
    
    // 序列化原子类型
    pos += snprintf(json + pos, buffer_size - pos, "  \"atomic_types\": [\n");
    for (size_t i = 0; i < ontology->atomic_count; i++) {
        pos += snprintf(json + pos, buffer_size - pos, "    {\n");
        pos += snprintf(json + pos, buffer_size - pos, "      \"name\": \"%s\",\n",
                       ontology->atomic_types[i].name);
        pos += snprintf(json + pos, buffer_size - pos, "      \"base_type\": \"%s\"\n",
                       ontology->atomic_types[i].base_type);
        pos += snprintf(json + pos, buffer_size - pos, "    }%s\n",
                       (i < ontology->atomic_count - 1) ? "," : "");
    }
    pos += snprintf(json + pos, buffer_size - pos, "  ],\n");
    
    // 序列化谓词类型
    pos += snprintf(json + pos, buffer_size - pos, "  \"predicate_types\": [\n");
    for (size_t i = 0; i < ontology->predicate_count; i++) {
        pos += snprintf(json + pos, buffer_size - pos, "    {\n");
        pos += snprintf(json + pos, buffer_size - pos, "      \"name\": \"%s\",\n",
                       ontology->predicate_types[i].name);
        pos += snprintf(json + pos, buffer_size - pos, "      \"param_count\": %d\n",
                       ontology->predicate_types[i].param_count);
        pos += snprintf(json + pos, buffer_size - pos, "    }%s\n",
                       (i < ontology->predicate_count - 1) ? "," : "");
    }
    pos += snprintf(json + pos, buffer_size - pos, "  ],\n");
    
    // 序列化事件类型
    pos += snprintf(json + pos, buffer_size - pos, "  \"event_types\": [\n");
    for (size_t i = 0; i < ontology->event_count; i++) {
        pos += snprintf(json + pos, buffer_size - pos, "    {\n");
        pos += snprintf(json + pos, buffer_size - pos, "      \"name\": \"%s\",\n",
                       ontology->event_types[i].name);
        pos += snprintf(json + pos, buffer_size - pos, "      \"field_count\": %d\n",
                       ontology->event_types[i].field_count);
        pos += snprintf(json + pos, buffer_size - pos, "    }%s\n",
                       (i < ontology->event_count - 1) ? "," : "");
    }
    pos += snprintf(json + pos, buffer_size - pos, "  ]\n");
    
    // 结束JSON对象
    pos += snprintf(json + pos, buffer_size - pos, "}\n");
    
    return json;
}

// 从JSON字符串反序列化本体（简化实现）
TypeOntology* kos_ontology_deserialize(const char* json_str) {
    // TODO: 实现完整的JSON解析
    // 这里返回NULL作为占位符
    (void)json_str;
    return NULL;
}

// 根据本体定义构建类型实例
kos_term* kos_ontology_mk_type_instance(TypeOntology* ontology,
                                        const char* type_name,
                                        void* data) {
    if (!ontology || !type_name) {
        return NULL;
    }
    
    // 查找类型定义
    AtomicTypeDef* type_def = kos_ontology_find_atomic_type(ontology, type_name);
    if (!type_def) {
        return NULL;
    }
    
    // 根据基础类型构建实例
    if (strcmp(type_def->base_type, "String") == 0) {
        char* str_data = (char*)data;
        return kos_mk_val(str_data);
    }
    
    // 其他类型处理...
    return NULL;
}

// 根据事件类型定义构建事件实例
kos_term* kos_ontology_mk_event_instance(TypeOntology* ontology,
                                         const char* event_type_name,
                                         void* event_data) {
    (void)event_data; // 暂时未使用
    if (!ontology || !event_type_name) {
        return NULL;
    }
    
    EventTypeDef* event_def = kos_ontology_find_event_type(ontology, event_type_name);
    if (!event_def) {
        return NULL;
    }
    
    // 根据事件类型定义构建实例
    // 这里需要根据event_data解析字段并构建
    // 简化实现
    return NULL;
}

// 验证实例是否符合类型定义
bool kos_ontology_validate_instance(TypeOntology* ontology,
                                    kos_term* instance,
                                    const char* type_name) {
    if (!ontology || !instance || !type_name) {
        return false;
    }
    
    AtomicTypeDef* type_def = kos_ontology_find_atomic_type(ontology, type_name);
    if (!type_def) {
        return false;
    }
    
    // 验证约束
    if (type_def->constraints) {
        return kos_type_check(NULL, instance, type_def->constraints);
    }
    
    return true;
}

