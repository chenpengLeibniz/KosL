// src/core/storage.c
// Core 层存储和加载工具：序列化/反序列化 kos_term

#include "kos_core.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

// 辅助函数：转义 JSON 字符串
static void json_escape_string(const char* str, char* buffer, size_t buffer_size) {
    if (!str || !buffer || buffer_size == 0) {
        return;
    }
    
    size_t pos = 0;
    buffer[pos++] = '"';
    
    for (size_t i = 0; str[i] != '\0' && pos < buffer_size - 2; i++) {
        switch (str[i]) {
            case '"': 
                if (pos < buffer_size - 2) { buffer[pos++] = '\\'; buffer[pos++] = '"'; }
                break;
            case '\\': 
                if (pos < buffer_size - 2) { buffer[pos++] = '\\'; buffer[pos++] = '\\'; }
                break;
            case '\n': 
                if (pos < buffer_size - 2) { buffer[pos++] = '\\'; buffer[pos++] = 'n'; }
                break;
            case '\r': 
                if (pos < buffer_size - 2) { buffer[pos++] = '\\'; buffer[pos++] = 'r'; }
                break;
            case '\t': 
                if (pos < buffer_size - 2) { buffer[pos++] = '\\'; buffer[pos++] = 't'; }
                break;
            default:
                buffer[pos++] = str[i];
                break;
        }
    }
    
    buffer[pos++] = '"';
    buffer[pos] = '\0';
}

// 序列化 term 为 JSON 格式（递归）
static void serialize_term_recursive(kos_term* t, char* buffer, size_t buffer_size, size_t* pos) {
    if (!t || !buffer || !pos) {
        return;
    }
    
    if (*pos >= buffer_size - 1) {
        return; // 缓冲区不足
    }
    
    buffer[*pos] = '{';
    (*pos)++;
    
    // 写入 kind
    const char* kind_str = "";
    switch (t->kind) {
        case KOS_VAL: kind_str = "VAL"; break;
        case KOS_TIME: kind_str = "TIME"; break;
        case KOS_ID: kind_str = "ID"; break;
        case KOS_SIGMA: kind_str = "SIGMA"; break;
        case KOS_PAIR: kind_str = "PAIR"; break;
        case KOS_PROP: kind_str = "PROP"; break;
        case KOS_PI: kind_str = "PI"; break;
        case KOS_SUM: kind_str = "SUM"; break;
        case KOS_U: kind_str = "U"; break;
        case KOS_TYPE: kind_str = "TYPE"; break;
    }
    
    snprintf(buffer + *pos, buffer_size - *pos, "\"kind\":\"%s\"", kind_str);
    *pos += strlen(buffer + *pos);
    
    // 根据类型写入数据
    switch (t->kind) {
        case KOS_VAL:
        case KOS_TIME:
        case KOS_ID:
        case KOS_PROP:
            if (t->data.atomic.val) {
                char escaped[1024];
                json_escape_string(t->data.atomic.val, escaped, sizeof(escaped));
                snprintf(buffer + *pos, buffer_size - *pos, ",\"val\":%s", escaped);
                *pos += strlen(buffer + *pos);
            }
            if (t->data.atomic.type) {
                snprintf(buffer + *pos, buffer_size - *pos, ",\"type\":");
                *pos += strlen(buffer + *pos);
                serialize_term_recursive(t->data.atomic.type, buffer, buffer_size, pos);
            }
            break;
            
        case KOS_PAIR:
            snprintf(buffer + *pos, buffer_size - *pos, ",\"data\":");
            *pos += strlen(buffer + *pos);
            serialize_term_recursive(t->data.pair.data, buffer, buffer_size, pos);
            
            snprintf(buffer + *pos, buffer_size - *pos, ",\"proof\":");
            *pos += strlen(buffer + *pos);
            serialize_term_recursive(t->data.pair.proof, buffer, buffer_size, pos);
            break;
            
        case KOS_SIGMA:
            snprintf(buffer + *pos, buffer_size - *pos, ",\"domain\":");
            *pos += strlen(buffer + *pos);
            serialize_term_recursive(t->data.sigma.domain, buffer, buffer_size, pos);
            
            snprintf(buffer + *pos, buffer_size - *pos, ",\"body\":");
            *pos += strlen(buffer + *pos);
            serialize_term_recursive(t->data.sigma.body, buffer, buffer_size, pos);
            break;
            
        case KOS_PI:
            snprintf(buffer + *pos, buffer_size - *pos, ",\"domain\":");
            *pos += strlen(buffer + *pos);
            serialize_term_recursive(t->data.pi.domain, buffer, buffer_size, pos);
            
            if (t->data.pi.body) {
                snprintf(buffer + *pos, buffer_size - *pos, ",\"body\":");
                *pos += strlen(buffer + *pos);
                serialize_term_recursive(t->data.pi.body, buffer, buffer_size, pos);
            }
            if (t->data.pi.body_term) {
                snprintf(buffer + *pos, buffer_size - *pos, ",\"body_term\":");
                *pos += strlen(buffer + *pos);
                serialize_term_recursive(t->data.pi.body_term, buffer, buffer_size, pos);
            }
            break;
            
        case KOS_SUM:
            snprintf(buffer + *pos, buffer_size - *pos, ",\"left_type\":");
            *pos += strlen(buffer + *pos);
            serialize_term_recursive(t->data.sum.left_type, buffer, buffer_size, pos);
            
            snprintf(buffer + *pos, buffer_size - *pos, ",\"right_type\":");
            *pos += strlen(buffer + *pos);
            serialize_term_recursive(t->data.sum.right_type, buffer, buffer_size, pos);
            
            if (t->data.sum.value) {
                snprintf(buffer + *pos, buffer_size - *pos, ",\"is_left\":%s", t->data.sum.is_left ? "true" : "false");
                *pos += strlen(buffer + *pos);
                snprintf(buffer + *pos, buffer_size - *pos, ",\"value\":");
                *pos += strlen(buffer + *pos);
                serialize_term_recursive(t->data.sum.value, buffer, buffer_size, pos);
            }
            break;
            
        case KOS_U:
        case KOS_TYPE:
            snprintf(buffer + *pos, buffer_size - *pos, ",\"axis\":%d", t->data.universe.axis);
            *pos += strlen(buffer + *pos);
            snprintf(buffer + *pos, buffer_size - *pos, ",\"level\":%d", t->data.universe.level);
            *pos += strlen(buffer + *pos);
            break;
    }
    
    buffer[*pos] = '}';
    (*pos)++;
    buffer[*pos] = '\0';
}

// 序列化 term 为 JSON 格式
kos_serialized* kos_term_serialize(kos_term* t) {
    if (!t) {
        return NULL;
    }
    
    // 分配足够大的缓冲区（简化实现，实际应该动态调整）
    size_t buffer_size = 64 * 1024; // 64KB
    char* buffer = (char*)malloc(buffer_size);
    if (!buffer) {
        return NULL;
    }
    
    size_t pos = 0;
    serialize_term_recursive(t, buffer, buffer_size, &pos);
    
    kos_serialized* result = (kos_serialized*)malloc(sizeof(kos_serialized));
    if (!result) {
        free(buffer);
        return NULL;
    }
    
    result->data = buffer;
    result->length = pos;
    
    return result;
}

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

// 递归反序列化 term
static kos_term* deserialize_term_recursive(const char* json_str) {
    if (!json_str) {
        return NULL;
    }
    
    json_str = skip_whitespace(json_str);
    if (*json_str != '{') {
        return NULL;
    }
    
    // 提取整个JSON对象
    char* obj_str = NULL;
    const char* next = extract_json_object(json_str, &obj_str);
    if (!obj_str) {
        return NULL;
    }
    
    // 解析kind
    const char* kind_value = find_json_value(obj_str, "kind");
    if (!kind_value) {
        free(obj_str);
        return NULL;
    }
    
    char* kind_str = NULL;
    const char* after_kind = parse_json_string(kind_value, &kind_str);
    if (!kind_str) {
        free(obj_str);
        return NULL;
    }
    
    // 确定term_kind
    term_kind kind;
    if (strcmp(kind_str, "VAL") == 0) {
        kind = KOS_VAL;
    } else if (strcmp(kind_str, "TIME") == 0) {
        kind = KOS_TIME;
    } else if (strcmp(kind_str, "ID") == 0) {
        kind = KOS_ID;
    } else if (strcmp(kind_str, "PROP") == 0) {
        kind = KOS_PROP;
    } else if (strcmp(kind_str, "SIGMA") == 0) {
        kind = KOS_SIGMA;
    } else if (strcmp(kind_str, "PI") == 0) {
        kind = KOS_PI;
    } else if (strcmp(kind_str, "SUM") == 0) {
        kind = KOS_SUM;
    } else if (strcmp(kind_str, "PAIR") == 0) {
        kind = KOS_PAIR;
    } else if (strcmp(kind_str, "U") == 0) {
        kind = KOS_U;
    } else if (strcmp(kind_str, "TYPE") == 0) {
        kind = KOS_TYPE;
    } else {
        free(kind_str);
        free(obj_str);
        return NULL;
    }
    
    free(kind_str);
    
    // 创建term
    kos_term* term = (kos_term*)calloc(1, sizeof(kos_term));
    if (!term) {
        free(obj_str);
        return NULL;
    }
    
    term->kind = kind;
    
    // 根据类型解析数据
    switch (kind) {
        case KOS_VAL:
        case KOS_TIME:
        case KOS_ID:
        case KOS_PROP: {
            const char* val_value = find_json_value(obj_str, "val");
            if (val_value) {
                char* val_str = NULL;
                parse_json_string(val_value, &val_str);
                if (val_str) {
                    term->data.atomic.val = val_str;
                }
            }
            const char* type_value = find_json_value(obj_str, "type");
            if (type_value && parse_json_null(type_value) == NULL) {
                term->data.atomic.type = deserialize_term_recursive(type_value);
            }
            break;
        }
        
        case KOS_PAIR: {
            const char* data_value = find_json_value(obj_str, "data");
            if (data_value) {
                term->data.pair.data = deserialize_term_recursive(data_value);
            }
            const char* proof_value = find_json_value(obj_str, "proof");
            if (proof_value) {
                term->data.pair.proof = deserialize_term_recursive(proof_value);
            }
            break;
        }
        
        case KOS_SIGMA: {
            const char* domain_value = find_json_value(obj_str, "domain");
            if (domain_value) {
                term->data.sigma.domain = deserialize_term_recursive(domain_value);
            }
            const char* body_value = find_json_value(obj_str, "body");
            if (body_value) {
                term->data.sigma.body = deserialize_term_recursive(body_value);
            }
            break;
        }
        
        case KOS_PI: {
            const char* domain_value = find_json_value(obj_str, "domain");
            if (domain_value) {
                term->data.pi.domain = deserialize_term_recursive(domain_value);
            }
            const char* body_value = find_json_value(obj_str, "body");
            if (body_value && parse_json_null(body_value) == NULL) {
                term->data.pi.body = deserialize_term_recursive(body_value);
            }
            const char* body_term_value = find_json_value(obj_str, "body_term");
            if (body_term_value && parse_json_null(body_term_value) == NULL) {
                term->data.pi.body_term = deserialize_term_recursive(body_term_value);
            }
            break;
        }
        
        case KOS_SUM: {
            const char* left_type_value = find_json_value(obj_str, "left_type");
            if (left_type_value) {
                term->data.sum.left_type = deserialize_term_recursive(left_type_value);
            }
            const char* right_type_value = find_json_value(obj_str, "right_type");
            if (right_type_value) {
                term->data.sum.right_type = deserialize_term_recursive(right_type_value);
            }
            const char* is_left_value = find_json_value(obj_str, "is_left");
            if (is_left_value) {
                bool is_left = false;
                parse_json_bool(is_left_value, &is_left);
                term->data.sum.is_left = is_left;
            }
            const char* value_value = find_json_value(obj_str, "value");
            if (value_value && parse_json_null(value_value) == NULL) {
                term->data.sum.value = deserialize_term_recursive(value_value);
            }
            break;
        }
        
        case KOS_U:
        case KOS_TYPE: {
            const char* axis_value = find_json_value(obj_str, "axis");
            if (axis_value) {
                int axis = 0;
                parse_json_number(axis_value, &axis);
                term->data.universe.axis = (universe_axis)axis;
            }
            const char* level_value = find_json_value(obj_str, "level");
            if (level_value) {
                int level = 0;
                parse_json_number(level_value, &level);
                term->data.universe.level = level;
            }
            break;
        }
    }
    
    // 设置Universe信息（从类型推断或从数据中获取）
    if (kind == KOS_U || kind == KOS_TYPE) {
        term->universe.axis = term->data.universe.axis;
        term->universe.level = term->data.universe.level;
    } else {
        // 使用默认Universe信息，实际应该从类型推断
        term->universe.axis = UNIVERSE_COMPUTATIONAL;
        term->universe.level = 0;
    }
    
    free(obj_str);
    return term;
}

// 从 JSON 格式反序列化 term
kos_term* kos_term_deserialize(const char* json_str) {
    if (!json_str) {
        return NULL;
    }
    
    return deserialize_term_recursive(json_str);
}

// 释放序列化数据
void kos_serialized_free(kos_serialized* s) {
    if (s) {
        if (s->data) {
            free(s->data);
        }
        free(s);
    }
}

// 存储 term 到文件
int kos_term_save_to_file(kos_term* t, const char* filename) {
    if (!t || !filename) {
        return -1;
    }
    
    kos_serialized* serialized = kos_term_serialize(t);
    if (!serialized) {
        return -1;
    }
    
    FILE* file = fopen(filename, "w");
    if (!file) {
        kos_serialized_free(serialized);
        return -1;
    }
    
    size_t written = fwrite(serialized->data, 1, serialized->length, file);
    fclose(file);
    
    kos_serialized_free(serialized);
    
    return (written == serialized->length) ? 0 : -1;
}

// 从文件加载 term
kos_term* kos_term_load_from_file(const char* filename) {
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
    kos_term* term = kos_term_deserialize(buffer);
    free(buffer);
    
    return term;
}

// 存储知识集 K 到文件
int kos_knowledge_save(kos_term* K, const char* filename) {
    return kos_term_save_to_file(K, filename);
}

// 从文件加载知识集 K
kos_term* kos_knowledge_load(const char* filename) {
    return kos_term_load_from_file(filename);
}





