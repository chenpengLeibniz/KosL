/**
 * @file storage.c
 * @brief KOS Core 层存储与加载（序列化/反序列化 kos_term）
 *
 * 提供 kos_term 与 JSON 的双向转换及文件持久化：
 * - kos_term_serialize/deserialize: JSON 序列化/反序列化
 * - kos_term_save_to_file/load_from_file: 文件读写
 * - kos_knowledge_save/load: 知识集 K 的持久化
 *
 * JSON 格式与 kos-core json-term 输出兼容，支持 VAL/TIME/ID/PROP/PAIR/SIGMA/PI/SUM/U/TYPE/ID_TYPE/REFL/LET。
 */

#include "kos_core.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

/** 将 str 转义为 JSON 字符串格式写入 buffer（含外围引号）。 */
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

/** 递归序列化 term 为 JSON 对象，写入 buffer，*pos 为当前写入位置。 */
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
        case KOS_ID_TYPE: kind_str = "ID_TYPE"; break;
        case KOS_SIGMA: kind_str = "SIGMA"; break;
        case KOS_SPLIT: kind_str = "SPLIT"; break;
        case KOS_PAIR: kind_str = "PAIR"; break;
        case KOS_PROP: kind_str = "PROP"; break;
        case KOS_PI: kind_str = "PI"; break;
        case KOS_SUM: kind_str = "SUM"; break;
        case KOS_U: kind_str = "U"; break;
        case KOS_TYPE: kind_str = "TYPE"; break;
        case KOS_GT: kind_str = "GT"; break;
        case KOS_GE: kind_str = "GE"; break;
        case KOS_LT: kind_str = "LT"; break;
        case KOS_LE: kind_str = "LE"; break;
        case KOS_EQ: kind_str = "EQ"; break;
        case KOS_APP: kind_str = "APP"; break;
        case KOS_LAM: kind_str = "LAM"; break;
        case KOS_CASE: kind_str = "CASE"; break;
        case KOS_REFL: kind_str = "REFL"; break;
        case KOS_LET: kind_str = "LET"; break;
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
        case KOS_ID_TYPE:
            snprintf(buffer + *pos, buffer_size - *pos, ",\"type\":");
            *pos += strlen(buffer + *pos);
            serialize_term_recursive(t->data.id_type.type, buffer, buffer_size, pos);
            snprintf(buffer + *pos, buffer_size - *pos, ",\"left\":");
            *pos += strlen(buffer + *pos);
            serialize_term_recursive(t->data.id_type.left, buffer, buffer_size, pos);
            snprintf(buffer + *pos, buffer_size - *pos, ",\"right\":");
            *pos += strlen(buffer + *pos);
            serialize_term_recursive(t->data.id_type.right, buffer, buffer_size, pos);
            break;
            
        case KOS_SIGMA:
            if (t->data.sigma.var_name) {
                char escaped[256];
                json_escape_string(t->data.sigma.var_name, escaped, sizeof(escaped));
                snprintf(buffer + *pos, buffer_size - *pos, ",\"var\":%s", escaped);
                *pos += strlen(buffer + *pos);
            }
            snprintf(buffer + *pos, buffer_size - *pos, ",\"domain\":");
            *pos += strlen(buffer + *pos);
            serialize_term_recursive(t->data.sigma.domain, buffer, buffer_size, pos);
            
            snprintf(buffer + *pos, buffer_size - *pos, ",\"body\":");
            *pos += strlen(buffer + *pos);
            serialize_term_recursive(t->data.sigma.body, buffer, buffer_size, pos);
            break;
            
        case KOS_PI:
            if (t->data.pi.var_name) {
                char escaped[256];
                json_escape_string(t->data.pi.var_name, escaped, sizeof(escaped));
                snprintf(buffer + *pos, buffer_size - *pos, ",\"var\":%s", escaped);
                *pos += strlen(buffer + *pos);
            }
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
        case KOS_GT:
        case KOS_GE:
        case KOS_LT:
        case KOS_LE:
        case KOS_EQ:
            snprintf(buffer + *pos, buffer_size - *pos, ",\"left\":");
            *pos += strlen(buffer + *pos);
            serialize_term_recursive(t->data.pred.left, buffer, buffer_size, pos);
            snprintf(buffer + *pos, buffer_size - *pos, ",\"right\":");
            *pos += strlen(buffer + *pos);
            serialize_term_recursive(t->data.pred.right, buffer, buffer_size, pos);
            break;
            
        case KOS_APP:
            snprintf(buffer + *pos, buffer_size - *pos, ",\"func\":");
            *pos += strlen(buffer + *pos);
            serialize_term_recursive(t->data.app.func, buffer, buffer_size, pos);
            snprintf(buffer + *pos, buffer_size - *pos, ",\"arg\":");
            *pos += strlen(buffer + *pos);
            serialize_term_recursive(t->data.app.arg, buffer, buffer_size, pos);
            break;
            
        case KOS_LAM:
            if (t->data.lam.var_name) {
                char escaped[1024];
                json_escape_string(t->data.lam.var_name, escaped, sizeof(escaped));
                snprintf(buffer + *pos, buffer_size - *pos, ",\"var\":%s", escaped);
                *pos += strlen(buffer + *pos);
            }
            snprintf(buffer + *pos, buffer_size - *pos, ",\"type\":");
            *pos += strlen(buffer + *pos);
            serialize_term_recursive(t->data.lam.type, buffer, buffer_size, pos);
            snprintf(buffer + *pos, buffer_size - *pos, ",\"body\":");
            *pos += strlen(buffer + *pos);
            serialize_term_recursive(t->data.lam.body, buffer, buffer_size, pos);
            break;
        case KOS_SPLIT:
            if (t->data.split.var1) {
                char escaped1[256];
                json_escape_string(t->data.split.var1, escaped1, sizeof(escaped1));
                snprintf(buffer + *pos, buffer_size - *pos, ",\"var1\":%s", escaped1);
                *pos += strlen(buffer + *pos);
            }
            if (t->data.split.var2) {
                char escaped2[256];
                json_escape_string(t->data.split.var2, escaped2, sizeof(escaped2));
                snprintf(buffer + *pos, buffer_size - *pos, ",\"var2\":%s", escaped2);
                *pos += strlen(buffer + *pos);
            }
            snprintf(buffer + *pos, buffer_size - *pos, ",\"pair\":");
            *pos += strlen(buffer + *pos);
            serialize_term_recursive(t->data.split.pair, buffer, buffer_size, pos);
            snprintf(buffer + *pos, buffer_size - *pos, ",\"body\":");
            *pos += strlen(buffer + *pos);
            serialize_term_recursive(t->data.split.body, buffer, buffer_size, pos);
            break;
        case KOS_CASE:
            if (t->data.case_term.left_var) {
                char escapedL[256];
                json_escape_string(t->data.case_term.left_var, escapedL, sizeof(escapedL));
                snprintf(buffer + *pos, buffer_size - *pos, ",\"left_var\":%s", escapedL);
                *pos += strlen(buffer + *pos);
            }
            if (t->data.case_term.right_var) {
                char escapedR[256];
                json_escape_string(t->data.case_term.right_var, escapedR, sizeof(escapedR));
                snprintf(buffer + *pos, buffer_size - *pos, ",\"right_var\":%s", escapedR);
                *pos += strlen(buffer + *pos);
            }
            snprintf(buffer + *pos, buffer_size - *pos, ",\"sum\":");
            *pos += strlen(buffer + *pos);
            serialize_term_recursive(t->data.case_term.sum, buffer, buffer_size, pos);
            snprintf(buffer + *pos, buffer_size - *pos, ",\"left_body\":");
            *pos += strlen(buffer + *pos);
            serialize_term_recursive(t->data.case_term.left_body, buffer, buffer_size, pos);
            snprintf(buffer + *pos, buffer_size - *pos, ",\"right_body\":");
            *pos += strlen(buffer + *pos);
            serialize_term_recursive(t->data.case_term.right_body, buffer, buffer_size, pos);
            break;
        case KOS_REFL:
            snprintf(buffer + *pos, buffer_size - *pos, ",\"value\":");
            *pos += strlen(buffer + *pos);
            serialize_term_recursive(t->data.refl.value, buffer, buffer_size, pos);
            break;
        case KOS_LET:
            if (t->data.let_term.var_name) {
                char escaped[1024];
                json_escape_string(t->data.let_term.var_name, escaped, sizeof(escaped));
                snprintf(buffer + *pos, buffer_size - *pos, ",\"var\":%s", escaped);
                *pos += strlen(buffer + *pos);
            }
            snprintf(buffer + *pos, buffer_size - *pos, ",\"type\":");
            *pos += strlen(buffer + *pos);
            serialize_term_recursive(t->data.let_term.type, buffer, buffer_size, pos);
            snprintf(buffer + *pos, buffer_size - *pos, ",\"value\":");
            *pos += strlen(buffer + *pos);
            serialize_term_recursive(t->data.let_term.value, buffer, buffer_size, pos);
            snprintf(buffer + *pos, buffer_size - *pos, ",\"body\":");
            *pos += strlen(buffer + *pos);
            serialize_term_recursive(t->data.let_term.body, buffer, buffer_size, pos);
            break;
    }
    
    buffer[*pos] = '}';
    (*pos)++;
    buffer[*pos] = '\0';
}

/** 将 term 序列化为 JSON 字符串，返回 kos_serialized*。调用者负责 kos_serialized_free。 */
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
    } else if (strcmp(kind_str, "ID_TYPE") == 0) {
        kind = KOS_ID_TYPE;
    } else if (strcmp(kind_str, "SIGMA") == 0) {
        kind = KOS_SIGMA;
    } else if (strcmp(kind_str, "SPLIT") == 0) {
        kind = KOS_SPLIT;
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
    } else if (strcmp(kind_str, "GT") == 0) {
        kind = KOS_GT;
    } else if (strcmp(kind_str, "GE") == 0) {
        kind = KOS_GE;
    } else if (strcmp(kind_str, "LT") == 0) {
        kind = KOS_LT;
    } else if (strcmp(kind_str, "LE") == 0) {
        kind = KOS_LE;
    } else if (strcmp(kind_str, "EQ") == 0) {
        kind = KOS_EQ;
    } else if (strcmp(kind_str, "APP") == 0) {
        kind = KOS_APP;
    } else if (strcmp(kind_str, "LAM") == 0) {
        kind = KOS_LAM;
    } else if (strcmp(kind_str, "CASE") == 0) {
        kind = KOS_CASE;
    } else if (strcmp(kind_str, "REFL") == 0) {
        kind = KOS_REFL;
    } else if (strcmp(kind_str, "LET") == 0) {
        kind = KOS_LET;
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
        case KOS_ID_TYPE: {
            const char* type_value = find_json_value(obj_str, "type");
            if (type_value) {
                term->data.id_type.type = deserialize_term_recursive(type_value);
            }
            const char* left_value = find_json_value(obj_str, "left");
            if (left_value) {
                term->data.id_type.left = deserialize_term_recursive(left_value);
            }
            const char* right_value = find_json_value(obj_str, "right");
            if (right_value) {
                term->data.id_type.right = deserialize_term_recursive(right_value);
            }
            break;
        }
        
        case KOS_SIGMA: {
            const char* var_value = find_json_value(obj_str, "var");
            if (var_value) {
                char* var_str = NULL;
                parse_json_string(var_value, &var_str);
                if (var_str) term->data.sigma.var_name = var_str;
            }
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
            const char* var_value = find_json_value(obj_str, "var");
            if (var_value) {
                char* var_str = NULL;
                parse_json_string(var_value, &var_str);
                if (var_str) term->data.pi.var_name = var_str;
            }
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
        
        case KOS_GT:
        case KOS_GE:
        case KOS_LT:
        case KOS_LE:
        case KOS_EQ: {
            const char* left_value = find_json_value(obj_str, "left");
            if (left_value && parse_json_null(left_value) == NULL) {
                term->data.pred.left = deserialize_term_recursive(left_value);
            }
            const char* right_value = find_json_value(obj_str, "right");
            if (right_value && parse_json_null(right_value) == NULL) {
                term->data.pred.right = deserialize_term_recursive(right_value);
            }
            break;
        }
        
        case KOS_APP: {
            const char* func_value = find_json_value(obj_str, "func");
            if (func_value) {
                term->data.app.func = deserialize_term_recursive(func_value);
            }
            const char* arg_value = find_json_value(obj_str, "arg");
            if (arg_value) {
                term->data.app.arg = deserialize_term_recursive(arg_value);
            }
            break;
        }
        
        case KOS_LAM: {
            const char* var_value = find_json_value(obj_str, "var");
            if (var_value) {
                char* var_str = NULL;
                parse_json_string(var_value, &var_str);
                if (var_str) term->data.lam.var_name = var_str;
            }
            const char* type_value = find_json_value(obj_str, "type");
            if (type_value) {
                term->data.lam.type = deserialize_term_recursive(type_value);
            }
            const char* body_value = find_json_value(obj_str, "body");
            if (body_value) {
                term->data.lam.body = deserialize_term_recursive(body_value);
            }
            break;
        }
        case KOS_SPLIT: {
            const char* var1_value = find_json_value(obj_str, "var1");
            if (var1_value) {
                char* var1_str = NULL;
                parse_json_string(var1_value, &var1_str);
                if (var1_str) term->data.split.var1 = var1_str;
            }
            const char* var2_value = find_json_value(obj_str, "var2");
            if (var2_value) {
                char* var2_str = NULL;
                parse_json_string(var2_value, &var2_str);
                if (var2_str) term->data.split.var2 = var2_str;
            }
            const char* pair_value = find_json_value(obj_str, "pair");
            if (pair_value) {
                term->data.split.pair = deserialize_term_recursive(pair_value);
            }
            const char* body_value = find_json_value(obj_str, "body");
            if (body_value) {
                term->data.split.body = deserialize_term_recursive(body_value);
            }
            break;
        }
        case KOS_CASE: {
            const char* left_var_value = find_json_value(obj_str, "left_var");
            if (left_var_value) {
                char* lv = NULL;
                parse_json_string(left_var_value, &lv);
                if (lv) term->data.case_term.left_var = lv;
            }
            const char* right_var_value = find_json_value(obj_str, "right_var");
            if (right_var_value) {
                char* rv = NULL;
                parse_json_string(right_var_value, &rv);
                if (rv) term->data.case_term.right_var = rv;
            }
            const char* sum_value = find_json_value(obj_str, "sum");
            if (sum_value) {
                term->data.case_term.sum = deserialize_term_recursive(sum_value);
            }
            const char* left_body_value = find_json_value(obj_str, "left_body");
            if (left_body_value) {
                term->data.case_term.left_body = deserialize_term_recursive(left_body_value);
            }
            const char* right_body_value = find_json_value(obj_str, "right_body");
            if (right_body_value) {
                term->data.case_term.right_body = deserialize_term_recursive(right_body_value);
            }
            break;
        }
        case KOS_REFL: {
            const char* value_value = find_json_value(obj_str, "value");
            if (value_value) {
                term->data.refl.value = deserialize_term_recursive(value_value);
            }
            break;
        }
        case KOS_LET: {
            const char* var_value = find_json_value(obj_str, "var");
            if (var_value) {
                char* var_str = NULL;
                parse_json_string(var_value, &var_str);
                if (var_str) term->data.let_term.var_name = var_str;
            }
            const char* type_value = find_json_value(obj_str, "type");
            if (type_value) {
                term->data.let_term.type = deserialize_term_recursive(type_value);
            }
            const char* value_value = find_json_value(obj_str, "value");
            if (value_value) {
                term->data.let_term.value = deserialize_term_recursive(value_value);
            }
            const char* body_value = find_json_value(obj_str, "body");
            if (body_value) {
                term->data.let_term.body = deserialize_term_recursive(body_value);
            }
            break;
        }
    }
    
    // 设置Universe信息（从类型推断或从数据中获取）
    if (kind == KOS_U || kind == KOS_TYPE) {
        term->universe.axis = term->data.universe.axis;
        term->universe.level = term->data.universe.level;
    } else if (kind == KOS_GT || kind == KOS_GE || kind == KOS_LT || kind == KOS_LE || kind == KOS_EQ ||
               kind == KOS_ID_TYPE || kind == KOS_REFL) {
        term->universe.axis = UNIVERSE_LOGICAL;
        term->universe.level = 1;  /* Prop : Type_1 */
    } else {
        // 使用默认Universe信息，实际应该从类型推断
        term->universe.axis = UNIVERSE_COMPUTATIONAL;
        term->universe.level = 0;
    }
    
    free(obj_str);
    return term;
}

/** 从 JSON 字符串反序列化为 kos_term*。调用者负责 kos_term_free。 */
kos_term* kos_term_deserialize(const char* json_str) {
    if (!json_str) {
        return NULL;
    }
    
    return deserialize_term_recursive(json_str);
}

/** 释放 kos_serialized 及其内部 data。 */
void kos_serialized_free(kos_serialized* s) {
    if (s) {
        if (s->data) {
            free(s->data);
        }
        free(s);
    }
}

/** 将 term 序列化并写入文件。成功返回 0，失败返回 -1。 */
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

/** 从文件读取并反序列化为 kos_term*。调用者负责 kos_term_free。 */
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

/** 存储知识集 K 到文件。委托给 kos_term_save_to_file。 */
int kos_knowledge_save(kos_term* K, const char* filename) {
    return kos_term_save_to_file(K, filename);
}

/** 从文件加载知识集 K。委托给 kos_term_load_from_file。 */
kos_term* kos_knowledge_load(const char* filename) {
    return kos_term_load_from_file(filename);
}





