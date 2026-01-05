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

// 从 JSON 格式反序列化 term（简化实现）
// 注意：这是一个简化版本，完整的 JSON 解析需要更复杂的实现
kos_term* kos_term_deserialize(const char* json_str) {
    // TODO: 实现完整的 JSON 解析
    // 这里返回 NULL 作为占位符
    (void)json_str;
    return NULL;
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





