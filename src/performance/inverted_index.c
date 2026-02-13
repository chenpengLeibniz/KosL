// src/performance/inverted_index.c
// Phase 7: Inverted Index Implementation
// 倒排索引：用于全文搜索和精确匹配

#include "../../include/kos_performance.h"
#include "../../include/kos_query.h"
#include "../../include/kos_core.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

// 倒排列表项（文档ID）
typedef struct posting {
    kos_term* document_id;          // 文档ID
    size_t frequency;                // 词频（在该文档中出现次数）
    struct posting* next;            // 下一个文档
} posting_t;

// 倒排索引项（词条）
typedef struct term_entry {
    char* term;                      // 词条
    posting_t* postings;              // 倒排列表
    size_t document_count;           // 包含该词条的文档数
    struct term_entry* next;         // 哈希表链
} term_entry_t;

// 倒排索引结构
struct kos_inverted_index {
    term_entry_t** buckets;          // 哈希表桶数组
    size_t bucket_count;            // 桶数量
    const char* field_name;         // 索引字段名
    size_t term_count;               // 词条数量
    size_t document_count;          // 文档数量
};

#define DEFAULT_BUCKET_COUNT 1024

// 计算词条的哈希值
static size_t hash_term(const char* term, size_t bucket_count) {
    uint64_t hash = kos_hash_string(term);
    return hash % bucket_count;
}

// 创建倒排列表项
static posting_t* posting_create(kos_term* document_id) {
    posting_t* posting = (posting_t*)calloc(1, sizeof(posting_t));
    if (!posting) {
        return NULL;
    }
    
    posting->document_id = document_id ? kos_term_copy(document_id) : NULL;
    posting->frequency = 1;
    
    return posting;
}

// 释放倒排列表
static void posting_list_free(posting_t* head) {
    while (head) {
        posting_t* next = head->next;
        if (head->document_id) {
            kos_term_free(head->document_id);
        }
        free(head);
        head = next;
    }
}

// 创建词条项
static term_entry_t* term_entry_create(const char* term) {
    term_entry_t* entry = (term_entry_t*)calloc(1, sizeof(term_entry_t));
    if (!entry) {
        return NULL;
    }
    
    entry->term = strdup(term);
    entry->postings = NULL;
    entry->document_count = 0;
    
    return entry;
}

// 释放词条项
static void term_entry_free(term_entry_t* entry) {
    if (!entry) {
        return;
    }
    
    if (entry->term) {
        free(entry->term);
    }
    
    posting_list_free(entry->postings);
    free(entry);
}

// 简单的分词函数（按空格分割）
static void tokenize(const char* text, char*** tokens, size_t* token_count) {
    if (!text) {
        *tokens = NULL;
        *token_count = 0;
        return;
    }
    
    // 简化实现：按空格分割
    size_t capacity = 64;
    *tokens = (char**)calloc(capacity, sizeof(char*));
    *token_count = 0;
    
    const char* start = text;
    const char* p = text;
    
    while (*p) {
        if (*p == ' ' || *p == '\t' || *p == '\n') {
            if (p > start) {
                size_t len = p - start;
                if (*token_count >= capacity) {
                    capacity *= 2;
                    *tokens = (char**)realloc(*tokens, capacity * sizeof(char*));
                }
                (*tokens)[*token_count] = (char*)calloc(len + 1, sizeof(char));
                strncpy((*tokens)[*token_count], start, len);
                (*token_count)++;
            }
            start = p + 1;
        }
        p++;
    }
    
    // 最后一个词
    if (p > start) {
        size_t len = p - start;
        if (*token_count >= capacity) {
            capacity *= 2;
            *tokens = (char**)realloc(*tokens, capacity * sizeof(char*));
        }
        (*tokens)[*token_count] = (char*)calloc(len + 1, sizeof(char));
        strncpy((*tokens)[*token_count], start, len);
        (*token_count)++;
    }
}

// 创建倒排索引
kos_inverted_index_t* kos_inverted_index_create(const char* field_name) {
    if (!field_name) {
        return NULL;
    }
    
    kos_inverted_index_t* index = (kos_inverted_index_t*)calloc(1, sizeof(kos_inverted_index_t));
    if (!index) {
        return NULL;
    }
    
    index->bucket_count = DEFAULT_BUCKET_COUNT;
    index->buckets = (term_entry_t**)calloc(index->bucket_count, sizeof(term_entry_t*));
    if (!index->buckets) {
        free(index);
        return NULL;
    }
    
    index->field_name = strdup(field_name);
    index->term_count = 0;
    index->document_count = 0;
    
    return index;
}

// 插入文档到倒排索引
int kos_inverted_index_insert(
    kos_inverted_index_t* index,
    kos_term* document_id,
    const char* text_content) {
    
    if (!index || !document_id || !text_content) {
        return -1;
    }
    
    // 分词
    char** tokens = NULL;
    size_t token_count = 0;
    tokenize(text_content, &tokens, &token_count);
    
    // 为每个词条创建或更新倒排列表
    for (size_t i = 0; i < token_count; i++) {
        if (!tokens[i] || strlen(tokens[i]) == 0) {
            continue;
        }
        
        size_t bucket_idx = hash_term(tokens[i], index->bucket_count);
        term_entry_t* entry = index->buckets[bucket_idx];
        
        // 查找是否已存在该词条
        while (entry) {
            if (strcmp(entry->term, tokens[i]) == 0) {
                break;
            }
            entry = entry->next;
        }
        
        // 如果不存在，创建新词条项
        if (!entry) {
            entry = term_entry_create(tokens[i]);
            if (!entry) {
                continue;
            }
            entry->next = index->buckets[bucket_idx];
            index->buckets[bucket_idx] = entry;
            index->term_count++;
        }
        
        // 检查文档是否已在倒排列表中
        posting_t* posting = entry->postings;
        while (posting) {
            // 简化：假设document_id可以直接比较
            if (posting->document_id == document_id) {
                posting->frequency++;
                break;
            }
            posting = posting->next;
        }
        
        // 如果文档不在列表中，添加新项
        if (!posting) {
            posting_t* new_posting = posting_create(document_id);
            if (new_posting) {
                new_posting->next = entry->postings;
                entry->postings = new_posting;
                entry->document_count++;
            }
        }
    }
    
    // 释放分词结果
    if (tokens) {
        for (size_t i = 0; i < token_count; i++) {
            if (tokens[i]) {
                free(tokens[i]);
            }
        }
        free(tokens);
    }
    
    index->document_count++;
    
    return 0;
}

// 搜索（返回匹配的文档ID）
kos_query_result_t* kos_inverted_index_search(
    kos_inverted_index_t* index,
    const char* query_term) {
    
    if (!index || !query_term) {
        return NULL;
    }
    
    kos_query_result_t* result = (kos_query_result_t*)calloc(1, sizeof(kos_query_result_t));
    if (!result) {
        return NULL;
    }
    
    result->capacity = 1024;
    result->results = (kos_term**)calloc(result->capacity, sizeof(kos_term*));
    if (!result->results) {
        free(result);
        return NULL;
    }
    
    // 查找词条
    size_t bucket_idx = hash_term(query_term, index->bucket_count);
    term_entry_t* entry = index->buckets[bucket_idx];
    
    while (entry) {
        if (strcmp(entry->term, query_term) == 0) {
            // 收集所有文档ID
            posting_t* posting = entry->postings;
            while (posting) {
                if (result->count >= result->capacity) {
                    result->capacity *= 2;
                    result->results = (kos_term**)realloc(
                        result->results, result->capacity * sizeof(kos_term*));
                    if (!result->results) {
                        kos_query_result_free(result);
                        return NULL;
                    }
                }
                result->results[result->count++] = kos_term_copy(posting->document_id);
                posting = posting->next;
            }
            break;
        }
        entry = entry->next;
    }
    
    return result;
}

// 释放倒排索引
void kos_inverted_index_free(kos_inverted_index_t* index) {
    if (!index) {
        return;
    }
    
    if (index->buckets) {
        for (size_t i = 0; i < index->bucket_count; i++) {
            term_entry_t* entry = index->buckets[i];
            while (entry) {
                term_entry_t* next = entry->next;
                term_entry_free(entry);
                entry = next;
            }
        }
        free(index->buckets);
    }
    
    if (index->field_name) {
        free((void*)index->field_name);
    }
    
    free(index);
}
