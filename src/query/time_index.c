// src/query/time_index.c
// KOS Query Engine - Time Series Index
// 基于 B+树的时间序列索引，支持高效的时间范围查询

#include "../../include/kos_query.h"
#include "../../include/kos_core.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

// ========== B+树节点操作 ==========

// 创建时间索引节点
static time_index_node_t* time_index_node_create(const char* time_value, kos_term* data) {
    time_index_node_t* node = (time_index_node_t*)calloc(1, sizeof(time_index_node_t));
    if (!node) {
        return NULL;
    }
    
    node->time_value = strdup(time_value);
    node->data = data ? kos_term_copy(data) : NULL;
    node->height = 1;
    
    return node;
}

// 释放时间索引节点
static void time_index_node_free(time_index_node_t* node) {
    if (!node) {
        return;
    }
    
    if (node->time_value) {
        free(node->time_value);
    }
    
    if (node->data) {
        kos_term_free(node->data);
    }
    
    time_index_node_free(node->left);
    time_index_node_free(node->right);
    
    free(node);
}

// 获取节点高度
static int get_height(time_index_node_t* node) {
    return node ? node->height : 0;
}

// 更新节点高度
static void update_height(time_index_node_t* node) {
    if (node) {
        int left_height = get_height(node->left);
        int right_height = get_height(node->right);
        node->height = (left_height > right_height ? left_height : right_height) + 1;
    }
}

// 获取平衡因子
static int get_balance(time_index_node_t* node) {
    if (!node) {
        return 0;
    }
    return get_height(node->left) - get_height(node->right);
}

// 右旋转（AVL 树平衡操作）
static time_index_node_t* rotate_right(time_index_node_t* y) {
    time_index_node_t* x = y->left;
    time_index_node_t* T2 = x->right;
    
    x->right = y;
    y->left = T2;
    
    update_height(y);
    update_height(x);
    
    return x;
}

// 左旋转（AVL 树平衡操作）
static time_index_node_t* rotate_left(time_index_node_t* x) {
    time_index_node_t* y = x->right;
    time_index_node_t* T2 = y->left;
    
    y->left = x;
    x->right = T2;
    
    update_height(x);
    update_height(y);
    
    return y;
}

// 插入节点到 AVL 树
static time_index_node_t* time_index_insert_node(time_index_node_t* node,
                                                 const char* time_value,
                                                 kos_term* data) {
    if (!node) {
        return time_index_node_create(time_value, data);
    }
    
    int cmp = kos_compare_time(time_value, node->time_value);
    
    if (cmp < 0) {
        node->left = time_index_insert_node(node->left, time_value, data);
    } else if (cmp > 0) {
        node->right = time_index_insert_node(node->right, time_value, data);
    } else {
        // 时间值相同，更新数据
        if (node->data) {
            kos_term_free(node->data);
        }
        node->data = data ? kos_term_copy(data) : NULL;
        return node;
    }
    
    update_height(node);
    
    // 平衡树
    int balance = get_balance(node);
    
    // 左左情况
    if (balance > 1 && kos_compare_time(time_value, node->left->time_value) < 0) {
        return rotate_right(node);
    }
    
    // 右右情况
    if (balance < -1 && kos_compare_time(time_value, node->right->time_value) > 0) {
        return rotate_left(node);
    }
    
    // 左右情况
    if (balance > 1 && kos_compare_time(time_value, node->left->time_value) > 0) {
        node->left = rotate_left(node->left);
        return rotate_right(node);
    }
    
    // 右左情况
    if (balance < -1 && kos_compare_time(time_value, node->right->time_value) < 0) {
        node->right = rotate_right(node->right);
        return rotate_left(node);
    }
    
    return node;
}

// 范围查询（中序遍历）
static void time_index_query_range_recursive(time_index_node_t* node,
                                            const char* start_time,
                                            const char* end_time,
                                            kos_query_result_t* result) {
    if (!node) {
        return;
    }
    
    int cmp_start = kos_compare_time(node->time_value, start_time);
    int cmp_end = kos_compare_time(node->time_value, end_time);
    
    // 如果当前节点在范围内，递归左子树
    if (cmp_start > 0) {
        time_index_query_range_recursive(node->left, start_time, end_time, result);
    }
    
    // 如果当前节点在范围内，添加到结果
    if (cmp_start >= 0 && cmp_end <= 0 && node->data) {
        if (result->count >= result->capacity) {
            size_t new_capacity = result->capacity * 2;
            kos_term** new_results = (kos_term**)realloc(
                result->results,
                new_capacity * sizeof(kos_term*)
            );
            if (new_results) {
                result->results = new_results;
                result->capacity = new_capacity;
            } else {
                return; // 内存分配失败
            }
        }
        
        result->results[result->count++] = kos_term_copy(node->data);
    }
    
    // 如果当前节点小于结束时间，递归右子树
    if (cmp_end < 0) {
        time_index_query_range_recursive(node->right, start_time, end_time, result);
    }
}

// ========== 时间索引 API ==========

// 创建时间序列索引
kos_time_index_t* kos_time_index_create(const char* time_field) {
    kos_time_index_t* index = (kos_time_index_t*)calloc(1, sizeof(kos_time_index_t));
    if (!index) {
        return NULL;
    }
    
    index->time_field = strdup(time_field ? time_field : "t");
    index->root = NULL;
    index->count = 0;
    
    return index;
}

// 向索引插入数据
int kos_time_index_insert(kos_time_index_t* index, kos_term* data) {
    if (!index || !data) {
        return -1;
    }
    
    // 从数据中提取时间字段
    kos_term* time_field = kos_extract_field(data, index->time_field);
    if (!time_field) {
        return -1;
    }
    
    // 提取时间字符串
    char* time_str = NULL;
    if (time_field->kind == KOS_TIME && time_field->data.atomic.val) {
        time_str = strdup(time_field->data.atomic.val);
    } else if (time_field->kind == KOS_VAL && time_field->data.atomic.val) {
        time_str = strdup(time_field->data.atomic.val);
    }
    
    kos_term_free(time_field);
    
    if (!time_str) {
        return -1;
    }
    
    // 插入到树中
    index->root = time_index_insert_node(index->root, time_str, data);
    index->count++;
    
    free(time_str);
    
    return 0;
}

// 从索引删除数据
int kos_time_index_delete(kos_time_index_t* index, const char* time_value) {
    // TODO: 实现删除操作
    (void)index;
    (void)time_value;
    return -1;
}

// 时间范围查询
kos_query_result_t* kos_time_index_query_range(kos_time_index_t* index,
                                               const char* start_time,
                                               const char* end_time) {
    if (!index || !start_time || !end_time) {
        return NULL;
    }
    
    kos_query_result_t* result = (kos_query_result_t*)calloc(1, sizeof(kos_query_result_t));
    if (!result) {
        return NULL;
    }
    
    result->capacity = 16;
    result->results = (kos_term**)malloc(result->capacity * sizeof(kos_term*));
    if (!result->results) {
        free(result);
        return NULL;
    }
    
    result->count = 0;
    
    // 执行范围查询
    time_index_query_range_recursive(index->root, start_time, end_time, result);
    
    return result;
}

// 释放时间索引
void kos_time_index_free(kos_time_index_t* index) {
    if (!index) {
        return;
    }
    
    if (index->time_field) {
        free(index->time_field);
    }
    
    time_index_node_free(index->root);
    
    free(index);
}

// ========== 时间范围查询 API（基于知识集 K） ==========

// 执行时间范围查询（基于知识集 K）
kos_query_result_t* kos_query_time_range(kos_time_index_t* index,
                                        const char* start_time,
                                        const char* end_time) {
    return kos_time_index_query_range(index, start_time, end_time);
}
