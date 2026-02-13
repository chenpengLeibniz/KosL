// src/performance/bplus_tree.c
// Phase 7: B+ Tree Index Implementation
// B+树索引：用于高效的范围查询和排序操作

#include "../../include/kos_performance.h"
#include "../../include/kos_query.h"
#include "../../include/kos_core.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

// B+树节点结构
typedef struct bplus_node {
    bool is_leaf;                   // 是否为叶子节点
    size_t key_count;               // 键的数量
    kos_term** keys;                // 键数组
    union {
        struct bplus_node** children; // 内部节点：子节点指针数组
        kos_term** values;            // 叶子节点：值数组
    };
    struct bplus_node* next;        // 叶子节点：下一个叶子节点（用于范围扫描）
    struct bplus_node* parent;      // 父节点
} bplus_node_t;

// B+树索引结构
struct kos_bplus_tree_index {
    bplus_node_t* root;             // 根节点
    size_t order;                   // B+树的阶（每个节点最多order-1个键）
    const char* field_name;         // 索引字段名
    size_t entry_count;             // 索引项数量
};

// 创建B+树节点
static bplus_node_t* bplus_node_create(bool is_leaf, size_t order) {
    bplus_node_t* node = (bplus_node_t*)calloc(1, sizeof(bplus_node_t));
    if (!node) {
        return NULL;
    }
    
    node->is_leaf = is_leaf;
    node->keys = (kos_term**)calloc(order, sizeof(kos_term*));
    
    if (is_leaf) {
        node->values = (kos_term**)calloc(order, sizeof(kos_term*));
    } else {
        node->children = (bplus_node_t**)calloc(order + 1, sizeof(bplus_node_t*));
    }
    
    if (!node->keys || (!is_leaf && !node->children) || (is_leaf && !node->values)) {
        if (node->keys) free(node->keys);
        if (node->values) free(node->values);
        if (node->children) free(node->children);
        free(node);
        return NULL;
    }
    
    return node;
}

// 释放B+树节点
static void bplus_node_free(bplus_node_t* node) {
    if (!node) {
        return;
    }
    
    if (node->keys) {
        for (size_t i = 0; i < node->key_count; i++) {
            if (node->keys[i]) {
                kos_term_free(node->keys[i]);
            }
        }
        free(node->keys);
    }
    
    if (node->is_leaf) {
        if (node->values) {
            for (size_t i = 0; i < node->key_count; i++) {
                if (node->values[i]) {
                    kos_term_free(node->values[i]);
                }
            }
            free(node->values);
        }
    } else {
        if (node->children) {
            for (size_t i = 0; i <= node->key_count; i++) {
                if (node->children[i]) {
                    bplus_node_free(node->children[i]);
                }
            }
            free(node->children);
        }
    }
    
    free(node);
}

// 比较两个kos_term（简化版本，假设是原子值）
static int compare_terms(kos_term* a, kos_term* b) {
    if (!a || !b) {
        return 0;
    }
    
    if (a->kind == KOS_VAL && b->kind == KOS_VAL) {
        if (a->data.atomic.val && b->data.atomic.val) {
            return strcmp(a->data.atomic.val, b->data.atomic.val);
        }
    }
    
    // 其他类型的比较可以扩展
    return 0;
}

// 在叶子节点中查找插入位置
static size_t find_insert_position(bplus_node_t* node, kos_term* key) {
    size_t pos = 0;
    while (pos < node->key_count && compare_terms(node->keys[pos], key) < 0) {
        pos++;
    }
    return pos;
}

// 分裂节点
static bplus_node_t* split_node(bplus_node_t* node, size_t order) {
    size_t mid = order / 2;
    bplus_node_t* new_node = bplus_node_create(node->is_leaf, order);
    if (!new_node) {
        return NULL;
    }
    
    new_node->parent = node->parent;
    
    // 移动后半部分到新节点
    for (size_t i = mid; i < node->key_count; i++) {
        new_node->keys[new_node->key_count] = node->keys[i];
        node->keys[i] = NULL;
        
        if (node->is_leaf) {
            new_node->values[new_node->key_count] = node->values[i];
            node->values[i] = NULL;
        } else {
            new_node->children[new_node->key_count] = node->children[i + 1];
            if (new_node->children[new_node->key_count]) {
                new_node->children[new_node->key_count]->parent = new_node;
            }
            node->children[i + 1] = NULL;
        }
        
        new_node->key_count++;
    }
    
    node->key_count = mid;
    
    // 连接叶子节点
    if (node->is_leaf) {
        new_node->next = node->next;
        node->next = new_node;
    }
    
    return new_node;
}

// 插入到B+树（简化实现）
static int bplus_tree_insert_internal(kos_bplus_tree_index_t* tree, kos_term* key, kos_term* value) {
    if (!tree || !key || !value) {
        return -1;
    }
    
    // 如果树为空，创建根节点
    if (!tree->root) {
        tree->root = bplus_node_create(true, tree->order);
        if (!tree->root) {
            return -1;
        }
    }
    
    // 简化实现：直接插入到叶子节点（实际B+树需要更复杂的逻辑）
    // 这里提供一个基本的实现框架
    
    bplus_node_t* leaf = tree->root;
    
    // 找到叶子节点（简化：假设根就是叶子）
    while (!leaf->is_leaf) {
        // 实际应该根据键值向下查找
        break; // 简化实现
    }
    
    // 检查是否需要分裂
    if (leaf->key_count >= tree->order - 1) {
        bplus_node_t* new_leaf = split_node(leaf, tree->order);
        if (!new_leaf) {
            return -1;
        }
        
        // 创建新的根节点（如果需要）
        if (!leaf->parent) {
            bplus_node_t* new_root = bplus_node_create(false, tree->order);
            if (!new_root) {
                return -1;
            }
            new_root->keys[0] = new_leaf->keys[0];
            new_root->key_count = 1;
            new_root->children[0] = leaf;
            new_root->children[1] = new_leaf;
            leaf->parent = new_root;
            new_leaf->parent = new_root;
            tree->root = new_root;
        }
        
        // 决定插入到哪个节点
        if (compare_terms(key, new_leaf->keys[0]) >= 0) {
            leaf = new_leaf;
        }
    }
    
    // 插入键值对
    size_t pos = find_insert_position(leaf, key);
    
    // 移动后续元素
    for (size_t i = leaf->key_count; i > pos; i--) {
        leaf->keys[i] = leaf->keys[i - 1];
        leaf->values[i] = leaf->values[i - 1];
    }
    
    leaf->keys[pos] = kos_term_copy(key);
    leaf->values[pos] = kos_term_copy(value);
    leaf->key_count++;
    tree->entry_count++;
    
    return 0;
}

// 创建B+树索引
kos_bplus_tree_index_t* kos_bplus_tree_create(const char* field_name, size_t order) {
    if (!field_name || order < 3) {
        return NULL;
    }
    
    kos_bplus_tree_index_t* tree = (kos_bplus_tree_index_t*)calloc(1, sizeof(kos_bplus_tree_index_t));
    if (!tree) {
        return NULL;
    }
    
    tree->field_name = strdup(field_name);
    tree->order = order;
    tree->root = NULL;
    tree->entry_count = 0;
    
    return tree;
}

// 插入数据到B+树
int kos_bplus_tree_insert(kos_bplus_tree_index_t* tree, kos_term* key, kos_term* value) {
    return bplus_tree_insert_internal(tree, key, value);
}

// 范围查询（简化实现）
kos_query_result_t* kos_bplus_tree_range_query(
    kos_bplus_tree_index_t* tree,
    kos_term* min_key,
    kos_term* max_key) {
    
    if (!tree || !tree->root) {
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
    
    // 简化实现：遍历所有叶子节点
    bplus_node_t* leaf = tree->root;
    while (!leaf->is_leaf && leaf->children[0]) {
        leaf = leaf->children[0];
    }
    
    // 遍历叶子节点链表
    while (leaf) {
        for (size_t i = 0; i < leaf->key_count; i++) {
            if (leaf->keys[i]) {
                bool in_range = true;
                
                if (min_key && compare_terms(leaf->keys[i], min_key) < 0) {
                    in_range = false;
                }
                if (max_key && compare_terms(leaf->keys[i], max_key) > 0) {
                    in_range = false;
                }
                
                if (in_range && leaf->values[i]) {
                    if (result->count >= result->capacity) {
                        result->capacity *= 2;
                        result->results = (kos_term**)realloc(
                            result->results, result->capacity * sizeof(kos_term*));
                        if (!result->results) {
                            kos_query_result_free(result);
                            return NULL;
                        }
                    }
                    result->results[result->count++] = kos_term_copy(leaf->values[i]);
                }
            }
        }
        
        leaf = leaf->next;
    }
    
    return result;
}

// 释放B+树索引
void kos_bplus_tree_free(kos_bplus_tree_index_t* tree) {
    if (!tree) {
        return;
    }
    
    if (tree->root) {
        bplus_node_free(tree->root);
    }
    
    if (tree->field_name) {
        free((void*)tree->field_name);
    }
    
    free(tree);
}
