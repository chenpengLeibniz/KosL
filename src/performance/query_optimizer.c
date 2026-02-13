// src/performance/query_optimizer.c
// Phase 7: Query Optimizer - Automatic Index Selection
// 查询优化器：自动选择最优索引，生成查询计划

#include "../../include/kos_performance.h"
#include "../../include/kos_query.h"
#include "../../include/kos_core.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <float.h>
#include <math.h>

// 创建查询计划节点
static kos_plan_node_t* plan_node_create(kos_plan_node_type_t type) {
    kos_plan_node_t* node = (kos_plan_node_t*)calloc(1, sizeof(kos_plan_node_t));
    if (!node) {
        return NULL;
    }
    
    node->type = type;
    node->estimated_cost = 0.0;
    node->estimated_rows = 0;
    node->children = NULL;
    node->child_count = 0;
    
    return node;
}

// 释放查询计划节点
static void plan_node_free(kos_plan_node_t* node) {
    if (!node) {
        return;
    }
    
    if (node->children) {
        for (size_t i = 0; i < node->child_count; i++) {
            plan_node_free(node->children[i]);
        }
        free(node->children);
    }
    
    if (node->index_name) {
        free((void*)node->index_name);
    }
    
    free(node);
}

// 估算全表扫描代价
static double estimate_scan_cost(size_t total_rows) {
    // 全表扫描代价 = 行数 * 单行扫描代价
    return total_rows * 1.0;
}

// 估算索引扫描代价
static double estimate_index_cost(kos_index_metadata_t* index, size_t estimated_rows) {
    if (!index) {
        return DBL_MAX;
    }
    
    // 索引扫描代价 = 索引查找代价 + 行数 * 单行访问代价
    double index_lookup_cost = 10.0; // 索引查找的固定代价
    double row_access_cost = 0.1;   // 单行访问代价
    
    return index_lookup_cost + estimated_rows * row_access_cost;
}

// 估算过滤后的行数（简化：假设选择性为10%）
static size_t estimate_filtered_rows(size_t total_rows, query_condition_t* conditions) {
    if (!conditions) {
        return total_rows;
    }
    
    // 简化：每个条件减少90%的行数
    size_t filtered = total_rows;
    query_condition_t* cond = conditions;
    while (cond) {
        filtered = filtered / 10; // 简化假设
        cond = cond->next;
    }
    
    return filtered > 0 ? filtered : 1;
}

// 查找可用于查询条件的索引
static kos_index_metadata_t* find_usable_index(
    kos_index_manager_t* index_manager,
    query_condition_t* conditions) {
    
    if (!index_manager || !conditions) {
        return NULL;
    }
    
    // 查找第一个条件的字段是否有索引
    query_condition_t* cond = conditions;
    while (cond) {
        if (cond->field_name) {
            kos_index_metadata_t* index = kos_index_manager_find_by_field(
                index_manager, cond->field_name);
            if (index) {
                return index;
            }
        }
        cond = cond->next;
    }
    
    return NULL;
}

// 创建查询计划
kos_query_plan_t* kos_query_optimizer_create_plan(
    kos_query_t* query,
    kos_index_manager_t* index_manager) {
    
    if (!query) {
        return NULL;
    }
    
    kos_query_plan_t* plan = (kos_query_plan_t*)calloc(1, sizeof(kos_query_plan_t));
    if (!plan) {
        return NULL;
    }
    
    // 假设总行数（实际应该从统计信息获取）
    size_t total_rows = 10000; // 示例值
    
    // 查找可用索引
    kos_index_metadata_t* usable_index = find_usable_index(index_manager, query->conditions);
    
    kos_plan_node_t* root = NULL;
    
    if (usable_index) {
        // 使用索引扫描
        root = plan_node_create(KOS_PLAN_INDEX_SCAN);
        root->index_name = strdup(usable_index->name);
        
        // 估算过滤后的行数
        size_t estimated_rows = estimate_filtered_rows(total_rows, query->conditions);
        root->estimated_rows = estimated_rows;
        root->estimated_cost = estimate_index_cost(usable_index, estimated_rows);
        
        // 如果有过滤条件，添加过滤节点
        if (query->conditions) {
            kos_plan_node_t* filter_node = plan_node_create(KOS_PLAN_FILTER);
            filter_node->estimated_rows = estimated_rows;
            filter_node->estimated_cost = estimated_rows * 0.1; // 过滤代价
            
            filter_node->children = (kos_plan_node_t**)calloc(1, sizeof(kos_plan_node_t*));
            filter_node->children[0] = root;
            filter_node->child_count = 1;
            
            root = filter_node;
        }
    } else {
        // 全表扫描
        root = plan_node_create(KOS_PLAN_SCAN);
        root->estimated_rows = total_rows;
        root->estimated_cost = estimate_scan_cost(total_rows);
        
        // 添加过滤节点
        if (query->conditions) {
            kos_plan_node_t* filter_node = plan_node_create(KOS_PLAN_FILTER);
            size_t estimated_rows = estimate_filtered_rows(total_rows, query->conditions);
            filter_node->estimated_rows = estimated_rows;
            filter_node->estimated_cost = estimated_rows * 0.1;
            
            filter_node->children = (kos_plan_node_t**)calloc(1, sizeof(kos_plan_node_t*));
            filter_node->children[0] = root;
            filter_node->child_count = 1;
            
            root = filter_node;
        }
    }
    
    // 添加排序节点（如果需要）
    if (query->order_by_field) {
        kos_plan_node_t* sort_node = plan_node_create(KOS_PLAN_SORT);
        sort_node->estimated_rows = root->estimated_rows;
        sort_node->estimated_cost = root->estimated_rows * (double)log2((double)root->estimated_rows) * 0.1; // 排序代价
        
        sort_node->children = (kos_plan_node_t**)calloc(1, sizeof(kos_plan_node_t*));
        sort_node->children[0] = root;
        sort_node->child_count = 1;
        
        root = sort_node;
    }
    
    // 添加聚合节点（如果需要）
    if (query->aggregation != AGG_NONE) {
        kos_plan_node_t* agg_node = plan_node_create(KOS_PLAN_AGGREGATE);
        agg_node->estimated_rows = 1; // 聚合结果通常只有一行
        agg_node->estimated_cost = root->estimated_rows * 0.05; // 聚合代价
        
        agg_node->children = (kos_plan_node_t**)calloc(1, sizeof(kos_plan_node_t*));
        agg_node->children[0] = root;
        agg_node->child_count = 1;
        
        root = agg_node;
    }
    
    plan->root = root;
    plan->total_cost = root->estimated_cost;
    plan->estimated_result_count = root->estimated_rows;
    
    return plan;
}

// 释放查询计划
void kos_query_plan_free(kos_query_plan_t* plan) {
    if (!plan) {
        return;
    }
    
    if (plan->root) {
        plan_node_free(plan->root);
    }
    
    free(plan);
}

// 执行查询计划（简化实现）
kos_query_result_t* kos_query_plan_execute(
    kos_query_plan_t* plan,
    const kos_term* knowledge_set,
    kos_index_manager_t* index_manager) {
    
    if (!plan || !plan->root) {
        return NULL;
    }
    
    // 简化实现：直接执行查询（实际应该根据计划节点类型执行）
    // 这里返回一个空结果作为占位符
    kos_query_result_t* result = (kos_query_result_t*)calloc(1, sizeof(kos_query_result_t));
    if (result) {
        result->capacity = 1024;
        result->results = (kos_term**)calloc(result->capacity, sizeof(kos_term*));
        result->count = 0;
    }
    
    return result;
}

// 打印查询计划（用于调试）
static void plan_node_print(kos_plan_node_t* node, int indent) {
    if (!node) {
        return;
    }
    
    for (int i = 0; i < indent; i++) {
        printf("  ");
    }
    
    const char* type_name = "UNKNOWN";
    switch (node->type) {
        case KOS_PLAN_SCAN: type_name = "SCAN"; break;
        case KOS_PLAN_INDEX_SCAN: type_name = "INDEX_SCAN"; break;
        case KOS_PLAN_INDEX_SEEK: type_name = "INDEX_SEEK"; break;
        case KOS_PLAN_FILTER: type_name = "FILTER"; break;
        case KOS_PLAN_SORT: type_name = "SORT"; break;
        case KOS_PLAN_JOIN: type_name = "JOIN"; break;
        case KOS_PLAN_AGGREGATE: type_name = "AGGREGATE"; break;
    }
    
    printf("%s", type_name);
    if (node->index_name) {
        printf(" [index: %s]", node->index_name);
    }
    printf(" (cost=%.2f, rows=%zu)\n", node->estimated_cost, node->estimated_rows);
    
    for (size_t i = 0; i < node->child_count; i++) {
        plan_node_print(node->children[i], indent + 1);
    }
}

void kos_query_plan_print(kos_query_plan_t* plan) {
    if (!plan) {
        printf("Query Plan: NULL\n");
        return;
    }
    
    printf("Query Plan (total_cost=%.2f, estimated_rows=%zu):\n",
           plan->total_cost, plan->estimated_result_count);
    plan_node_print(plan->root, 0);
}

// 生成查询键（从查询对象生成唯一键）
char* kos_query_generate_key(kos_query_t* query) {
    if (!query) {
        return NULL;
    }
    
    // 简化实现：基于查询的主要特征生成键
    char buffer[1024];
    snprintf(buffer, sizeof(buffer), "%s:%s:%zu",
             query->type_name ? query->type_name : "",
             query->order_by_field ? query->order_by_field : "",
             query->limit);
    
    return strdup(buffer);
}
