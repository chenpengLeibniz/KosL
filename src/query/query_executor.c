// src/query/query_executor.c
// KOS Query Engine - Query Executor
// 基于知识集 K 执行查询

#include "../../include/kos_query.h"
#include "../../include/kos_core.h"
#include "../../include/kos_kernel.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

#define INITIAL_RESULT_CAPACITY 16

// ========== 辅助函数：从知识集 K 中提取实例 ==========

// 递归遍历知识集 K（Σ 链结构），提取指定类型的所有实例
static void extract_instances_recursive(const kos_term* K,
                                       const char* type_name,
                                       kos_query_result_t* result) {
    if (!K) {
        return;
    }
    
    // 如果 K 是 KOS_PAIR，递归处理
    if (K->kind == KOS_PAIR) {
        // 检查当前项是否是目标类型
        // 简化实现：检查项的结构是否符合类型定义
        // 实际实现中应该使用类型检查器验证
        
        // 先处理当前项
        if (K->data.pair.data) {
            // 这里简化处理：假设所有 KOS_PAIR 都是实例
            // 实际应该检查类型匹配
            if (result->count >= result->capacity) {
                // 扩容
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
            
            // 添加到结果
            result->results[result->count++] = kos_term_copy(K->data.pair.data);
        }
        
        // 递归处理知识集的其余部分
        extract_instances_recursive(K->data.pair.proof, type_name, result);
    }
}

// 从知识集 K 中提取指定类型的所有实例
kos_query_result_t* kos_extract_instances_by_type(const kos_term* K, const char* type_name) {
    kos_query_result_t* result = (kos_query_result_t*)calloc(1, sizeof(kos_query_result_t));
    if (!result) {
        return NULL;
    }
    
    result->capacity = INITIAL_RESULT_CAPACITY;
    result->results = (kos_term**)malloc(result->capacity * sizeof(kos_term*));
    if (!result->results) {
        free(result);
        return NULL;
    }
    
    result->count = 0;
    
    // 递归提取实例
    extract_instances_recursive(K, type_name, result);
    
    return result;
}

// ========== 辅助函数：从 Σ 类型实例中提取字段值 ==========

// 从嵌套的 Σ 类型实例中提取字段值
// field_path 格式：如 "batch" 或 "err" 或 "t"
kos_term* kos_extract_field(kos_term* instance, const char* field_path) {
    if (!instance || !field_path) {
        return NULL;
    }
    
    // 简化实现：假设 Σ 类型实例是嵌套的 KOS_PAIR
    // 实际实现中应该根据类型定义解析字段路径
    
    if (instance->kind != KOS_PAIR) {
        return NULL;
    }
    
    // 对于嵌套的 Σ 类型，第一个字段在 data.pair.data
    // 这里简化处理：只支持提取第一个字段
    if (strcmp(field_path, "first") == 0 || strcmp(field_path, "0") == 0) {
        return kos_term_copy(instance->data.pair.data);
    }
    
    // 递归提取嵌套字段
    if (instance->data.pair.proof && instance->data.pair.proof->kind == KOS_PAIR) {
        return kos_extract_field(instance->data.pair.proof, field_path);
    }
    
    return NULL;
}

// ========== 辅助函数：比较时间值 ==========

// 比较两个时间值（字符串格式：ISO 8601 或 Unix 时间戳）
int kos_compare_time(const char* time1, const char* time2) {
    if (!time1 || !time2) {
        return 0;
    }
    
    // 如果是 Unix 时间戳（纯数字）
    if (strspn(time1, "0123456789") == strlen(time1) &&
        strspn(time2, "0123456789") == strlen(time2)) {
        long long t1 = atoll(time1);
        long long t2 = atoll(time2);
        if (t1 < t2) return -1;
        if (t1 > t2) return 1;
        return 0;
    }
    
    // 否则使用字符串比较（ISO 8601 格式）
    return strcmp(time1, time2);
}

// ========== 查询条件评估 ==========

// 评估单个查询条件
static bool evaluate_condition(kos_term* instance,
                               query_condition_t* condition) {
    if (!instance || !condition) {
        return false;
    }
    
    // 提取字段值
    kos_term* field_value = kos_extract_field(instance, condition->field_name);
    if (!field_value) {
        return false;
    }
    
    bool result = false;
    
    // 根据操作符比较
    switch (condition->op) {
        case OP_EQ: {
            // 简化比较：比较字符串表示
            kos_serialized* field_serialized = kos_term_serialize(field_value);
            kos_serialized* value_serialized = kos_term_serialize(condition->value);
            result = (field_serialized && field_serialized->data && 
                     value_serialized && value_serialized->data &&
                     strcmp(field_serialized->data, value_serialized->data) == 0);
            if (field_serialized) kos_serialized_free(field_serialized);
            if (value_serialized) kos_serialized_free(value_serialized);
            break;
        }
        case OP_NE: {
            kos_serialized* field_serialized = kos_term_serialize(field_value);
            kos_serialized* value_serialized = kos_term_serialize(condition->value);
            result = (field_serialized && field_serialized->data && 
                     value_serialized && value_serialized->data &&
                     strcmp(field_serialized->data, value_serialized->data) != 0);
            if (field_serialized) kos_serialized_free(field_serialized);
            if (value_serialized) kos_serialized_free(value_serialized);
            break;
        }
        case OP_BETWEEN: {
            // BETWEEN 操作：需要两个值
            // 简化实现：假设 condition->value 是包含两个值的结构
            // 实际应该支持两个值的比较
            result = true; // 占位实现
            break;
        }
        default:
            result = false;
            break;
    }
    
    kos_term_free(field_value);
    return result;
}

// 评估所有查询条件（AND 连接）
static bool evaluate_conditions(kos_term* instance, query_condition_t* conditions) {
    query_condition_t* cond = conditions;
    while (cond) {
        if (!evaluate_condition(instance, cond)) {
            return false;
        }
        cond = cond->next;
    }
    return true;
}

// ========== 查询执行 ==========

// 执行查询（基于知识集 K）
kos_query_result_t* kos_query_execute(kos_query_t* query, const kos_term* K) {
    if (!query || !K) {
        return NULL;
    }
    
    // 1. 从知识集 K 中提取指定类型的所有实例
    kos_query_result_t* candidates = kos_extract_instances_by_type(K, query->type_name);
    if (!candidates) {
        return NULL;
    }
    
    // 2. 创建结果集
    kos_query_result_t* result = (kos_query_result_t*)calloc(1, sizeof(kos_query_result_t));
    if (!result) {
        kos_query_result_free(candidates);
        return NULL;
    }
    
    result->capacity = INITIAL_RESULT_CAPACITY;
    result->results = (kos_term**)malloc(result->capacity * sizeof(kos_term*));
    if (!result->results) {
        free(result);
        kos_query_result_free(candidates);
        return NULL;
    }
    
    result->count = 0;
    
    // 3. 应用 WHERE 条件过滤
    for (size_t i = 0; i < candidates->count; i++) {
        kos_term* instance = candidates->results[i];
        
        // 评估条件
        if (!query->conditions || evaluate_conditions(instance, query->conditions)) {
            // 满足条件，添加到结果
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
                    break; // 内存分配失败
                }
            }
            
            result->results[result->count++] = kos_term_copy(instance);
        }
    }
    
    // 4. 应用 ORDER BY（简化实现：按时间字段排序）
    if (query->order_by_field && result->count > 0) {
        // 使用简单的冒泡排序（实际应该使用更高效的排序算法）
        for (size_t i = 0; i < result->count - 1; i++) {
            for (size_t j = 0; j < result->count - i - 1; j++) {
                kos_term* field1 = kos_extract_field(result->results[j], query->order_by_field);
                kos_term* field2 = kos_extract_field(result->results[j + 1], query->order_by_field);
                
                if (field1 && field2) {
                    // 提取时间字符串进行比较
                    char* time1_str = NULL;
                    char* time2_str = NULL;
                    
                    if (field1->kind == KOS_TIME && field1->data.atomic.val) {
                        time1_str = strdup(field1->data.atomic.val);
                    }
                    if (field2->kind == KOS_TIME && field2->data.atomic.val) {
                        time2_str = strdup(field2->data.atomic.val);
                    }
                    
                    if (time1_str && time2_str) {
                        int cmp = kos_compare_time(time1_str, time2_str);
                        if ((query->order_desc && cmp < 0) || (!query->order_desc && cmp > 0)) {
                            // 交换
                            kos_term* temp = result->results[j];
                            result->results[j] = result->results[j + 1];
                            result->results[j + 1] = temp;
                        }
                    }
                    
                    free(time1_str);
                    free(time2_str);
                }
                
                kos_term_free(field1);
                kos_term_free(field2);
            }
        }
    }
    
    // 5. 应用 LIMIT
    if (query->limit > 0 && result->count > query->limit) {
        // 释放多余的结果
        for (size_t i = query->limit; i < result->count; i++) {
            kos_term_free(result->results[i]);
        }
        result->count = query->limit;
    }
    
    // 6. 清理临时结果
    kos_query_result_free(candidates);
    
    return result;
}

// ========== 查询结果管理 ==========

// 释放查询结果
void kos_query_result_free(kos_query_result_t* result) {
    if (!result) {
        return;
    }
    
    if (result->results) {
        for (size_t i = 0; i < result->count; i++) {
            if (result->results[i]) {
                kos_term_free(result->results[i]);
            }
        }
        free(result->results);
    }
    
    free(result);
}

// ========== 查询构建 API ==========

// 创建查询对象
kos_query_t* kos_query_create(const char* type_name) {
    if (!type_name) {
        return NULL;
    }
    
    kos_query_t* query = (kos_query_t*)calloc(1, sizeof(kos_query_t));
    if (!query) {
        return NULL;
    }
    
    query->type_name = strdup(type_name);
    query->limit = 0; // 0 表示无限制
    query->order_desc = false;
    query->aggregation = AGG_NONE;
    
    return query;
}

// 添加 WHERE 条件
int kos_query_add_condition(kos_query_t* query,
                           const char* field_name,
                           comparison_op_t op,
                           kos_term* value) {
    if (!query || !field_name || !value) {
        return -1;
    }
    
    query_condition_t* cond = (query_condition_t*)calloc(1, sizeof(query_condition_t));
    if (!cond) {
        return -1;
    }
    
    cond->field_name = strdup(field_name);
    cond->op = op;
    cond->value = kos_term_copy(value);
    cond->next = query->conditions;
    
    query->conditions = cond;
    
    return 0;
}

// 设置 ORDER BY
int kos_query_set_order_by(kos_query_t* query, const char* field_name, bool desc) {
    if (!query || !field_name) {
        return -1;
    }
    
    if (query->order_by_field) {
        free(query->order_by_field);
    }
    
    query->order_by_field = strdup(field_name);
    query->order_desc = desc;
    
    return 0;
}

// 设置 LIMIT
int kos_query_set_limit(kos_query_t* query, size_t limit) {
    if (!query) {
        return -1;
    }
    
    query->limit = limit;
    return 0;
}

// 设置聚合操作
int kos_query_set_aggregation(kos_query_t* query, aggregation_op_t op, const char* field_name) {
    if (!query) {
        return -1;
    }
    
    query->aggregation = op;
    if (field_name) {
        if (query->aggregation_field) {
            free(query->aggregation_field);
        }
        query->aggregation_field = strdup(field_name);
    }
    
    return 0;
}

// 释放查询对象
void kos_query_free(kos_query_t* query) {
    if (!query) {
        return;
    }
    
    if (query->type_name) {
        free(query->type_name);
    }
    
    // 释放条件链表
    query_condition_t* cond = query->conditions;
    while (cond) {
        query_condition_t* next = cond->next;
        if (cond->field_name) {
            free(cond->field_name);
        }
        if (cond->value) {
            kos_term_free(cond->value);
        }
        free(cond);
        cond = next;
    }
    
    if (query->order_by_field) {
        free(query->order_by_field);
    }
    
    if (query->aggregation_field) {
        free(query->aggregation_field);
    }
    
    free(query);
}

// ========== 查询解析（简化实现） ==========
// 注意：kos_query_parse 的实现位于 query_parser.c 中
