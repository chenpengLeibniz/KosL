// src/integration/fusion_engine.c
// 数据融合规则引擎实现

#include "kos_data_integration.h"
#include "kos_core.h"
#include <stdlib.h>
#include <string.h>
#include <math.h>

// 创建融合规则
kos_fusion_rule_t* kos_create_fusion_rule(
    const char* target_type,
    fusion_strategy_t default_strategy
) {
    if (!target_type) {
        return NULL;
    }
    
    kos_fusion_rule_t* rule = (kos_fusion_rule_t*)malloc(sizeof(kos_fusion_rule_t));
    if (!rule) {
        return NULL;
    }
    
    rule->target_type = strdup(target_type);
    rule->field_rules = NULL;
    rule->field_rule_count = 0;
    rule->default_strategy = default_strategy;
    rule->type_constraint = NULL;
    
    return rule;
}

// 添加字段融合规则
int kos_fusion_rule_add_field(
    kos_fusion_rule_t* rule,
    const char* field_name,
    fusion_strategy_t strategy,
    double weight
) {
    if (!rule || !field_name) {
        return -1;
    }
    
    // 重新分配字段规则数组
    kos_field_fusion_rule_t* new_rules = (kos_field_fusion_rule_t*)realloc(
        rule->field_rules,
        (rule->field_rule_count + 1) * sizeof(kos_field_fusion_rule_t)
    );
    
    if (!new_rules) {
        return -2;
    }
    
    rule->field_rules = new_rules;
    
    // 添加新规则
    kos_field_fusion_rule_t* new_rule = &rule->field_rules[rule->field_rule_count];
    new_rule->field_name = strdup(field_name);
    new_rule->strategy = strategy;
    new_rule->weight = weight;
    new_rule->custom_rule = NULL;
    
    rule->field_rule_count++;
    return 0;
}

// 释放融合规则
void kos_fusion_rule_free(kos_fusion_rule_t* rule) {
    if (!rule) {
        return;
    }
    
    if (rule->target_type) {
        free((void*)rule->target_type);
    }
    
    if (rule->field_rules) {
        for (size_t i = 0; i < rule->field_rule_count; i++) {
            if (rule->field_rules[i].field_name) {
                free((void*)rule->field_rules[i].field_name);
            }
            // 注意：custom_rule 应该由调用者管理
        }
        free(rule->field_rules);
    }
    
    // 注意：type_constraint 应该由调用者管理
    
    free(rule);
}

// 获取字段融合规则
static kos_field_fusion_rule_t* get_field_rule(
    kos_fusion_rule_t* rule,
    const char* field_name
) {
    if (!rule || !field_name) {
        return NULL;
    }
    
    for (size_t i = 0; i < rule->field_rule_count; i++) {
        if (strcmp(rule->field_rules[i].field_name, field_name) == 0) {
            return &rule->field_rules[i];
        }
    }
    
    return NULL;
}

// 融合两个值（简化实现，实际应该根据类型进行类型安全的融合）
static kos_term* fuse_values(
    kos_term* val1,
    kos_term* val2,
    fusion_strategy_t strategy,
    double weight
) {
    if (!val1) return val2;
    if (!val2) return val1;
    
    // 简化实现：根据策略选择值
    // 实际实现应该：
    // 1. 检查类型兼容性
    // 2. 根据策略执行融合（加权平均、最大值、最小值等）
    // 3. 返回融合后的值
    
    switch (strategy) {
        case FUSION_LATEST_FIRST:
            // 假设 val2 是更新的（简化）
            return val2;
            
        case FUSION_MAX:
            // 简化：比较字符串值
            if (val1->kind == KOS_VAL && val2->kind == KOS_VAL &&
                val1->data.atomic.val && val2->data.atomic.val) {
                return (strcmp(val1->data.atomic.val, val2->data.atomic.val) > 0) ? val1 : val2;
            }
            return val2;
            
        case FUSION_MIN:
            if (val1->kind == KOS_VAL && val2->kind == KOS_VAL &&
                val1->data.atomic.val && val2->data.atomic.val) {
                return (strcmp(val1->data.atomic.val, val2->data.atomic.val) < 0) ? val1 : val2;
            }
            return val1;
            
        case FUSION_WEIGHTED_AVG:
            // 简化：对于数值类型应该计算加权平均
            // 这里只是占位符
            return val2;
            
        case FUSION_UNION:
        case FUSION_INTERSECTION:
        case FUSION_CUSTOM:
        default:
            // 简化：默认返回第二个值
            return val2;
    }
}

// 融合两个数据项
kos_term* kos_fuse_data(
    kos_term* data1,
    kos_term* data2,
    kos_fusion_rule_t* rule
) {
    if (!data1 && !data2) {
        return NULL;
    }
    
    if (!data1) return data2;
    if (!data2) return data1;
    
    if (!rule) {
        // 没有规则：默认返回第二个数据
        return data2;
    }
    
    // 简化实现：创建融合后的数据项
    // 实际实现应该：
    // 1. 遍历所有字段
    // 2. 对每个字段应用融合规则
    // 3. 构建融合后的记录
    
    // 这里返回一个占位符
    kos_term* fused = (kos_term*)malloc(sizeof(kos_term));
    if (!fused) {
        return NULL;
    }
    
    // 简化：复制 data2 的结构（实际应该融合）
    fused->kind = data2->kind;
    fused->universe = data2->universe;
    
    // 实际实现应该根据规则融合字段
    // 这里只是占位符
    
    return fused;
}

// 融合多个数据项
kos_term* kos_fuse_data_array(
    kos_term** data_array,
    size_t count,
    kos_fusion_rule_t* rule
) {
    if (!data_array || count == 0) {
        return NULL;
    }
    
    if (count == 1) {
        return data_array[0];
    }
    
    // 递归融合：fuse(data[0], fuse(data[1], ...))
    kos_term* result = data_array[0];
    for (size_t i = 1; i < count; i++) {
        kos_term* fused = kos_fuse_data(result, data_array[i], rule);
        if (fused != result) {
            // 如果返回了新对象，可能需要释放旧的（简化：不释放）
        }
        result = fused;
    }
    
    return result;
}

// 融合来自多个数据源的数据
kos_term* kos_fuse_from_sources(
    kos_data_source_handle_t** sources,
    size_t source_count,
    const char* query_or_filter,
    kos_fusion_rule_t* rule
) {
    if (!sources || source_count == 0) {
        return NULL;
    }
    
    // 从所有数据源读取数据
    kos_term** data_array = (kos_term**)malloc(source_count * sizeof(kos_term*));
    if (!data_array) {
        return NULL;
    }
    
    size_t valid_count = 0;
    for (size_t i = 0; i < source_count; i++) {
        if (sources[i]) {
            kos_term* data = kos_data_source_read_data(sources[i], query_or_filter);
            if (data) {
                data_array[valid_count++] = data;
            }
        }
    }
    
    if (valid_count == 0) {
        free(data_array);
        return NULL;
    }
    
    // 融合所有数据
    kos_term* result = kos_fuse_data_array(data_array, valid_count, rule);
    
    // 清理（简化：不释放 data_array 中的项，由调用者管理）
    free(data_array);
    
    return result;
}
