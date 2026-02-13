// src/integration/fusion_engine_enhanced.c
// Phase 3 Enhancement: Enhanced Data Fusion Engine
// 增强的数据融合引擎：类型安全的字段提取和融合

#include "../../include/kos_data_integration.h"
#include "../../include/kos_core.h"
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <float.h>

// ========== 字段提取辅助函数 ==========

// 从 Σ 类型中提取字段值
static kos_term* extract_sigma_field(kos_term* sigma_term, const char* field_name) {
    if (!sigma_term || sigma_term->kind != KOS_SIGMA || !field_name) {
        return NULL;
    }
    
    if (!sigma_term->data.sigma.fields) {
        return NULL;
    }
    
    for (size_t i = 0; i < sigma_term->data.sigma.field_count; i++) {
        kos_sigma_field_t* field = &sigma_term->data.sigma.fields[i];
        if (field->name && strcmp(field->name, field_name) == 0) {
            return field->value;
        }
    }
    
    return NULL;
}

// 从 kos_term 中提取数值（增强版：支持更多类型）
static double extract_numeric_value_enhanced(kos_term* term) {
    if (!term) {
        return 0.0;
    }
    
    switch (term->kind) {
        case KOS_VAL:
        case KOS_ID:
        case KOS_TIME:
            if (term->data.atomic.val) {
                return strtod(term->data.atomic.val, NULL);
            }
            break;
        default:
            break;
    }
    
    return 0.0;
}

// 比较两个值（用于 MAX/MIN 策略）
static int compare_values(kos_term* val1, kos_term* val2) {
    if (!val1 && !val2) return 0;
    if (!val1) return -1;
    if (!val2) return 1;
    
    // 数值比较
    double num1 = extract_numeric_value_enhanced(val1);
    double num2 = extract_numeric_value_enhanced(val2);
    
    if (num1 != 0.0 || num2 != 0.0) {
        if (num1 < num2) return -1;
        if (num1 > num2) return 1;
        return 0;
    }
    
    // 字符串比较
    const char* str1 = (val1->kind == KOS_VAL || val1->kind == KOS_ID) 
                      ? val1->data.atomic.val : NULL;
    const char* str2 = (val2->kind == KOS_VAL || val2->kind == KOS_ID) 
                      ? val2->data.atomic.val : NULL;
    
    if (str1 && str2) {
        return strcmp(str1, str2);
    }
    
    return 0;
}

// ========== 增强的融合策略实现 ==========

// 融合两个值（增强版：类型安全和完整策略支持）
static kos_term* fuse_values_enhanced(
    kos_term* val1,
    kos_term* val2,
    fusion_strategy_t strategy,
    double weight
) {
    if (!val1) return val2 ? kos_term_copy(val2) : NULL;
    if (!val2) return val1 ? kos_term_copy(val1) : NULL;
    
    switch (strategy) {
        case FUSION_LATEST_FIRST:
            // 最新优先：返回第二个值（假设 val2 是更新的）
            return kos_term_copy(val2);
            
        case FUSION_MAX: {
            int cmp = compare_values(val1, val2);
            return kos_term_copy(cmp >= 0 ? val1 : val2);
        }
        
        case FUSION_MIN: {
            int cmp = compare_values(val1, val2);
            return kos_term_copy(cmp <= 0 ? val1 : val2);
        }
        
        case FUSION_WEIGHTED_AVG: {
            // 加权平均：只对数值类型有效
            double num1 = extract_numeric_value_enhanced(val1);
            double num2 = extract_numeric_value_enhanced(val2);
            
            if (num1 == 0.0 && num2 == 0.0) {
                // 非数值类型，返回第二个值
                return kos_term_copy(val2);
            }
            
            double total_weight = weight + (1.0 - weight);
            double weighted_avg = (num1 * weight + num2 * (1.0 - weight)) / total_weight;
            
            char buffer[64];
            snprintf(buffer, sizeof(buffer), "%.6f", weighted_avg);
            return kos_mk_atomic(buffer, NULL);
        }
        
        case FUSION_UNION:
            // 并集：创建包含两个值的结构（简化：返回第二个）
            return kos_term_copy(val2);
            
        case FUSION_INTERSECTION:
            // 交集：只保留共同值（简化：如果相等则返回，否则返回 NULL）
            if (compare_values(val1, val2) == 0) {
                return kos_term_copy(val1);
            }
            return NULL;
            
        case FUSION_CUSTOM:
        default:
            // 自定义策略：返回第二个值（实际应该执行自定义规则）
            return kos_term_copy(val2);
    }
}

// ========== 增强的数据融合 ==========

// 融合两个数据项（增强版：字段级融合）
kos_term* kos_fuse_data_enhanced(
    kos_term* data1,
    kos_term* data2,
    kos_fusion_rule_t* rule
) {
    if (!data1 && !data2) {
        return NULL;
    }
    
    if (!data1) return data2 ? kos_term_copy(data2) : NULL;
    if (!data2) return data1 ? kos_term_copy(data1) : NULL;
    
    if (!rule) {
        // 没有规则：默认返回第二个数据
        return kos_term_copy(data2);
    }
    
    // 如果两个数据都是 Σ 类型，进行字段级融合
    if (data1->kind == KOS_SIGMA && data2->kind == KOS_SIGMA) {
        // 创建融合后的 Σ 类型
        kos_term* fused = (kos_term*)calloc(1, sizeof(kos_term));
        if (!fused) {
            return NULL;
        }
        
        fused->kind = KOS_SIGMA;
        fused->universe = data1->universe;
        
        // 确定字段数量（取两个数据源字段的并集）
        size_t max_fields = data1->data.sigma.field_count + data2->data.sigma.field_count;
        fused->data.sigma.fields = (kos_sigma_field_t*)calloc(
            max_fields, 
            sizeof(kos_sigma_field_t)
        );
        if (!fused->data.sigma.fields) {
            free(fused);
            return NULL;
        }
        
        fused->data.sigma.field_count = 0;
        
        // 融合字段
        // 1. 处理 data1 的所有字段
        for (size_t i = 0; i < data1->data.sigma.field_count; i++) {
            kos_sigma_field_t* field1 = &data1->data.sigma.fields[i];
            if (!field1->name) continue;
            
            // 查找 data2 中对应的字段
            kos_term* field2_val = extract_sigma_field(data2, field1->name);
            
            // 获取字段融合规则
            kos_field_fusion_rule_t* field_rule = NULL;
            for (size_t j = 0; j < rule->field_rule_count; j++) {
                if (strcmp(rule->field_rules[j].field_name, field1->name) == 0) {
                    field_rule = &rule->field_rules[j];
                    break;
                }
            }
            
            // 确定融合策略
            fusion_strategy_t strategy = field_rule ? field_rule->strategy : rule->default_strategy;
            double weight = field_rule ? field_rule->weight : 0.5;
            
            // 融合字段值
            kos_term* fused_value = fuse_values_enhanced(
                field1->value,
                field2_val,
                strategy,
                weight
            );
            
            if (fused_value) {
                kos_sigma_field_t* fused_field = &fused->data.sigma.fields[fused->data.sigma.field_count++];
                fused_field->name = strdup(field1->name);
                fused_field->type = field1->type ? kos_term_copy(field1->type) : NULL;
                fused_field->value = fused_value;
            }
        }
        
        // 2. 处理 data2 中 data1 没有的字段
        for (size_t i = 0; i < data2->data.sigma.field_count; i++) {
            kos_sigma_field_t* field2 = &data2->data.sigma.fields[i];
            if (!field2->name) continue;
            
            // 检查是否已经在融合结果中
            bool already_processed = false;
            for (size_t j = 0; j < fused->data.sigma.field_count; j++) {
                if (strcmp(fused->data.sigma.fields[j].name, field2->name) == 0) {
                    already_processed = true;
                    break;
                }
            }
            
            if (!already_processed) {
                // 添加新字段
                kos_sigma_field_t* fused_field = &fused->data.sigma.fields[fused->data.sigma.field_count++];
                fused_field->name = strdup(field2->name);
                fused_field->type = field2->type ? kos_term_copy(field2->type) : NULL;
                fused_field->value = field2->value ? kos_term_copy(field2->value) : NULL;
            }
        }
        
        return fused;
    }
    
    // 非 Σ 类型：使用简化融合
    return fuse_values_enhanced(data1, data2, rule->default_strategy, 0.5);
}

// ========== 批量融合增强 ==========

// 融合多个数据项（增强版：使用增强的融合函数）
kos_term* kos_fuse_data_array_enhanced(
    kos_term** data_array,
    size_t count,
    kos_fusion_rule_t* rule
) {
    if (!data_array || count == 0) {
        return NULL;
    }
    
    if (count == 1) {
        return data_array[0] ? kos_term_copy(data_array[0]) : NULL;
    }
    
    // 递归融合：fuse(data[0], fuse(data[1], ...))
    kos_term* result = data_array[0] ? kos_term_copy(data_array[0]) : NULL;
    for (size_t i = 1; i < count; i++) {
        if (data_array[i]) {
            kos_term* fused = kos_fuse_data_enhanced(result, data_array[i], rule);
            if (result) {
                kos_term_free(result);
            }
            result = fused;
        }
    }
    
    return result;
}

// ========== 时间戳感知的融合 ==========

// 从事件中提取时间戳（毫秒）
static int64_t extract_timestamp_ms(kos_term* event) {
    if (!event || event->kind != KOS_SIGMA) {
        return 0;
    }
    
    // 查找时间字段（常见名称：t, time, timestamp, event_time）
    const char* time_field_names[] = {"t", "time", "timestamp", "event_time", NULL};
    
    for (int i = 0; time_field_names[i]; i++) {
        kos_term* time_field = extract_sigma_field(event, time_field_names[i]);
        if (time_field) {
            // 尝试解析时间戳
            if (time_field->kind == KOS_TIME || time_field->kind == KOS_VAL) {
                const char* time_str = time_field->data.atomic.val;
                if (time_str) {
                    return (int64_t)strtoll(time_str, NULL, 10);
                }
            }
        }
    }
    
    return 0;
}

// 时间戳感知的融合（最新优先策略使用时间戳）
kos_term* kos_fuse_data_with_timestamp(
    kos_term* data1,
    kos_term* data2,
    kos_fusion_rule_t* rule
) {
    if (!data1 && !data2) {
        return NULL;
    }
    
    if (!data1) return data2 ? kos_term_copy(data2) : NULL;
    if (!data2) return data1 ? kos_term_copy(data1) : NULL;
    
    // 提取时间戳
    int64_t ts1 = extract_timestamp_ms(data1);
    int64_t ts2 = extract_timestamp_ms(data2);
    
    // 如果使用最新优先策略，根据时间戳选择
    if (rule && rule->default_strategy == FUSION_LATEST_FIRST) {
        return kos_term_copy(ts2 >= ts1 ? data2 : data1);
    }
    
    // 否则使用标准融合
    return kos_fuse_data_enhanced(data1, data2, rule);
}
