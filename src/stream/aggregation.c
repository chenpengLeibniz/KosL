#include "kos_stream.h"
#include "kos_core.h"
#include <stdlib.h>
#include <string.h>
#include <math.h>

// ========== 聚合操作实现 ==========

// 从 kos_term 中提取数值（简化实现）
static double extract_numeric_value(kos_term* term) {
    if (!term) {
        return 0.0;
    }
    
    if (term->kind == KOS_VAL && term->data.atomic.val) {
        return strtod(term->data.atomic.val, NULL);
    }
    
    return 0.0;
}

// 更新聚合状态（内部函数）
static int update_aggregation_internal(
    kos_aggregation_state_t* agg_state,
    kos_term* event_data,
    const char* field_name
) {
    if (!agg_state || !event_data) {
        return -1;
    }
    
    // 简化实现：从事件中提取字段值
    // 实际应该根据 field_name 从 kos_term 结构中提取
    kos_term* field_value = event_data; // 占位符：实际应该提取字段
    
    double value = extract_numeric_value(field_value);
    
    switch (agg_state->config.op) {
        case KOS_AGG_SUM:
        case KOS_AGG_AVG:
            if (!agg_state->accumulator) {
                // 初始化累加器
                agg_state->accumulator = kos_mk_atomic("0.0", NULL);
            } else {
                double current = extract_numeric_value(agg_state->accumulator);
                double new_value = current + value;
                kos_term_free(agg_state->accumulator);
                char buffer[64];
                snprintf(buffer, sizeof(buffer), "%.6f", new_value);
                agg_state->accumulator = kos_mk_atomic(buffer, NULL);
            }
            agg_state->count++;
            break;
            
        case KOS_AGG_COUNT:
            agg_state->count++;
            if (!agg_state->accumulator) {
                agg_state->accumulator = kos_mk_atomic("0", NULL);
            } else {
                double current = extract_numeric_value(agg_state->accumulator);
                double new_value = current + 1.0;
                kos_term_free(agg_state->accumulator);
                char buffer[64];
                snprintf(buffer, sizeof(buffer), "%.0f", new_value);
                agg_state->accumulator = kos_mk_atomic(buffer, NULL);
            }
            break;
            
        case KOS_AGG_MAX:
            if (!agg_state->accumulator) {
                char buffer[64];
                snprintf(buffer, sizeof(buffer), "%.6f", value);
                agg_state->accumulator = kos_mk_atomic(buffer, NULL);
            } else {
                double current = extract_numeric_value(agg_state->accumulator);
                if (value > current) {
                    kos_term_free(agg_state->accumulator);
                    char buffer[64];
                    snprintf(buffer, sizeof(buffer), "%.6f", value);
                    agg_state->accumulator = kos_mk_atomic(buffer, NULL);
                }
            }
            agg_state->count++;
            break;
            
        case KOS_AGG_MIN:
            if (!agg_state->accumulator) {
                char buffer[64];
                snprintf(buffer, sizeof(buffer), "%.6f", value);
                agg_state->accumulator = kos_mk_atomic(buffer, NULL);
            } else {
                double current = extract_numeric_value(agg_state->accumulator);
                if (value < current) {
                    kos_term_free(agg_state->accumulator);
                    char buffer[64];
                    snprintf(buffer, sizeof(buffer), "%.6f", value);
                    agg_state->accumulator = kos_mk_atomic(buffer, NULL);
                }
            }
            agg_state->count++;
            break;
            
        case KOS_AGG_FIRST:
            if (!agg_state->accumulator) {
                char buffer[64];
                snprintf(buffer, sizeof(buffer), "%.6f", value);
                agg_state->accumulator = kos_mk_atomic(buffer, NULL);
                agg_state->count = 1;
            }
            break;
            
        case KOS_AGG_LAST:
            {
                kos_term_free(agg_state->accumulator);
                char buffer[64];
                snprintf(buffer, sizeof(buffer), "%.6f", value);
                agg_state->accumulator = kos_mk_atomic(buffer, NULL);
                agg_state->count++;
            }
            break;
            
        default:
            return -1;
    }
    
    return 0;
}

// 计算最终聚合结果（用于平均值等，内部函数）
static kos_term* compute_aggregation_result_internal(
    kos_aggregation_state_t* agg_state
) {
    if (!agg_state) {
        return NULL;
    }
    
    if (agg_state->config.op == KOS_AGG_AVG && agg_state->count > 0) {
        double sum = extract_numeric_value(agg_state->accumulator);
        double avg = sum / (double)agg_state->count;
        char buffer[64];
        snprintf(buffer, sizeof(buffer), "%.6f", avg);
        return kos_create_atomic_term(buffer);
    }
    
    // 其他操作直接返回累加器
    return agg_state->accumulator ? kos_term_copy(agg_state->accumulator) : NULL;
}

// 重置聚合状态（内部函数）
static void reset_aggregation_state_internal(kos_aggregation_state_t* agg_state) {
    if (!agg_state) {
        return;
    }
    
    if (agg_state->accumulator) {
        kos_term_free(agg_state->accumulator);
        agg_state->accumulator = NULL;
    }
    agg_state->count = 0;
}
