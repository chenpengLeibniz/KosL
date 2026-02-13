// src/stream/aggregation_integration.c
// Phase 4 Enhancement: Aggregation Integration with Pipeline
// 聚合操作与流处理管道的完整集成

#include "kos_stream.h"
#include "kos_core.h"
#include <stdlib.h>
#include <string.h>
#include <math.h>

// 前向声明
struct kos_aggregation_state;
typedef struct kos_aggregation_state kos_aggregation_state_t;

// ========== 聚合更新辅助函数 ==========

// 从 kos_term 中提取数值
static double extract_numeric_value(kos_term* term) {
    if (!term) {
        return 0.0;
    }
    
    if (term->kind == KOS_VAL && term->data.atomic.val) {
        return strtod(term->data.atomic.val, NULL);
    }
    
    return 0.0;
}

// 更新聚合状态（使用 aggregation.c 中的函数）
// 注意：需要将 aggregation.c 中的 update_aggregation_internal 改为非 static
// 或者在这里重新实现聚合逻辑
static int update_aggregation_for_window(
    kos_aggregation_config_t* config,
    kos_term** accumulator,
    size_t* count,
    kos_term* field_value
) {
    if (!config || !accumulator || !count) {
        return -1;
    }
    
    double value = extract_numeric_value(field_value);
    
    switch (config->op) {
        case KOS_AGG_SUM:
        case KOS_AGG_AVG: {
            double current = *accumulator ? extract_numeric_value(*accumulator) : 0.0;
            double new_value = current + value;
            if (*accumulator) {
                kos_term_free(*accumulator);
            }
            char buffer[64];
            snprintf(buffer, sizeof(buffer), "%.6f", new_value);
            *accumulator = kos_mk_atomic(buffer, NULL);
            (*count)++;
            break;
        }
        case KOS_AGG_COUNT: {
            (*count)++;
            double current = *accumulator ? extract_numeric_value(*accumulator) : 0.0;
            double new_value = current + 1.0;
            if (*accumulator) {
                kos_term_free(*accumulator);
            }
            char buffer[64];
            snprintf(buffer, sizeof(buffer), "%.0f", new_value);
            *accumulator = kos_mk_atomic(buffer, NULL);
            break;
        }
        case KOS_AGG_MAX: {
            double current = *accumulator ? extract_numeric_value(*accumulator) : -DBL_MAX;
            if (value > current) {
                if (*accumulator) {
                    kos_term_free(*accumulator);
                }
                char buffer[64];
                snprintf(buffer, sizeof(buffer), "%.6f", value);
                *accumulator = kos_mk_atomic(buffer, NULL);
            }
            (*count)++;
            break;
        }
        case KOS_AGG_MIN: {
            double current = *accumulator ? extract_numeric_value(*accumulator) : DBL_MAX;
            if (value < current) {
                if (*accumulator) {
                    kos_term_free(*accumulator);
                }
                char buffer[64];
                snprintf(buffer, sizeof(buffer), "%.6f", value);
                *accumulator = kos_mk_atomic(buffer, NULL);
            }
            (*count)++;
            break;
        }
        case KOS_AGG_FIRST: {
            if (!*accumulator) {
                *accumulator = kos_term_copy(field_value);
            }
            (*count)++;
            break;
        }
        case KOS_AGG_LAST: {
            if (*accumulator) {
                kos_term_free(*accumulator);
            }
            *accumulator = kos_term_copy(field_value);
            (*count)++;
            break;
        }
        default:
            return -1;
    }
    
    return 0;
}

// ========== 窗口聚合状态管理 ==========

// 窗口聚合状态（每个窗口维护独立的聚合状态）
typedef struct {
    kos_aggregation_state_t* aggregations;  // 该窗口的聚合状态列表
    int64_t window_start_ms;                // 窗口开始时间
    int64_t window_end_ms;                  // 窗口结束时间
    struct window_agg_state* next;          // 下一个窗口
} window_agg_state_t;

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

// ========== 聚合操作与窗口集成 ==========

// 更新窗口的聚合状态
int kos_stream_update_window_aggregations(
    kos_stream_pipeline_t* pipeline,
    kos_window_state_t* window,
    kos_term* event_data
) {
    if (!pipeline || !window || !event_data) {
        return -1;
    }
    
    // 遍历所有聚合配置
    // 注意：由于聚合状态是内部结构，聚合更新在窗口触发时统一计算
    // 这里简化实现：不做实时更新
    (void)pipeline;
    (void)window;
    (void)event_data;
    
    return 0;
}

// 在窗口触发时计算聚合结果
kos_aggregation_result_t* kos_stream_compute_window_aggregations(
    kos_stream_pipeline_t* pipeline,
    kos_window_state_t* window,
    size_t* result_count
) {
    if (!pipeline || !window || !result_count) {
        return NULL;
    }
    
    // 获取聚合配置列表（通过 API）
    size_t agg_count = 0;
    kos_aggregation_result_t* current_agg_results = kos_stream_get_aggregation_results(pipeline, &agg_count);
    
    if (!current_agg_results || agg_count == 0) {
        *result_count = 0;
        return NULL;
    }
    
    kos_aggregation_result_t* results = (kos_aggregation_result_t*)calloc(
        agg_count, 
        sizeof(kos_aggregation_result_t)
    );
    if (!results) {
        // 清理临时结果
        for (size_t i = 0; i < agg_count; i++) {
            if (current_agg_results[i].result) {
                kos_term_free(current_agg_results[i].result);
            }
        }
        free(current_agg_results);
        *result_count = 0;
        return NULL;
    }
    
    // 为每个聚合配置创建窗口级别的聚合状态
    for (size_t agg_idx = 0; agg_idx < agg_count; agg_idx++) {
        kos_aggregation_config_t config = {0};
        config.field_name = current_agg_results[agg_idx].field_name;
        config.op = current_agg_results[agg_idx].op;
        
        kos_term* accumulator = NULL;
        size_t event_count = 0;
        
        // 遍历窗口内所有事件
        for (size_t i = 0; i < window->event_count; i++) {
            kos_term* event_data = window->events[i].data;
            if (event_data) {
                kos_term* field_value = NULL;
                if (event_data->kind == KOS_SIGMA && config.field_name) {
                    field_value = extract_sigma_field(event_data, config.field_name);
                } else {
                    field_value = event_data;
                }
                
                if (field_value) {
                    update_aggregation_for_window(&config, &accumulator, &event_count, field_value);
                }
            }
        }
        
        // 对于 AVG，需要计算平均值
        if (config.op == KOS_AGG_AVG && event_count > 0 && accumulator) {
            double sum = extract_numeric_value(accumulator);
            double avg = sum / event_count;
            
            kos_term_free(accumulator);
            char buffer[64];
            snprintf(buffer, sizeof(buffer), "%.6f", avg);
            accumulator = kos_mk_atomic(buffer, NULL);
        }
        
        // 保存结果
        results[agg_idx].field_name = config.field_name;
        results[agg_idx].op = config.op;
        results[agg_idx].count = event_count;
        results[agg_idx].result = accumulator;
    }
    
    // 清理临时结果
    for (size_t i = 0; i < agg_count; i++) {
        if (current_agg_results[i].result) {
            kos_term_free(current_agg_results[i].result);
        }
    }
    free(current_agg_results);
    
    *result_count = agg_count;
    return results;
}

// ========== 增强的窗口触发（包含聚合） ==========

// 触发窗口并计算聚合（增强版）
// 注意：由于无法直接访问内部结构，这个函数使用标准 API
kos_window_result_t* kos_stream_trigger_window_with_aggregations(
    kos_stream_pipeline_t* pipeline,
    int64_t current_time_ms
) {
    if (!pipeline) {
        return NULL;
    }
    
    // 使用标准窗口触发 API
    kos_window_result_t* base_window_result = kos_stream_trigger_window(pipeline, current_time_ms);
    if (!base_window_result) {
        return NULL;
    }
    
    // 计算聚合结果（基于窗口内的事件）
    size_t global_agg_count = 0;
    kos_aggregation_result_t* global_agg_results = kos_stream_get_aggregation_results(
        pipeline, &global_agg_count
    );
    
    // 如果没有聚合配置或没有事件，直接返回
    if (!global_agg_results || global_agg_count == 0 || base_window_result->event_count == 0) {
        base_window_result->aggregation_count = 0;
        base_window_result->aggregations = NULL;
        if (global_agg_results) {
            for (size_t i = 0; i < global_agg_count; i++) {
                if (global_agg_results[i].result) {
                    kos_term_free(global_agg_results[i].result);
                }
            }
            free(global_agg_results);
        }
        return base_window_result;
    }
    
    // 为窗口重新计算聚合
    kos_aggregation_result_t* agg_results = (kos_aggregation_result_t*)calloc(
        global_agg_count, sizeof(kos_aggregation_result_t)
    );
    if (!agg_results) {
        base_window_result->aggregation_count = 0;
        base_window_result->aggregations = NULL;
        // 清理全局聚合结果
        for (size_t i = 0; i < global_agg_count; i++) {
            if (global_agg_results[i].result) {
                kos_term_free(global_agg_results[i].result);
            }
        }
        free(global_agg_results);
        return base_window_result;
    }
    
    // 计算每个聚合
    size_t event_count = base_window_result->event_count;
    for (size_t i = 0; i < global_agg_count; i++) {
        agg_results[i].field_name = global_agg_results[i].field_name;
        agg_results[i].op = global_agg_results[i].op;
        agg_results[i].count = 0;
        agg_results[i].result = NULL;
        
        // 对窗口内事件进行聚合
        kos_term* accumulator = NULL;
        for (size_t j = 0; j < event_count; j++) {
            kos_term* event_data = base_window_result->events[j].data;
            if (event_data) {
                kos_term* field_value = NULL;
                if (event_data->kind == KOS_SIGMA && global_agg_results[i].field_name) {
                    field_value = extract_sigma_field(event_data, global_agg_results[i].field_name);
                } else {
                    field_value = event_data;
                }
                
                if (field_value) {
                    kos_aggregation_config_t config = {0};
                    config.field_name = global_agg_results[i].field_name;
                    config.op = global_agg_results[i].op;
                    update_aggregation_for_window(&config, &accumulator, &agg_results[i].count, field_value);
                }
            }
        }
        
        // 对于 AVG，计算平均值
        if (global_agg_results[i].op == KOS_AGG_AVG && agg_results[i].count > 0 && accumulator) {
            double sum = extract_numeric_value(accumulator);
            double avg = sum / agg_results[i].count;
            kos_term_free(accumulator);
            char buffer[64];
            snprintf(buffer, sizeof(buffer), "%.6f", avg);
            accumulator = kos_mk_atomic(buffer, NULL);
        }
        
        agg_results[i].result = accumulator;
    }
    
    // 设置聚合结果
    base_window_result->aggregation_count = global_agg_count;
    base_window_result->aggregations = agg_results;
    
    // 清理全局聚合结果
    for (size_t i = 0; i < global_agg_count; i++) {
        if (global_agg_results[i].result) {
            kos_term_free(global_agg_results[i].result);
        }
    }
    free(global_agg_results);
    
    return base_window_result;
}

// ========== 实时聚合更新 ==========

// 处理事件时实时更新聚合（增强版）
int kos_stream_process_event_with_aggregation(
    kos_stream_pipeline_t* pipeline,
    kos_term* event_data,
    int64_t event_time_ms
) {
    if (!pipeline || !event_data) {
        return -1;
    }
    
    // 处理事件到窗口（原有逻辑）
    int process_result = kos_stream_process_event(pipeline, event_data, event_time_ms);
    if (process_result != 0) {
        return process_result;
    }
    
    // 注意：聚合更新在窗口触发时统一计算
    // 这里不做实时更新，避免重复计算
    
    return 0;
}
