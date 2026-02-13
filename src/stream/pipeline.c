#include "kos_stream.h"
#include "kos_core.h"
#include "kos_kernel.h"
#include <stdlib.h>
#include <string.h>
#include <time.h>
#ifdef _WIN32
#include <windows.h>
#else
#include <sys/time.h>
#endif

// ========== 内部数据结构 ==========

// 窗口状态
struct kos_window_state {
    kos_window_config_t config;
    kos_stream_event_t* events;     // 窗口内的事件
    size_t event_count;
    size_t event_capacity;
    int64_t window_start_ms;
    int64_t window_end_ms;
    bool is_active;
    struct kos_window_state* next;
};

// 聚合状态
struct kos_aggregation_state {
    kos_aggregation_config_t config;
    kos_term* accumulator;           // 累加器
    size_t count;                   // 计数
    struct kos_aggregation_state* next;
};

// 流处理管道
struct kos_stream_pipeline {
    kos_term* input_type;
    kos_term* output_type;
    kos_time_type_t time_type;
    
    // 窗口列表
    kos_window_state_t* windows;
    
    // 聚合列表
    kos_aggregation_state_t* aggregations;
    
    // 水位线配置
    kos_watermark_config_t watermark_config;
    
    // 背压配置
    size_t max_queue_size;
    
    // 事件队列（用于背压控制）
    queue_t* event_queue;
    
    // 统计信息
    size_t total_events_processed;
    size_t total_windows_triggered;
    size_t total_backpressure_events;
};

// ========== 管道创建与销毁 ==========

kos_stream_pipeline_t* kos_create_stream_pipeline(
    kos_term* input_type,
    kos_term* output_type,
    kos_time_type_t time_type
) {
    if (!input_type || !output_type) {
        return NULL;
    }
    
    kos_stream_pipeline_t* pipeline = (kos_stream_pipeline_t*)calloc(1, sizeof(kos_stream_pipeline_t));
    if (!pipeline) {
        return NULL;
    }
    
    pipeline->input_type = kos_term_copy(input_type);
    pipeline->output_type = kos_term_copy(output_type);
    pipeline->time_type = time_type;
    pipeline->windows = NULL;
    pipeline->aggregations = NULL;
    pipeline->max_queue_size = 10000; // 默认最大队列大小
    
    // 初始化水位线配置
    pipeline->watermark_config.max_lateness_ms = 5000; // 默认5秒延迟容忍
    pipeline->watermark_config.watermark_interval_ms = 1000; // 默认1秒更新间隔
    pipeline->watermark_config.current_watermark_ms = 0;
    
    // 创建事件队列
    pipeline->event_queue = kos_queue_create();
    if (!pipeline->event_queue) {
        kos_term_free(pipeline->input_type);
        kos_term_free(pipeline->output_type);
        free(pipeline);
        return NULL;
    }
    
    pipeline->total_events_processed = 0;
    pipeline->total_windows_triggered = 0;
    pipeline->total_backpressure_events = 0;
    
    return pipeline;
}

void kos_stream_pipeline_free(kos_stream_pipeline_t* pipeline) {
    if (!pipeline) {
        return;
    }
    
    // 释放输入/输出类型
    if (pipeline->input_type) {
        kos_term_free(pipeline->input_type);
    }
    if (pipeline->output_type) {
        kos_term_free(pipeline->output_type);
    }
    
    // 释放所有窗口状态
    kos_window_state_t* window = pipeline->windows;
    while (window) {
        kos_window_state_t* next = window->next;
        if (window->events) {
            for (size_t i = 0; i < window->event_count; i++) {
                if (window->events[i].data) {
                    kos_term_free(window->events[i].data);
                }
            }
            free(window->events);
        }
        free(window);
        window = next;
    }
    
    // 释放所有聚合状态
    kos_aggregation_state_t* agg = pipeline->aggregations;
    while (agg) {
        kos_aggregation_state_t* next = agg->next;
        if (agg->accumulator) {
            kos_term_free(agg->accumulator);
        }
        free(agg);
        agg = next;
    }
    
    // 释放事件队列
    if (pipeline->event_queue) {
        kos_queue_free(pipeline->event_queue);
    }
    
    free(pipeline);
}

// ========== 窗口操作 ==========

int kos_stream_add_window(
    kos_stream_pipeline_t* pipeline,
    const kos_window_config_t* config
) {
    if (!pipeline || !config) {
        return -1;
    }
    
    kos_window_state_t* window = (kos_window_state_t*)calloc(1, sizeof(kos_window_state_t));
    if (!window) {
        return -1;
    }
    
    window->config = *config;
    window->event_capacity = 1024; // 初始容量
    window->events = (kos_stream_event_t*)calloc(window->event_capacity, sizeof(kos_stream_event_t));
    if (!window->events) {
        free(window);
        return -1;
    }
    
    window->event_count = 0;
    window->is_active = false;
    window->next = pipeline->windows;
    pipeline->windows = window;
    
    return 0;
}

// ========== 聚合操作 ==========

int kos_stream_add_aggregation(
    kos_stream_pipeline_t* pipeline,
    const kos_aggregation_config_t* config
) {
    if (!pipeline || !config) {
        return -1;
    }
    
    kos_aggregation_state_t* agg = (kos_aggregation_state_t*)calloc(1, sizeof(kos_aggregation_state_t));
    if (!agg) {
        return -1;
    }
    
    agg->config = *config;
    agg->accumulator = NULL;
    agg->count = 0;
    agg->next = pipeline->aggregations;
    pipeline->aggregations = agg;
    
    return 0;
}

// ========== 事件处理 ==========

int kos_stream_process_event(
    kos_stream_pipeline_t* pipeline,
    kos_term* event_data,
    int64_t event_time_ms
) {
    if (!pipeline || !event_data) {
        return -1;
    }
    
    // 检查背压
    if (kos_stream_should_backpressure(pipeline)) {
        pipeline->total_backpressure_events++;
        return -2; // 背压，拒绝事件
    }
    
    // 获取处理时间
    int64_t processing_time_ms = kos_get_processing_time_ms();
    
    // 创建流事件
    kos_stream_event_t stream_event;
    stream_event.data = kos_term_copy(event_data);
    stream_event.event_time_ms = event_time_ms;
    stream_event.processing_time_ms = processing_time_ms;
    
    // 更新水位线
    if (pipeline->time_type == KOS_TIME_EVENT_TIME) {
        kos_stream_update_watermark(pipeline, event_time_ms);
    }
    
    // 将事件添加到所有窗口
    kos_window_state_t* window = pipeline->windows;
    while (window) {
        // 检查事件是否属于该窗口
        int64_t time_to_use = (pipeline->time_type == KOS_TIME_EVENT_TIME) 
                              ? event_time_ms 
                              : processing_time_ms;
        
        if (!window->is_active) {
            // 初始化窗口
            window->window_start_ms = time_to_use;
            window->window_end_ms = time_to_use + window->config.window_size_ms;
            window->is_active = true;
        }
        
        // 检查事件是否在窗口内
        if (time_to_use >= window->window_start_ms && 
            time_to_use < window->window_end_ms) {
            // 扩展容量（如果需要）
            if (window->event_count >= window->event_capacity) {
                size_t new_capacity = window->event_capacity * 2;
                kos_stream_event_t* new_events = (kos_stream_event_t*)realloc(
                    window->events, 
                    new_capacity * sizeof(kos_stream_event_t)
                );
                if (!new_events) {
                    kos_term_free(stream_event.data);
                    return -1;
                }
                window->events = new_events;
                window->event_capacity = new_capacity;
            }
            
            // 添加事件到窗口
            window->events[window->event_count] = stream_event;
            window->event_count++;
            
            // 深拷贝事件数据（窗口拥有所有权）
            window->events[window->event_count - 1].data = kos_term_copy(stream_event.data);
        }
        
        window = window->next;
    }
    
    // 更新聚合状态
    kos_aggregation_state_t* agg = pipeline->aggregations;
    while (agg) {
        // 这里简化实现：聚合操作在窗口触发时执行
        // 实际应该维护每个窗口的聚合状态
        agg = agg->next;
    }
    
    pipeline->total_events_processed++;
    
    // 释放临时事件数据（窗口已拥有副本）
    kos_term_free(stream_event.data);
    
    return 0;
}

// ========== 窗口触发 ==========

kos_window_result_t* kos_stream_trigger_window(
    kos_stream_pipeline_t* pipeline,
    int64_t current_time_ms
) {
    if (!pipeline) {
        return NULL;
    }
    
    kos_window_state_t* window = pipeline->windows;
    if (!window || !window->is_active) {
        return NULL;
    }
    
    // 检查窗口是否应该触发
    int64_t time_to_use = (pipeline->time_type == KOS_TIME_EVENT_TIME)
                          ? kos_stream_get_current_watermark(pipeline)
                          : current_time_ms;
    
    if (time_to_use < window->window_end_ms) {
        return NULL; // 窗口尚未到期
    }
    
    // 创建窗口结果
    kos_window_result_t* result = (kos_window_result_t*)calloc(1, sizeof(kos_window_result_t));
    if (!result) {
        return NULL;
    }
    
    result->event_count = window->event_count;
    result->window_start_ms = window->window_start_ms;
    result->window_end_ms = window->window_end_ms;
    result->aggregations = NULL;
    result->aggregation_count = 0;
    result->next = NULL;
    
    if (window->event_count > 0) {
        result->events = (kos_stream_event_t*)malloc(
            window->event_count * sizeof(kos_stream_event_t)
        );
        if (!result->events) {
            free(result);
            return NULL;
        }
        
        // 复制事件
        for (size_t i = 0; i < window->event_count; i++) {
            result->events[i] = window->events[i];
            result->events[i].data = kos_term_copy(window->events[i].data);
        }
    }
    
    // 重置窗口（滑动窗口：移动窗口）
    if (window->config.type == KOS_WINDOW_SLIDING) {
        // 移动窗口
        window->window_start_ms += window->config.slide_size_ms;
        window->window_end_ms = window->window_start_ms + window->config.window_size_ms;
        
        // 移除过期事件
        size_t new_count = 0;
        for (size_t i = 0; i < window->event_count; i++) {
            int64_t event_time = (pipeline->time_type == KOS_TIME_EVENT_TIME)
                                 ? window->events[i].event_time_ms
                                 : window->events[i].processing_time_ms;
            
            if (event_time >= window->window_start_ms) {
                if (new_count != i) {
                    window->events[new_count] = window->events[i];
                }
                new_count++;
            } else {
                // 释放过期事件
                if (window->events[i].data) {
                    kos_term_free(window->events[i].data);
                }
            }
        }
        window->event_count = new_count;
    } else if (window->config.type == KOS_WINDOW_TUMBLING) {
        // 滚动窗口：清空并重新开始
        for (size_t i = 0; i < window->event_count; i++) {
            if (window->events[i].data) {
                kos_term_free(window->events[i].data);
            }
        }
        window->event_count = 0;
        window->window_start_ms = window->window_end_ms;
        window->window_end_ms = window->window_start_ms + window->config.window_size_ms;
    } else {
        // 会话窗口：保持当前状态，等待新事件
        // 这里简化处理：清空窗口
        for (size_t i = 0; i < window->event_count; i++) {
            if (window->events[i].data) {
                kos_term_free(window->events[i].data);
            }
        }
        window->event_count = 0;
        window->is_active = false;
    }
    
    pipeline->total_windows_triggered++;
    
    return result;
}

void kos_window_result_free(kos_window_result_t* result) {
    if (!result) {
        return;
    }
    
    if (result->events) {
        for (size_t i = 0; i < result->event_count; i++) {
            if (result->events[i].data) {
                kos_term_free(result->events[i].data);
            }
        }
        free(result->events);
    }
    
    // 释放聚合结果
    if (result->aggregations) {
        for (size_t i = 0; i < result->aggregation_count; i++) {
            if (result->aggregations[i].result) {
                kos_term_free(result->aggregations[i].result);
            }
        }
        free(result->aggregations);
    }
    
    // 释放下一个窗口结果
    if (result->next) {
        kos_window_result_free(result->next);
    }
    
    free(result);
}

// ========== 聚合结果获取 ==========

kos_aggregation_result_t* kos_stream_get_aggregation_results(
    kos_stream_pipeline_t* pipeline,
    size_t* result_count
) {
    if (!pipeline || !result_count) {
        return NULL;
    }
    
    // 计算聚合数量
    size_t count = 0;
    kos_aggregation_state_t* agg = pipeline->aggregations;
    while (agg) {
        count++;
        agg = agg->next;
    }
    
    if (count == 0) {
        *result_count = 0;
        return NULL;
    }
    
    kos_aggregation_result_t* results = (kos_aggregation_result_t*)calloc(
        count, 
        sizeof(kos_aggregation_result_t)
    );
    if (!results) {
        return NULL;
    }
    
    size_t idx = 0;
    agg = pipeline->aggregations;
    while (agg) {
        results[idx].field_name = agg->config.field_name;
        results[idx].op = agg->config.op;
        results[idx].count = agg->count;
        results[idx].result = agg->accumulator ? kos_term_copy(agg->accumulator) : NULL;
        
        idx++;
        agg = agg->next;
    }
    
    *result_count = count;
    return results;
}

void kos_aggregation_result_free(kos_aggregation_result_t* result) {
    if (!result) {
        return;
    }
    
    // 注意：这里假设 result 是数组，需要知道数组大小
    // 实际使用中应该配合 result_count 一起释放
    // 这里简化处理：只释放单个结果
    if (result->result) {
        kos_term_free(result->result);
    }
}

// ========== 水位线管理 ==========

int kos_stream_set_watermark_config(
    kos_stream_pipeline_t* pipeline,
    const kos_watermark_config_t* config
) {
    if (!pipeline || !config) {
        return -1;
    }
    
    pipeline->watermark_config = *config;
    return 0;
}

int64_t kos_stream_get_current_watermark(
    kos_stream_pipeline_t* pipeline
) {
    if (!pipeline) {
        return 0;
    }
    
    return pipeline->watermark_config.current_watermark_ms;
}

int kos_stream_update_watermark(
    kos_stream_pipeline_t* pipeline,
    int64_t event_time_ms
) {
    if (!pipeline) {
        return -1;
    }
    
    // 更新水位线：取最大事件时间减去延迟容忍度
    int64_t new_watermark = event_time_ms - pipeline->watermark_config.max_lateness_ms;
    if (new_watermark > pipeline->watermark_config.current_watermark_ms) {
        pipeline->watermark_config.current_watermark_ms = new_watermark;
    }
    
    return 0;
}

// ========== 背压处理 ==========

int kos_stream_set_backpressure_threshold(
    kos_stream_pipeline_t* pipeline,
    size_t max_queue_size
) {
    if (!pipeline) {
        return -1;
    }
    
    pipeline->max_queue_size = max_queue_size;
    return 0;
}

kos_backpressure_state_t kos_stream_get_backpressure_state(
    kos_stream_pipeline_t* pipeline
) {
    kos_backpressure_state_t state = {0};
    
    if (!pipeline) {
        return state;
    }
    
    state.queue_size = kos_queue_size(pipeline->event_queue);
    state.max_queue_size = pipeline->max_queue_size;
    state.is_backpressure = (state.queue_size >= state.max_queue_size);
    
    if (state.max_queue_size > 0) {
        state.backpressure_ratio = (float)state.queue_size / (float)state.max_queue_size;
        if (state.backpressure_ratio > 1.0f) {
            state.backpressure_ratio = 1.0f;
        }
    }
    
    return state;
}

bool kos_stream_should_backpressure(
    kos_stream_pipeline_t* pipeline
) {
    if (!pipeline) {
        return false;
    }
    
    kos_backpressure_state_t state = kos_stream_get_backpressure_state(pipeline);
    return state.is_backpressure;
}

// ========== 流处理管道执行 ==========

int kos_stream_execute(
    kos_stream_pipeline_t* pipeline,
    kos_term* output_buffer,
    size_t* output_count
) {
    // 简化实现：处理队列中的事件并触发窗口
    if (!pipeline || !output_count) {
        return -1;
    }
    
    *output_count = 0;
    
    // 处理队列中的事件
    while (!kos_queue_is_empty(pipeline->event_queue)) {
        kos_term* event_pair = kos_queue_dequeue(pipeline->event_queue);
        if (!event_pair) {
            break;
        }
        
        // 提取事件数据和时间戳
        if (event_pair->kind == KOS_PAIR && event_pair->data.pair.data) {
            kos_term* event_data = event_pair->data.pair.data;
            int64_t event_time = kos_extract_event_time(event_data, NULL);
            
            kos_stream_process_event(pipeline, event_data, event_time);
        }
        
        kos_term_free(event_pair);
    }
    
    // 触发窗口（使用当前时间）
    int64_t current_time = kos_get_processing_time_ms();
    kos_window_result_t* window_result = kos_stream_trigger_window(pipeline, current_time);
    
    if (window_result) {
        // 这里可以将窗口结果添加到输出缓冲区
        // 简化实现：只返回窗口结果数量
        *output_count = window_result->event_count;
        kos_window_result_free(window_result);
    }
    
    return 0;
}

// ========== 辅助函数 ==========

int64_t kos_extract_event_time(
    kos_term* event,
    const char* time_field
) {
    // 简化实现：从事件中提取时间戳
    // 实际应该根据 time_field 从 kos_term 中提取
    // 这里返回当前时间作为占位符
    return kos_get_processing_time_ms();
}

int64_t kos_get_processing_time_ms(void) {
#ifdef _WIN32
    // Windows: 使用 GetSystemTimeAsFileTime
    FILETIME ft;
    GetSystemTimeAsFileTime(&ft);
    ULARGE_INTEGER uli;
    uli.LowPart = ft.dwLowDateTime;
    uli.HighPart = ft.dwHighDateTime;
    // Windows FILETIME is in 100-nanosecond intervals since 1601-01-01
    // Convert to milliseconds since 1970-01-01
    return (int64_t)((uli.QuadPart / 10000) - 11644473600000LL);
#else
    // Unix/Linux: 使用 clock_gettime
    struct timespec ts;
    if (clock_gettime(CLOCK_REALTIME, &ts) == 0) {
        return (int64_t)ts.tv_sec * 1000 + (int64_t)ts.tv_nsec / 1000000;
    }
    return 0;
#endif
}
