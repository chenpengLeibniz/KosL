// Phase 4: Real-time Stream Processing Demo
// 实时流处理演示程序
#include "kos_stream.h"
#include "kos_core.h"
#include "kos_kernel.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#ifdef _WIN32
#include <windows.h>
#endif

// 辅助函数：创建示例事件
static kos_term* create_sample_event(const char* id, double value, int64_t timestamp_ms) {
    // 简化实现：创建一个包含ID、值和时间戳的事件
    // 实际应该使用更复杂的 kos_term 结构
    char buffer[256];
    snprintf(buffer, sizeof(buffer), "{\"id\":\"%s\",\"value\":%.2f,\"timestamp\":%lld}", 
             id, value, (long long)timestamp_ms);
    return kos_mk_atomic(buffer, NULL);
}

// 辅助函数：打印窗口结果
static void print_window_result(kos_window_result_t* result) {
    if (!result) {
        return;
    }
    
    printf("=== Window Result ===\n");
    printf("Window: [%lld ms - %lld ms]\n", 
           (long long)result->window_start_ms, 
           (long long)result->window_end_ms);
    printf("Event Count: %zu\n", result->event_count);
    
    for (size_t i = 0; i < result->event_count; i++) {
        printf("  Event %zu: time=%lld ms, processing_time=%lld ms\n",
               i,
               (long long)result->events[i].event_time_ms,
               (long long)result->events[i].processing_time_ms);
        
        if (result->events[i].data) {
            kos_serialized* serialized = kos_term_serialize(result->events[i].data);
            if (serialized && serialized->data) {
                printf("    Data: %s\n", serialized->data);
                kos_serialized_free(serialized);
            }
        }
    }
    printf("\n");
}

int main() {
    printf("=== KOS-TL Phase 4: Real-time Stream Processing Demo ===\n\n");
    
    // 1. 创建输入和输出类型（简化：使用原子类型）
    kos_term* input_type = kos_mk_prop("StreamEvent");
    kos_term* output_type = kos_mk_prop("WindowResult");
    
    if (!input_type || !output_type) {
        fprintf(stderr, "Failed to create types\n");
        return 1;
    }
    
    // 2. 创建流处理管道（使用事件时间）
    kos_stream_pipeline_t* pipeline = kos_create_stream_pipeline(
        input_type,
        output_type,
        KOS_TIME_EVENT_TIME
    );
    
    if (!pipeline) {
        fprintf(stderr, "Failed to create stream pipeline\n");
        kos_term_free(input_type);
        kos_term_free(output_type);
        return 1;
    }
    
    // 3. 配置滑动窗口（窗口大小5秒，滑动步长2秒）
    kos_window_config_t window_config = {
        .type = KOS_WINDOW_SLIDING,
        .window_size_ms = 5000,    // 5秒窗口
        .slide_size_ms = 2000,      // 2秒滑动
        .session_gap_ms = 0,
        .time_field = "timestamp"
    };
    
    if (kos_stream_add_window(pipeline, &window_config) != 0) {
        fprintf(stderr, "Failed to add window\n");
        kos_stream_pipeline_free(pipeline);
        kos_term_free(input_type);
        kos_term_free(output_type);
        return 1;
    }
    
    // 4. 配置聚合操作（求和）
    kos_aggregation_config_t agg_config = {
        .field_name = "value",
        .op = KOS_AGG_SUM
    };
    
    if (kos_stream_add_aggregation(pipeline, &agg_config) != 0) {
        fprintf(stderr, "Failed to add aggregation\n");
        kos_stream_pipeline_free(pipeline);
        kos_term_free(input_type);
        kos_term_free(output_type);
        return 1;
    }
    
    // 5. 配置水位线
    kos_watermark_config_t watermark_config = {
        .max_lateness_ms = 2000,       // 2秒延迟容忍
        .watermark_interval_ms = 1000,  // 1秒更新间隔
        .current_watermark_ms = 0
    };
    
    kos_stream_set_watermark_config(pipeline, &watermark_config);
    
    // 6. 配置背压阈值
    kos_stream_set_backpressure_threshold(pipeline, 1000);
    
    // 7. 模拟流式事件输入
    printf("=== Processing Stream Events ===\n");
    
    int64_t base_time = kos_get_processing_time_ms();
    
    // 生成10个事件，每个间隔500ms
    for (int i = 0; i < 10; i++) {
        int64_t event_time = base_time + i * 500;
        double value = 10.0 + i * 2.5;
        char event_id[32];
        snprintf(event_id, sizeof(event_id), "event_%d", i);
        
        kos_term* event = create_sample_event(event_id, value, event_time);
        if (!event) {
            fprintf(stderr, "Failed to create event %d\n", i);
            continue;
        }
        
        printf("Processing event %d: time=%lld ms, value=%.2f\n", 
               i, (long long)event_time, value);
        
        int result = kos_stream_process_event(pipeline, event, event_time);
        if (result == -2) {
            printf("  -> Backpressure: event rejected\n");
        } else if (result != 0) {
            printf("  -> Error: failed to process event\n");
        }
        
        kos_term_free(event);
        
        // 模拟处理延迟
        // Sleep for 100ms (simulated)
        #ifdef _WIN32
        Sleep(100);
        #else
        struct timespec ts = {0, 100000000}; // 100ms
        nanosleep(&ts, NULL);
        #endif
    }
    
    printf("\n=== Triggering Windows ===\n");
    
    // 8. 触发窗口（使用当前时间）
    int64_t current_time = kos_get_processing_time_ms();
    printf("Current time: %lld ms\n", (long long)current_time);
    printf("Current watermark: %lld ms\n", 
           (long long)kos_stream_get_current_watermark(pipeline));
    
    // 触发窗口多次（模拟时间推进）
    for (int i = 0; i < 5; i++) {
        int64_t trigger_time = current_time + i * 2000; // 每2秒触发一次
        kos_window_result_t* window_result = kos_stream_trigger_window(pipeline, trigger_time);
        
        if (window_result) {
            printf("\nWindow triggered at %lld ms:\n", (long long)trigger_time);
            print_window_result(window_result);
            kos_window_result_free(window_result);
        } else {
            printf("No window triggered at %lld ms\n", (long long)trigger_time);
        }
    }
    
    // 9. 获取聚合结果
    printf("=== Aggregation Results ===\n");
    size_t agg_count = 0;
    kos_aggregation_result_t* agg_results = kos_stream_get_aggregation_results(
        pipeline, 
        &agg_count
    );
    
    if (agg_results) {
        for (size_t i = 0; i < agg_count; i++) {
            printf("Aggregation %zu: field=%s, op=%d, count=%zu\n",
                   i,
                   agg_results[i].field_name,
                   agg_results[i].op,
                   agg_results[i].count);
            
            if (agg_results[i].result) {
                kos_serialized* serialized = kos_term_serialize(agg_results[i].result);
                if (serialized && serialized->data) {
                    printf("  Result: %s\n", serialized->data);
                    kos_serialized_free(serialized);
                }
            }
        }
        
        // 释放聚合结果
        for (size_t i = 0; i < agg_count; i++) {
            kos_aggregation_result_free(&agg_results[i]);
        }
        free(agg_results);
    }
    
    // 10. 检查背压状态
    printf("\n=== Backpressure State ===\n");
    kos_backpressure_state_t bp_state = kos_stream_get_backpressure_state(pipeline);
    printf("Queue size: %zu / %zu\n", bp_state.queue_size, bp_state.max_queue_size);
    printf("Backpressure ratio: %.2f\n", bp_state.backpressure_ratio);
    printf("Is backpressure: %s\n", bp_state.is_backpressure ? "Yes" : "No");
    
    // 11. 清理资源
    kos_stream_pipeline_free(pipeline);
    kos_term_free(input_type);
    kos_term_free(output_type);
    
    printf("\n=== Demo Completed ===\n");
    
    return 0;
}
