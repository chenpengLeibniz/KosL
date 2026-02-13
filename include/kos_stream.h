// Phase 4: Real-time Stream Processing
// 实时流处理框架 - 对标 Palantir 流式数据处理能力
#ifndef KOS_STREAM_H
#define KOS_STREAM_H

#include "kos_core.h"
#include "kos_kernel.h"
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

// ========== 窗口类型 ==========
typedef enum {
    KOS_WINDOW_SLIDING,    // 滑动窗口
    KOS_WINDOW_TUMBLING,   // 滚动窗口
    KOS_WINDOW_SESSION     // 会话窗口
} kos_window_type_t;

// ========== 聚合操作类型 ==========
typedef enum {
    KOS_AGG_SUM,           // 求和
    KOS_AGG_AVG,           // 平均值
    KOS_AGG_COUNT,         // 计数
    KOS_AGG_MAX,           // 最大值
    KOS_AGG_MIN,           // 最小值
    KOS_AGG_FIRST,         // 第一个值
    KOS_AGG_LAST           // 最后一个值
} kos_aggregation_op_t;

// ========== 时间类型 ==========
typedef enum {
    KOS_TIME_EVENT_TIME,   // 事件时间（基于事件本身的时间戳）
    KOS_TIME_PROCESSING_TIME // 处理时间（基于系统处理时间）
} kos_time_type_t;

// ========== 窗口配置 ==========
typedef struct {
    kos_window_type_t type;        // 窗口类型
    int64_t window_size_ms;        // 窗口大小（毫秒）
    int64_t slide_size_ms;         // 滑动步长（毫秒，仅用于滑动窗口）
    int64_t session_gap_ms;        // 会话间隔（毫秒，仅用于会话窗口）
    const char* time_field;        // 时间字段名（用于提取事件时间）
} kos_window_config_t;

// ========== 聚合配置 ==========
typedef struct {
    const char* field_name;        // 聚合字段名
    kos_aggregation_op_t op;       // 聚合操作
} kos_aggregation_config_t;

// ========== 水位线配置 ==========
typedef struct {
    int64_t max_lateness_ms;       // 最大延迟容忍度（毫秒）
    int64_t watermark_interval_ms; // 水位线更新间隔（毫秒）
    int64_t current_watermark_ms;  // 当前水位线时间戳（毫秒）
} kos_watermark_config_t;

// ========== 流处理管道 ==========
typedef struct kos_stream_pipeline kos_stream_pipeline_t;

// ========== 窗口状态 ==========
typedef struct kos_window_state kos_window_state_t;

// ========== 聚合状态 ==========
typedef struct kos_aggregation_state kos_aggregation_state_t;

// ========== 流处理事件 ==========
typedef struct {
    kos_term* data;                // 事件数据（kos_term）
    int64_t event_time_ms;         // 事件时间戳（毫秒）
    int64_t processing_time_ms;    // 处理时间戳（毫秒）
} kos_stream_event_t;

// ========== 窗口结果 ==========
typedef struct kos_window_result {
    kos_stream_event_t* events;     // 窗口内的事件数组
    size_t event_count;             // 事件数量
    int64_t window_start_ms;        // 窗口开始时间
    int64_t window_end_ms;          // 窗口结束时间
    kos_aggregation_result_t* aggregations; // 聚合结果数组
    size_t aggregation_count;        // 聚合结果数量
    struct kos_window_result* next; // 下一个窗口结果（用于多个窗口）
} kos_window_result_t;

// ========== 聚合结果 ==========
typedef struct {
    const char* field_name;         // 聚合字段名
    kos_aggregation_op_t op;        // 聚合操作
    kos_term* result;               // 聚合结果（kos_term）
    size_t count;                   // 参与聚合的事件数
} kos_aggregation_result_t;

// ========== 背压状态 ==========
typedef struct {
    size_t queue_size;              // 当前队列大小
    size_t max_queue_size;          // 最大队列大小
    bool is_backpressure;           // 是否处于背压状态
    float backpressure_ratio;       // 背压比例 (0.0-1.0)
} kos_backpressure_state_t;

// ========== 核心 API ==========

// --- 流处理管道创建与销毁 ---
kos_stream_pipeline_t* kos_create_stream_pipeline(
    kos_term* input_type,
    kos_term* output_type,
    kos_time_type_t time_type
);

void kos_stream_pipeline_free(kos_stream_pipeline_t* pipeline);

// --- 窗口操作 ---
int kos_stream_add_window(
    kos_stream_pipeline_t* pipeline,
    const kos_window_config_t* config
);

// --- 聚合操作 ---
int kos_stream_add_aggregation(
    kos_stream_pipeline_t* pipeline,
    const kos_aggregation_config_t* config
);

// --- 事件处理 ---
int kos_stream_process_event(
    kos_stream_pipeline_t* pipeline,
    kos_term* event_data,
    int64_t event_time_ms
);

// --- 窗口触发与结果获取 ---
kos_window_result_t* kos_stream_trigger_window(
    kos_stream_pipeline_t* pipeline,
    int64_t current_time_ms
);

void kos_window_result_free(kos_window_result_t* result);

// --- 聚合结果获取 ---
kos_aggregation_result_t* kos_stream_get_aggregation_results(
    kos_stream_pipeline_t* pipeline,
    size_t* result_count
);

void kos_aggregation_result_free(kos_aggregation_result_t* result);

// --- 水位线管理 ---
int kos_stream_set_watermark_config(
    kos_stream_pipeline_t* pipeline,
    const kos_watermark_config_t* config
);

int64_t kos_stream_get_current_watermark(
    kos_stream_pipeline_t* pipeline
);

int kos_stream_update_watermark(
    kos_stream_pipeline_t* pipeline,
    int64_t event_time_ms
);

// --- 背压处理 ---
int kos_stream_set_backpressure_threshold(
    kos_stream_pipeline_t* pipeline,
    size_t max_queue_size
);

kos_backpressure_state_t kos_stream_get_backpressure_state(
    kos_stream_pipeline_t* pipeline
);

bool kos_stream_should_backpressure(
    kos_stream_pipeline_t* pipeline
);

// --- 流处理管道执行 ---
int kos_stream_execute(
    kos_stream_pipeline_t* pipeline,
    kos_term* output_buffer,
    size_t* output_count
);

// --- 辅助函数：从 kos_term 提取时间戳 ---
int64_t kos_extract_event_time(
    kos_term* event,
    const char* time_field
);

// --- 辅助函数：获取当前处理时间 ---
int64_t kos_get_processing_time_ms(void);

#endif // KOS_STREAM_H
