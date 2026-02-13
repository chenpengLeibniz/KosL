#ifndef KOS_RUNTIME_H
#define KOS_RUNTIME_H

#include "kos_kernel.h" // 必须包含 L1
#include "kos_kernel_session.h"
#include "kos_trace.h"   // MTK: Event Log 与确定性重放
#include <stddef.h>
#include <stdbool.h>
#include <stdint.h>

// ========== L2: Runtime Layer - 环境演化层 ==========
// 定义"如何运行" - 外部I/O、物理存储映射

// 原始信号流类型（字节数组）
typedef struct {
    unsigned char* data;
    size_t length;
} bitstream;

// ========== Elaboration Engine (Elaborator) ==========
// 连接物理 I/O 设备，监控外部中断和传感器数据流

// 信号源类型
typedef enum {
    RUNTIME_SOURCE_STDIN,      // 标准输入
    RUNTIME_SOURCE_FILE,       // 文件
    RUNTIME_SOURCE_SOCKET,     // 网络套接字
    RUNTIME_SOURCE_SENSOR,     // 传感器设备
    RUNTIME_SOURCE_CUSTOM      // 自定义源
} runtime_signal_source;

// 信号源描述
typedef struct {
    runtime_signal_source source_type;
    void* source_handle;       // 源句柄（文件指针、套接字等）
    const char* source_name;   // 源名称（用于日志）
} signal_source_t;

// 精化上下文（用于轨迹记录和重放）
typedef struct {
    kos_term* event_pair;      // 精化后的事件对 <e, p>
    bitstream original_signal;  // 原始信号（用于重放）
    int logical_clock;         // 精化时的逻辑时钟
    size_t signal_index;       // 信号索引（用于排序）
} elaboration_record_t;

// ========== Physical Storage Manager ==========
// 抽象底层媒体差异，管理数据库和内存映射

// 存储后端类型
typedef enum {
    STORAGE_BACKEND_MEMORY,    // 内存存储（测试用）
    STORAGE_BACKEND_FILE,      // 文件存储
    STORAGE_BACKEND_DATABASE,  // 数据库存储（SQLite等）
    STORAGE_BACKEND_CUSTOM     // 自定义后端
} storage_backend_type;

// 存储后端接口
typedef struct storage_backend {
    storage_backend_type type;
    void* backend_handle;      // 后端句柄
    
    // 后端操作接口
    int (*write)(struct storage_backend* backend, const kos_term* fact, int logical_clock);
    int (*read)(struct storage_backend* backend, int logical_clock, kos_term** fact);
    int (*commit)(struct storage_backend* backend);  // 原子提交
    int (*rollback)(struct storage_backend* backend); // 回滚
    void (*free)(struct storage_backend* backend);
} storage_backend_t;

// 物化状态（用于原子提交栅栏）
typedef enum {
    MATERIALIZE_PREPARE,       // 准备阶段
    MATERIALIZE_COMMIT,        // 提交阶段
    MATERIALIZE_ROLLBACK       // 回滚阶段
} materialize_phase_t;

// ========== Scheduler Relay ==========
// 作为 Kernel 顺序演化的缓冲区，负责多线程信号的并发接收和有序排队

// 信号缓冲区（用于并发接收）
typedef struct {
    bitstream* signals;        // 信号数组
    size_t capacity;           // 容量
    size_t count;              // 当前数量
    size_t head;               // 头部索引（用于循环缓冲区）
    size_t tail;               // 尾部索引
} signal_buffer_t;

// ========== L2 核心接口 ==========

// --- Elaboration Engine ---
// elab 算子：将物理比特流映射为带逻辑证明的事件对象 <e, p>
// 输入：原始信号 + 本体（知识集）
// 输出：Σ-Type事件对，若无法构造证明则返回NULL（逻辑防火墙）
kos_term* kos_elab(bitstream raw_signal, kos_term* ontology);

// elab 带错误输出：成功时 event_out 非 NULL，失败时 errmsg 含错误提示
bool kos_elab_ex(bitstream raw_signal, kos_term* ontology,
                 kos_term** event_out, char* errmsg, size_t errmsg_size);

// 从 .kos 源字符串精化：信号为 .kos 表达式时，经 kos-core 形式化校验后转为事件对
// 支持：1) 纯 .kos 字符串（UTF-8）；2) JSON 对象 {"_kos": "<.kos expr>"}
// 返回 <event_term, proof> 对，失败返回 NULL
kos_term* kos_elab_from_kos(bitstream raw_signal);

// 批量精化：处理多个信号
// 返回成功精化的事件对数组（调用者负责释放）
kos_term** kos_elab_batch(bitstream* signals, size_t count, kos_term* ontology, size_t* success_count);

// 记录精化轨迹（用于重放和自愈）
elaboration_record_t* kos_elab_record(kos_term* event_pair, bitstream original_signal, int logical_clock);

// 释放精化记录
void kos_elab_record_free(elaboration_record_t* record);

// --- 信号处理与自动溯源 ---
// 当外部信号发生时：能构成合法事件则自动溯源找根因，否则返回错误提示

typedef struct {
    bool success;                    // 是否成功构成合法事件
    kos_term* event_pair;           // 成功时的事件对
    void* root_cause_report;        // 成功且为 FailEvt 时的根因报告 (RootCauseReport*)
    char errmsg[512];               // 失败时的错误提示
} kos_signal_process_result_t;

// 释放 result 内资源（event_pair、root_cause_report）
void kos_signal_process_result_free(kos_signal_process_result_t* result);

// 处理外部信号：尝试精化为合法事件，成功则自动溯源，失败则返回错误
int kos_runtime_process_signal(bitstream signal, kos_term* ontology, kos_state_t* sigma,
                              kos_signal_process_result_t* result);

/* ========== Runtime 优化：精化 + Γ 动态扩展 + 事件入队 ==========
 * 通过 kos-core infer-term 精化信号，并根据结果更新 Γ；
 * 若精化得到事件对 <e,p>，则自动入队到 σ 的 P。 */
typedef struct {
    bool success;
    bool is_event;
    size_t gamma_added;            // 新增类型/谓词数量
    kos_term* refined_term;        // 非事件时的精化结果
    kos_term* event_pair;          // 事件对（若 is_event）
    kos_term* inferred_type;       // kos-core 推导出的类型
    char errmsg[512];
} kos_runtime_refine_result_t;

void kos_runtime_refine_result_free(kos_runtime_refine_result_t* result);

int kos_runtime_refine_signal(bitstream signal, kos_kernel_session_t* session,
                              kos_runtime_refine_result_t* result);

/* 从外部数据库拉取并精化（简化：query 返回 .kos 表达式） */
int kos_runtime_refine_from_db(const char* query, kos_kernel_session_t* session,
                               kos_runtime_refine_result_t* result);

// 溯源处理器：尝试从事件中提取根因报告，返回 RootCauseReport* 或 NULL
typedef void* (*kos_traceability_handler_t)(const kos_term* event, kos_state_t* sigma);

// 根因报告释放函数（由注册方提供）
typedef void (*kos_root_cause_free_t)(void* report);

// 注册溯源处理器（制造业等领域可注册，用于自动溯源；free_func 用于释放报告）
void kos_runtime_register_traceability_handler(kos_traceability_handler_t handler,
                                                kos_root_cause_free_t free_func);

// --- Physical Storage Manager ---
// M 算子：将抽象逻辑结论下沉为物理动作
// 实现原子提交栅栏（Atomic Commit Fence）
// 只有逻辑演化成功后，才执行物理存储写入
int kos_materialize(kos_state_t* sigma, storage_backend_t* backend);

// 创建存储后端
storage_backend_t* kos_storage_create(storage_backend_type type, const char* config);

// 释放存储后端
void kos_storage_free(storage_backend_t* backend);

// 物化单个事实（两阶段提交）
int kos_materialize_fact(storage_backend_t* backend, const kos_term* fact, int logical_clock);

// --- Scheduler Relay ---
// 创建信号缓冲区
signal_buffer_t* kos_signal_buffer_create(size_t capacity);

// 释放信号缓冲区
void kos_signal_buffer_free(signal_buffer_t* buffer);

// 添加信号到缓冲区（并发安全）
int kos_signal_buffer_add(signal_buffer_t* buffer, bitstream signal);

// 从缓冲区取出信号（有序）
bitstream kos_signal_buffer_take(signal_buffer_t* buffer);

// 检查缓冲区是否为空
bool kos_signal_buffer_is_empty(signal_buffer_t* buffer);

// 获取缓冲区大小
size_t kos_signal_buffer_size(signal_buffer_t* buffer);

// --- 系统初始化和I/O ---
// 初始化系统（创建初始本体）
kos_state_t* kos_runtime_init(kos_term* initial_ontology);

// 释放运行时系统
void kos_runtime_free(kos_state_t* sigma);

// 捕获物理信号（从标准输入或API）
bitstream kos_capture_physical_signal(signal_source_t* source);

// 创建信号源
signal_source_t* kos_signal_source_create(runtime_signal_source type, void* handle, const char* name);

// 释放信号源
void kos_signal_source_free(signal_source_t* source);

// --- 数据库接口（物理存储）---
int kos_db_init(const char* db_path);
int kos_db_close(void);

// --- 轨迹重放和自愈 ---
// 重放精化轨迹（用于灾后自愈，信号级）
int kos_replay_elaboration_trajectory(elaboration_record_t* records, size_t count,
                                      kos_state_t* sigma, storage_backend_t* backend);

// 保存精化轨迹到文件
int kos_save_elaboration_trajectory(elaboration_record_t* records, size_t count, const char* filename);

// 从文件加载精化轨迹
elaboration_record_t* kos_load_elaboration_trajectory(const char* filename, size_t* count);

// --- MTK: 从 Event Log 确定性重放与崩溃恢复 ---
// 从 Trace（Event Log）重放得到 σ；同一 trace + 同一 initial_K ⇒ 同一 σ。调用者负责 kos_state_free。
kos_state_t* kos_replay_from_trace(const kos_trace_t* trace, kos_term* initial_K);

// 从持久化的 Event Log 文件重放，得到一致状态（崩溃恢复）。optional_initial_K 可为 NULL 表示空初始 K。
// 若 expected_hash 非 0，重放后验证 state_hash == expected_hash；验证失败返回 NULL 且 *verify_ok 为 false。
// 调用者负责 kos_state_free 返回的 sigma；verify_ok 可为 NULL。
kos_state_t* kos_replay_from_trace_file(const char* trace_filename, kos_term* optional_initial_K,
                                        uint64_t expected_hash, bool* verify_ok);

#endif