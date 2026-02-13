/**
 * @file kos_trace.h
 * @brief KOS Kernel 层轨迹（Trace）：小步演化序列，MTK Event Log
 *
 * 对应 monograph 第 8 章「核心层：证明构造与小步演化」中的轨迹概念：
 *   T = σ₀ →^⟨e₁,p₁⟩ σ₁ →^⟨e₂,p₂⟩ σ₂ → …
 * 即状态在事件对驱动下的演化序列；MTK 中 Trace = List<Event>，本结构为不可变追加的 Event Log。
 */

#ifndef KOS_TRACE_H
#define KOS_TRACE_H

#include "kos_core.h"
#include <stddef.h>
#include <stdbool.h>
#include <stdint.h>

/* 前向声明：状态 σ */
struct kos_state;

/* ========== 单步记录 ========== */

typedef struct kos_trace_step {
    size_t step_index;      /* 步序号（从 0 起） */
    int ts_after;           /* 本步执行后的逻辑时钟 TS */
    kos_term* event_pair;   /* 本步驱动的事件对 ⟨e, p⟩（轨迹持有，深拷贝） */
    kos_term* K_snapshot;   /* 可选：本步后的知识集 K 快照，NULL 表示不保存以省内存 */
} kos_trace_step_t;

/* ========== 轨迹 ========== */

typedef struct kos_trace {
    kos_trace_step_t* steps;
    size_t count;
    size_t capacity;
    bool store_K_snapshot;  /* 是否在每步保存 K 快照 */
} kos_trace_t;

/* ========== API ========== */

/** 创建空轨迹。store_K 为 true 时每步追加时保存 K 快照（占用更多内存） */
kos_trace_t* kos_trace_create(bool store_K);

/** 释放轨迹及所有步骤中的 event_pair/K_snapshot */
void kos_trace_free(kos_trace_t* trace);

/** 追加一步：记录演化后的状态与驱动事件对。event_pair 会被深拷贝。 */
int kos_trace_append(kos_trace_t* trace, const struct kos_state* sigma_after, kos_term* event_pair);

/** 轨迹长度（步数） */
size_t kos_trace_length(const kos_trace_t* trace);

/** 获取第 i 步（只读）。i >= length 返回 NULL */
const kos_trace_step_t* kos_trace_get_step(const kos_trace_t* trace, size_t i);

/* ========== MTK: Deterministic Replay ========== */
/** 从 Event Log（trace）确定性重放：σ₀ + [e₁..eₙ] → σ。同一 trace + 同一 initial_K 必得同一 σ。调用者负责 kos_state_free 返回值。 */
struct kos_state* kos_trace_replay(const kos_trace_t* trace, kos_term* initial_K);

/** 从 trace 折叠计算哈希（与 kos_state 的 state_hash 一致）。用于重放后验证。 */
uint64_t kos_trace_fold_hash(const kos_trace_t* trace);

/** 将 Event Log 保存到文件（MTK 崩溃恢复用）。返回 0 成功，-1 失败。 */
int kos_trace_save_to_file(const kos_trace_t* trace, const char* filename);

/** 从文件加载 Event Log。调用者负责 kos_trace_free。失败返回 NULL。 */
kos_trace_t* kos_trace_load_from_file(const char* filename);

#endif /* KOS_TRACE_H */
