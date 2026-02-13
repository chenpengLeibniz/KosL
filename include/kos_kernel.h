#ifndef KOS_KERNEL_H
#define KOS_KERNEL_H

#include "kos_core.h" // 必须包含 L0
#include <stddef.h>
#include <stdint.h>

// ========== L1: Kernel Layer - 动态迁移层 ==========
// 定义"如何改变" - 确定性状态演化
//
// 核心数据结构（对应 monograph 第 8 章「核心层：证明构造与小步演化」）：
//   Γ (Gamma)：类型与谓词集合，kos_kernel_context_t，保存所有类型、谓词、构造子
//   σ (sigma)：知识状态 kos_state_t = 〈K, TS, P〉
//     - K: 经证实的知识集（类型实例/叶子项），Σ 链形式
//     - TS: 逻辑时钟
//     - P: 待处理事件队列
//   trace：σ 的演化轨迹 T = σ₀ →^⟨e₁,p₁⟩ σ₁ → …，kos_trace_t
//
// 统一会话：kos_kernel_session_t（见 kos_kernel_session.h）整合 Γ、σ、trace，
// 提供事件喂入、小步演化、根因查找（调用 kos-core prove）及轨迹跟踪。
//
// 小步操作：以事件为核心，Peek → Verify(Pre) → Reduce → Update(K, TS) → Confirm(Post)

// 简单队列结构（待处理事件队列）
typedef struct queue_node {
    kos_term* data;
    struct queue_node* next;
} queue_node;

typedef struct {
    queue_node* front;
    queue_node* rear;
    size_t size;
} queue_t;

// 前向声明：知识库（可选，用于依赖项管理与可视化）
struct kos_knowledge_base;

// 前向声明：轨迹（可选，用于记录小步演化序列）
struct kos_trace;

/** σ（sigma）：知识库状态，对应书中 σ = 〈K, TS, P〉；MTK 对齐：store→K, version→state_hash */
typedef struct kos_state {
    kos_term* K;    // 经证实的知识集（事实集合），Σ 链形式
    int TS;         // 逻辑时钟 [cite: 159]
    queue_t* P;     // 待处理事件队列 [cite: 159]
    struct kos_knowledge_base* KB;  // 可选：知识库，支持依赖图与可视化
    struct kos_trace* trace;       // 可选：Event Log（不可变追加），每步成功自动追加
    uint64_t state_hash;           // MTK: version/hash，hash(σ)=fold(trace)，可验证一致性
} kos_state_t;

// 类型别名：σ 与书中符号一致
typedef kos_state_t kos_sigma_t;

// 保持兼容性
typedef kos_state_t kos_state;

// ========== L1 核心接口 ==========

// --- 状态管理 ---
// 创建初始状态 σ = 〈K, TS, P〉
kos_state_t* kos_state_create(kos_term* initial_K);

// 释放状态（递归释放所有资源）
void kos_state_free(kos_state_t* sigma);

// --- Event Queue Manager ---
// 初始化事件队列
queue_t* kos_queue_create(void);

// 释放事件队列
void kos_queue_free(queue_t* queue);

// 入队：添加事件对 〈e, p〉 到队列（严格顺序）
// 返回 0 成功，-1 失败
int kos_queue_enqueue(queue_t* queue, kos_term* event_pair);

// 出队：从队列头部取出事件对
// 返回事件对，队列为空返回 NULL
kos_term* kos_queue_dequeue(queue_t* queue);

// 查看队列头部（不取出）
kos_term* kos_queue_peek(queue_t* queue);

// 检查队列是否为空
bool kos_queue_is_empty(queue_t* queue);

// 获取队列大小
size_t kos_queue_size(queue_t* queue);

// --- Evolution Scheduler ---
// STEP 算子：确定性状态演化
// 从旧状态 σ 到新状态 σ' 的单调演化
// 前提：事件必须通过类型检查（合规验证）
bool kos_step(kos_state_t* sigma);

// 直接对给定事件对执行演化
bool kos_kernel_step(kos_state_t* sigma, kos_term* event_pair);

// Peek-Verify-Reduce-Confirm 流程的完整实现
// 1. Peek: 查看队列头部事件
// 2. Verify: 验证前置条件 Pre(e)
// 3. Reduce: 执行归约操作
// 4. Confirm: 验证后置条件 Post(e) 并更新状态
bool kos_evolution_cycle(kos_state_t* sigma);

// --- 前置/后置条件验证 ---
// 验证事件的前置条件（Pre(e)）
// 验证事件证明是否满足前置条件
bool kos_verify_precondition(kos_term* event_pair, kos_state_t* sigma);

// 验证事件的后置条件（Post(e)）
// 在状态更新后，验证新状态是否满足后置条件
bool kos_verify_postcondition(kos_term* event_pair, kos_state_t* old_sigma, kos_state_t* new_sigma);

// --- 知识集更新 ---
// 更新知识集（添加新证明的事实）
// 使用 Σ 链结构：K' = Σ(new_fact, K)
kos_term* kos_update_knowledge(kos_term* K, kos_term* new_fact);

// --- State Mirror ---
// 获取当前知识集 K（只读视图）
const kos_term* kos_state_get_K(const kos_state_t* sigma);

// 获取当前逻辑时钟 TS
int kos_state_get_TS(const kos_state_t* sigma);

// 获取当前事件队列大小
size_t kos_state_get_queue_size(const kos_state_t* sigma);

// 检查状态是否为空（K 为空且队列为空）
bool kos_state_is_empty(const kos_state_t* sigma);

// --- 知识库 (Knowledge Base) ---
// 设置/获取可选知识库（用于依赖项管理与可视化）
void kos_state_set_kb(kos_state_t* sigma, void* kb);
void* kos_state_get_kb(const kos_state_t* sigma);

// --- 轨迹 (Trace) ---
// 设置/获取可选轨迹；若设置，kos_kernel_step 成功后会自动追加一步
void kos_state_set_trace(kos_state_t* sigma, void* trace);
void* kos_state_get_trace(const kos_state_t* sigma);

// --- MTK: State Hash (version) ---
// 获取当前状态哈希（由 Trace 折叠计算，满足 hash(σ)=fold(trace)）
uint64_t kos_state_get_hash(const kos_state_t* sigma);
// 内部用：单步成功后按 event_pair 更新 state_hash
void kos_state_update_hash_after_step(kos_state_t* sigma, kos_term* event_pair);

#endif