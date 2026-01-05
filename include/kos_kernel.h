#ifndef KOS_KERNEL_H
#define KOS_KERNEL_H

#include "kos_core.h" // 必须包含 L0
#include <stddef.h>

// ========== L1: Kernel Layer - 动态迁移层 ==========
// 定义"如何改变" - 确定性状态演化

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

// 系统状态结构
// 维护当前知识集K和逻辑时钟TS
typedef struct {
    kos_term* K;    // 经证实的知识集（本体）
    int TS;         // 逻辑时钟 [cite: 159]
    queue_t* P;     // 待处理事件队列 [cite: 159]
} kos_state_t;

// 类型别名，保持兼容性
typedef kos_state_t kos_state;

// ========== L1 核心接口 ==========
// STEP 算子：确定性状态演化
// 从旧状态 σ 到新状态 σ' 的单调演化
// 前提：事件必须通过类型检查（合规验证）
bool kos_step(kos_state_t* sigma);
bool kos_kernel_step(kos_state_t* sigma, kos_term* event_pair);

// 验证事件的前置条件（Pre(e)）
// 验证事件证明是否满足前置条件
bool kos_verify_precondition(kos_term* event_pair, kos_state_t* sigma);

// 更新知识集（添加新证明的事实）
kos_term* kos_update_knowledge(kos_term* K, kos_term* new_fact);

#endif