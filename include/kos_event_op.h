/**
 * @file kos_event_op.h
 * @brief KOS Kernel 层事件-操作栈映射（小步操作语义）
 *
 * 以事件为核心：某一类事件对应到一个可执行序列的栈，
 * 按照小步操作语义依次从栈中获取对应的函数（操作），直到结束为止。
 *
 * 对应 Kos.pdf 2.2.3 小步操作语义：
 *   e = head(Σ.P) → Verify(Pre) → Op(Args, Σ) → Confirm(Post)
 * 其中 Op 可分解为多个小步操作。
 */

#ifndef KOS_EVENT_OP_H
#define KOS_EVENT_OP_H

#include "kos_kernel.h"
#include "kos_core.h"
#include <stddef.h>
#include <stdbool.h>

/* ========== 单步操作结果 ========== */

typedef enum {
    KOS_OP_DONE,     /* 本步完成，进入下一步 */
    KOS_OP_CONTINUE, /* 本步未完成，需再次调用（如内部归约） */
    KOS_OP_FAIL      /* 失败，终止该事件的执行 */
} kos_op_result_t;

/* ========== 单步操作函数类型 ==========
 * 每个操作接收 (ctx, event_pair)，执行一小步，返回结果。
 * ctx 提供 sigma、registry 及 op 间传递的中间状态。 */

struct kos_small_step_ctx;  /* 前向声明 */
typedef kos_op_result_t (*kos_event_op_fn)(struct kos_small_step_ctx* ctx, kos_term* event_pair);

/* ========== 事件类型解析器 ==========
 * 从事件对中提取类型标识，用于查找对应的操作栈。
 * 默认可根据 e 的 kind 或 atomic.val 等生成。 */

typedef const char* (*kos_event_type_resolver_fn)(kos_term* event_pair);

/* ========== 操作栈注册表 ========== */

#define KOS_EVENT_OP_STACK_MAX 16

typedef struct kos_event_op_entry {
    const char* event_type_id;
    kos_event_op_fn ops[KOS_EVENT_OP_STACK_MAX];
    size_t op_count;
} kos_event_op_entry_t;

typedef struct kos_event_op_registry {
    kos_event_op_entry_t* entries;
    size_t capacity;
    size_t count;
    kos_event_type_resolver_fn type_resolver;
} kos_event_op_registry_t;

/* ========== 注册表 API ========== */

/** 创建空注册表 */
kos_event_op_registry_t* kos_event_op_registry_create(void);

/** 释放注册表 */
void kos_event_op_registry_free(kos_event_op_registry_t* reg);

/** 注册事件类型对应的操作栈
 * @param reg 注册表
 * @param event_type_id 事件类型标识（如 "SensorEvent", "RootCauseEvent"）
 * @param ops 操作函数数组
 * @param op_count 操作数量（不超过 KOS_EVENT_OP_STACK_MAX）
 * @return 0 成功，-1 失败 */
int kos_event_op_register(kos_event_op_registry_t* reg,
                           const char* event_type_id,
                           kos_event_op_fn* ops, size_t op_count);

/** 设置默认类型解析器（当未注册时使用） */
void kos_event_op_set_resolver(kos_event_op_registry_t* reg,
                              kos_event_type_resolver_fn resolver);

/** 查找事件类型对应的操作栈 */
kos_event_op_entry_t* kos_event_op_lookup(kos_event_op_registry_t* reg,
                                          const char* event_type_id);

/* ========== 小步执行上下文 ==========
 * 维护当前正在处理的事件及其操作栈进度。 */

typedef struct kos_small_step_ctx {
    kos_state_t* sigma;
    kos_event_op_registry_t* registry;
    kos_term* current_event;   /* 当前处理中的事件对（不拥有，来自队列 peek） */
    kos_event_op_fn* op_stack; /* 当前事件的操作栈副本 */
    size_t op_count;
    size_t op_index;          /* 当前执行到的操作下标 */
    kos_term* op_intermediate; /* op 间传递的中间结果（如 reduce 的输出） */
} kos_small_step_ctx_t;

/** 创建小步执行上下文 */
kos_small_step_ctx_t* kos_small_step_ctx_create(kos_state_t* sigma,
                                                 kos_event_op_registry_t* registry);

/** 释放小步执行上下文（不释放 sigma 和 registry） */
void kos_small_step_ctx_free(kos_small_step_ctx_t* ctx);

/** 执行一小步：从操作栈取一个操作执行，直到本步完成或失败
 * @return true 表示有进展（可能完成一个操作或整个事件），false 表示无事件或失败 */
bool kos_step_small(kos_small_step_ctx_t* ctx);

/** 持续小步执行直到队列为空且无进行中的事件 */
void kos_evolution_cycle_small(kos_small_step_ctx_t* ctx);

/* ========== 内置操作（可用于注册） ==========
 * 对应 Peek-Verify-Reduce-Update-Confirm 流程的分解。 */

/** 操作1：验证前置条件 Pre(e) */
kos_op_result_t kos_op_verify_pre(kos_small_step_ctx_t* ctx, kos_term* event_pair);

/** 操作2：归约 Reduce(e)，结果存入 ctx->op_intermediate */
kos_op_result_t kos_op_reduce(kos_small_step_ctx_t* ctx, kos_term* event_pair);

/** 操作3：更新知识集与逻辑时钟 Update(K, TS++)，使用 ctx->op_intermediate */
kos_op_result_t kos_op_update_knowledge(kos_small_step_ctx_t* ctx, kos_term* event_pair);

/** 操作4：验证后置条件 Post(e) 并完成 */
kos_op_result_t kos_op_confirm_post(kos_small_step_ctx_t* ctx, kos_term* event_pair);

#endif /* KOS_EVENT_OP_H */
