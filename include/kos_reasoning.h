// include/kos_reasoning.h
// 实时推理会话：按本体维度的推理状态、事件喂入与单步推进

#ifndef KOS_REASONING_H
#define KOS_REASONING_H

#include "kos_ontology_registry.h"
#include "kos_kernel.h"
#include "kos_runtime.h"
#include <stdbool.h>
#include <stddef.h>

typedef struct kos_reasoning_session kos_reasoning_session_t;

// 创建推理会话（绑定注册表；会话不拥有 registry）
kos_reasoning_session_t* kos_reasoning_session_create(kos_ontology_registry_t* registry);

void kos_reasoning_session_free(kos_reasoning_session_t* session);

// 获取或创建该 ontology_id 的 kernel 状态；initial_K 可为 NULL
kos_state_t* kos_reasoning_get_or_create_state(kos_reasoning_session_t* session,
                                                const char* ontology_id,
                                                kos_term* initial_K);

// 喂入原始信号：elab(raw_signal, state->K) 后 enqueue；返回 0 成功
int kos_reasoning_feed_event(kos_reasoning_session_t* session,
                             const char* ontology_id,
                             bitstream raw_signal);

// 对该 ontology_id 执行一次演化（处理队列头一个事件）
bool kos_reasoning_tick(kos_reasoning_session_t* session, const char* ontology_id);

// 对该 ontology_id 执行多步演化直到队列空或达到 max_steps
size_t kos_reasoning_run_until_idle(kos_reasoning_session_t* session,
                                     const char* ontology_id,
                                     size_t max_steps);

#endif /* KOS_REASONING_H */
