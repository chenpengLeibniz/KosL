/**
 * @file state_step.c
 * @brief KOS Kernel 层状态管理与迁移算子 (State & STEP)
 *
 * 实现 Kos.pdf 2.2.2 中的状态三元组 σ = 〈K, TS, P〉及小步操作语义：
 * - 状态管理：kos_state_create/free，状态镜像 kos_state_get_*
 * - 事件队列：FIFO 队列，严格顺序提交（Sequential Commit）
 * - 迁移算子：kos_kernel_step（单步）、kos_step（从队列取）、kos_evolution_cycle（批量）
 *
 * 演化流程：Peek → Verify(Pre) → Reduce → Update(K, TS) → Confirm(Post)
 */

#include "kos_kernel.h"
#include "kos_core.h"
#include "kos_knowledge_base.h"
#include "kos_trace.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <stdint.h>

/* MTK: 64-bit FNV-1a 用于 state_hash = fold(trace) */
static uint64_t hash_fnv1a_combine(uint64_t prev, const void* data, size_t len) {
    const uint64_t FNV_OFFSET = 14695981039346656037ULL;
    const uint64_t FNV_PRIME  = 1099511628211ULL;
    uint64_t h = prev ? prev : FNV_OFFSET;
    const unsigned char* p = (const unsigned char*)data;
    for (size_t i = 0; i < len; i++) {
        h ^= (uint64_t)p[i];
        h *= FNV_PRIME;
    }
    return h;
}

/* ========== 状态管理 ========== */

/** 创建初始状态 σ = 〈K, TS=0, P=空队列〉。initial_K 被深拷贝，调用者保留原对象。 */
kos_state_t* kos_state_create(kos_term* initial_K) {
    kos_state_t* sigma = (kos_state_t*)calloc(1, sizeof(kos_state_t));
    if (!sigma) {
        return NULL;
    }
    
    sigma->K = initial_K ? kos_term_copy(initial_K) : NULL;
    sigma->TS = 0;  // 初始逻辑时钟为 0
    sigma->P = kos_queue_create();
    sigma->KB = NULL;   // 知识库可选，由调用者设置
    sigma->trace = NULL; // Event Log（不可变追加），由调用者设置
    sigma->state_hash = 0;  // MTK: version，由 Trace 折叠更新
    
    if (!sigma->P) {
        if (sigma->K) kos_term_free(sigma->K);
        free(sigma);
        return NULL;
    }
    
    return sigma;
}

/** 释放状态及所有关联资源（K、KB、队列及其中的事件对）。 */
void kos_state_free(kos_state_t* sigma) {
    if (!sigma) {
        return;
    }
    
    if (sigma->K) {
        kos_term_free(sigma->K);
    }
    
    if (sigma->KB) {
        kos_kb_free((kos_knowledge_base_t*)sigma->KB);
    }
    if (sigma->trace) {
        kos_trace_free((kos_trace_t*)sigma->trace);
    }
    if (sigma->P) {
        kos_queue_free(sigma->P);
    }
    
    free(sigma);
}

/* ========== Event Queue Manager ==========
 * FIFO 队列，存储待处理事件对 <e, p>。遵循严格顺序提交，保证因果链唯一性。 */

/** 创建空事件队列。 */
queue_t* kos_queue_create(void) {
    queue_t* queue = (queue_t*)calloc(1, sizeof(queue_t));
    if (!queue) {
        return NULL;
    }
    
    queue->front = NULL;
    queue->rear = NULL;
    queue->size = 0;
    
    return queue;
}

void kos_queue_free(queue_t* queue) {
    if (!queue) {
        return;
    }
    
    // 释放所有队列节点
    queue_node* current = queue->front;
    while (current) {
        queue_node* next = current->next;
        if (current->data) {
            kos_term_free(current->data);
        }
        free(current);
        current = next;
    }
    
    free(queue);
}

/** 入队事件对。要求 event_pair 为 KOS_PAIR，内部深拷贝，队列拥有所有权。 */
int kos_queue_enqueue(queue_t* queue, kos_term* event_pair) {
    if (!queue || !event_pair) {
        return -1;
    }
    
    // 验证事件对格式：必须是 KOS_PAIR
    if (event_pair->kind != KOS_PAIR) {
        return -1;
    }
    
    queue_node* node = (queue_node*)malloc(sizeof(queue_node));
    if (!node) {
        return -1;
    }
    
    // 深拷贝事件对（队列拥有所有权）
    node->data = kos_term_copy(event_pair);
    if (!node->data) {
        free(node);
        return -1;
    }
    
    node->next = NULL;
    
    if (queue->rear) {
        queue->rear->next = node;
        queue->rear = node;
    } else {
        queue->front = queue->rear = node;
    }
    
    queue->size++;
    return 0;
}

/** 出队事件对。调用者负责释放返回的 kos_term*。 */
kos_term* kos_queue_dequeue(queue_t* queue) {
    if (!queue || !queue->front) {
        return NULL;
    }
    
    queue_node* node = queue->front;
    kos_term* event_pair = node->data;
    
    queue->front = node->next;
    if (!queue->front) {
        queue->rear = NULL;
    }
    
    queue->size--;
    free(node);
    
    return event_pair;  // 调用者负责释放
}

kos_term* kos_queue_peek(queue_t* queue) {
    if (!queue || !queue->front) {
        return NULL;
    }
    
    return queue->front->data;  // 不取出，只查看
}

bool kos_queue_is_empty(queue_t* queue) {
    return !queue || !queue->front || queue->size == 0;
}

size_t kos_queue_size(queue_t* queue) {
    return queue ? queue->size : 0;
}

/* ========== 前置/后置条件验证 ==========
 * Pre(e): 在当前知识集 K 上验证 p : e
 * Post(e): 验证新状态 K' 是否满足后置条件 */

/** 前置条件 Pre(e)：使用 Core 层 kos_type_check 验证 p : e（在当前 K 下）。 */
bool kos_verify_precondition(kos_term* event_pair, kos_state_t* sigma) {
    if (!event_pair || !sigma || event_pair->kind != KOS_PAIR) {
        return false;
    }
    
    kos_term* e = event_pair->data.pair.data;
    kos_term* p = event_pair->data.pair.proof;
    
    if (!e || !p) {
        return false;
    }
    
    // 使用 Core 层类型检查验证证明是否满足前置条件
    // Pre(e): 在当前知识集 K 上验证 p : e
    return kos_type_check(sigma->K, p, e);
}

bool kos_verify_postcondition(kos_term* event_pair, kos_state_t* old_sigma, kos_state_t* new_sigma) {
    if (!event_pair || !old_sigma || !new_sigma || event_pair->kind != KOS_PAIR) {
        return false;
    }
    
    kos_term* e = event_pair->data.pair.data;
    kos_term* p = event_pair->data.pair.proof;
    
    if (!e || !p) {
        return false;
    }
    
    // Post(e): 验证新状态 K' 是否满足后置条件
    // 简化实现：检查新知识集 K' 是否包含事件 e 的证明
    // 更精细的实现可以检查 K' 是否满足 e 的后置条件类型
    
    // 当前简化：如果新状态的 TS 增加了，且 K' 不为空，则认为后置条件满足
    if (new_sigma->TS > old_sigma->TS && new_sigma->K) {
        return true;
    }
    
    return false;
}

/* ========== 知识集更新 ==========
 * K' = Σ(new_fact, K)，使用 Σ 链表示知识集的单调增长。 */

/** 将 new_fact 追加到知识集 K 前，构造新知识集 K'。返回新 K，调用者负责释放旧 K。 */
kos_term* kos_update_knowledge(kos_term* K, kos_term* new_fact) {
    if (!new_fact) {
        return K;
    }
    
    // 使用 Σ 链结构表示知识集的单调增长
    // K' = Σ(new_fact, K)
    // 这样每个新事实都被"追加"到知识集的前面
    
    // 如果 K 为空，直接返回 new_fact 的副本
    if (!K) {
        return kos_term_copy(new_fact);
    }
    
    // 构造 Σ(new_fact, K)
    // 这里简化处理：使用 KOS_PAIR 表示知识集的追加
    // 实际实现中，可以考虑更高效的数据结构（如链表或树）
    kos_term* new_K = kos_mk_pair(kos_term_copy(new_fact), kos_term_copy(K));
    
    if (!new_K) {
        return K;  // 失败时返回原知识集
    }
    
    return new_K;
}

// ========== State Mirror ==========

const kos_term* kos_state_get_K(const kos_state_t* sigma) {
    return sigma ? sigma->K : NULL;
}

int kos_state_get_TS(const kos_state_t* sigma) {
    return sigma ? sigma->TS : -1;
}

size_t kos_state_get_queue_size(const kos_state_t* sigma) {
    return sigma && sigma->P ? sigma->P->size : 0;
}

bool kos_state_is_empty(const kos_state_t* sigma) {
    if (!sigma) {
        return true;
    }
    
    return !sigma->K && kos_queue_is_empty(sigma->P);
}

void kos_state_set_kb(kos_state_t* sigma, void* kb) {
    if (sigma) sigma->KB = kb;
}

void* kos_state_get_kb(const kos_state_t* sigma) {
    return sigma ? sigma->KB : NULL;
}

void kos_state_set_trace(kos_state_t* sigma, void* trace) {
    if (sigma) sigma->trace = trace;
}

void* kos_state_get_trace(const kos_state_t* sigma) {
    return sigma ? sigma->trace : NULL;
}

uint64_t kos_state_get_hash(const kos_state_t* sigma) {
    return sigma ? sigma->state_hash : 0;
}

void kos_state_update_hash_after_step(kos_state_t* sigma, kos_term* event_pair) {
    if (!sigma || !event_pair) return;
    kos_serialized* ser = kos_term_serialize(event_pair);
    if (!ser || !ser->data) return;
    sigma->state_hash = hash_fnv1a_combine(sigma->state_hash, ser->data, ser->length);
    kos_serialized_free(ser);
}

/* ========== Evolution Scheduler ==========
 * 单步演化：Verify(Pre) → Reduce(e) → Update(K, TS++) → Confirm(Post) */

/** 对给定事件对执行单步演化。不涉及队列，直接更新 sigma。 */
bool kos_kernel_step(kos_state_t* sigma, kos_term* event_pair) {
    if (!sigma || !event_pair || event_pair->kind != KOS_PAIR) {
        return false;
    }
    
    kos_term* e = event_pair->data.pair.data;
    kos_term* p = event_pair->data.pair.proof;
    
    if (!e || !p) {
        return false;
    }

    // 1. Verify: 前置条件验证 Pre(e)
    if (!kos_verify_precondition(event_pair, sigma)) {
        return false;  // 拦截非法演化
    }

    // 2. Reduce: 执行归约操作
    // 对事件 e 进行归约（β/ι 归约）
    kos_term* reduced_e = kos_reduce(kos_term_copy(e));
    if (!reduced_e) {
        return false;
    }

    // 3. Update: 更新知识集和逻辑时钟
    kos_term* old_K = sigma->K;
    sigma->K = kos_update_knowledge(sigma->K, reduced_e);
    
    if (!sigma->K) {
        sigma->K = old_K;  // 恢复原状态
        kos_term_free(reduced_e);
        return false;
    }
    
    // 如果更新成功，释放旧知识集
    if (old_K && old_K != sigma->K) {
        kos_term_free(old_K);
    }
    
    sigma->TS++;  // 逻辑时钟单调递增

    // 若存在知识库，将物化项加入（来源：运行时物化）
    if (sigma->KB && reduced_e) {
        char idbuf[64];
        snprintf(idbuf, sizeof(idbuf), "evt_ts%d", sigma->TS);
        kos_kb_add_item((kos_knowledge_base_t*)sigma->KB, idbuf,
                        kos_term_copy(reduced_e), kos_term_copy(e),
                        sigma->TS, KOS_KB_SOURCE_MATERIALIZED);
    }
    kos_term_free(reduced_e);

    // 4. Confirm: 后置条件验证 Post(e)（简化实现）
    // 这里可以进一步验证新状态是否满足后置条件
    // 当前简化：如果 TS 增加了，则认为演化成功

    // 若挂载了轨迹（Event Log），追加本步 σ →^⟨e,p⟩ σ'（不可变追加）
    if (sigma->trace) {
        kos_trace_append((kos_trace_t*)sigma->trace, sigma, event_pair);
    }
    // MTK: 更新 state_hash = fold(trace)，满足 State Hash Stability
    kos_state_update_hash_after_step(sigma, event_pair);
    return true;
}

/** 从队列头部取事件，执行 kos_kernel_step；成功则出队并释放。 */
bool kos_step(kos_state_t* sigma) {
    if (!sigma || kos_queue_is_empty(sigma->P)) {
        return false;
    }
    
    // Peek: 查看队列头部事件（不取出）
    kos_term* event_pair = kos_queue_peek(sigma->P);
    if (!event_pair) {
        return false;
    }
    
    // 保存旧状态用于后置条件验证
    kos_state_t old_sigma = *sigma;
    old_sigma.K = sigma->K ? kos_term_copy(sigma->K) : NULL;
    
    // 执行演化
    bool result = kos_kernel_step(sigma, event_pair);
    
    // 如果演化成功，从队列中移除事件
    if (result) {
        kos_term* dequeued = kos_queue_dequeue(sigma->P);
        if (dequeued) {
            kos_term_free(dequeued);
        }
        
        // 验证后置条件
        kos_verify_postcondition(event_pair, &old_sigma, sigma);
    }
    
    // 清理旧状态副本
    if (old_sigma.K && old_sigma.K != sigma->K) {
        kos_term_free(old_sigma.K);
    }
    
    return result;
}

/** 完整演化循环：持续 kos_step 直到队列为空或某步失败。有 max_iterations 防止无限循环。 */
bool kos_evolution_cycle(kos_state_t* sigma) {
    if (!sigma) {
        return false;
    }
    
    // 完整的 Peek-Verify-Reduce-Confirm 循环
    // 持续处理队列中的事件，直到队列为空或遇到错误
    
    bool has_progress = true;
    int max_iterations = 1000;  // 防止无限循环
    int iterations = 0;
    
    while (has_progress && !kos_queue_is_empty(sigma->P) && iterations < max_iterations) {
        bool step_result = kos_step(sigma);
        has_progress = step_result;
        iterations++;
        
        // 如果步骤失败，停止循环（可选：可以继续处理下一个事件）
        if (!step_result) {
            break;
        }
    }
    
    return iterations > 0 && has_progress;
}
