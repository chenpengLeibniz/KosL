#include "kos_kernel.h"
#include "kos_core.h"
#include <stdlib.h>
#include <string.h>

// 验证事件的前置条件 Pre(e)
// 对于转账事件：验证余额充足 + 实名证明
bool kos_verify_precondition(kos_term* event_pair, kos_state_t* sigma) {
    if (!event_pair || !sigma || event_pair->kind != KOS_PAIR) {
        return false;
    }
    
    kos_term* e = event_pair->data.pair.data;
    kos_term* p = event_pair->data.pair.proof;
    
    if (!e || !p) {
        return false;
    }
    
    // 使用类型检查验证证明是否满足前置条件
    // 对于转账事件，证明必须包含：余额充足证明 + 实名证明
    return kos_type_check(sigma->K, p, e);
}

// STEP 算子：实现状态的确定性演化 [cite: 623]
// 从旧状态 σ 到新状态 σ' 的单调演化
bool kos_kernel_step(kos_state_t* sigma, kos_term* event_pair) {
    if (!sigma || !event_pair || event_pair->kind != KOS_PAIR) {
        return false;
    }
    
    // 1. 提取事件 e 和证明 p [cite: 624]
    kos_term* e = event_pair->data.pair.data;
    kos_term* p = event_pair->data.pair.proof;
    
    if (!e || !p) {
        return false;
    }

    // 2. 前置条件验证 Pre(e) [cite: 630]
    // 验证证明是否满足事件的前置条件（余额充足 + 实名）
    if (!kos_verify_precondition(event_pair, sigma)) {
        return false; // 拦截非法演化 [cite: 632]
    }

    // 3. 执行归约并更新状态 [cite: 610, 625]
    extern kos_term* kos_reduce_update(kos_term* K, kos_term* e);
    sigma->K = kos_update_knowledge(sigma->K, e);
    sigma->TS++; // 逻辑时钟增加 [cite: 659]
    
    return true;
}

// 更新知识集（添加新证明的事实）
kos_term* kos_update_knowledge(kos_term* K, kos_term* new_fact) {
    if (!new_fact) {
        return K;
    }
    
    // 简化实现：将新事实添加到知识集
    // 实际实现中，K应该是一个更高效的数据结构
    // 这里暂时返回原知识集（实际应该合并新知识）
    (void)new_fact;
    return K;
}

// kos_step 的简化版本（不带事件对）
bool kos_step(kos_state_t* sigma) {
    if (!sigma || !sigma->P || !sigma->P->front) {
        return false;
    }
    
    // 从队列中取出事件对
    queue_node* node = sigma->P->front;
    kos_term* event_pair = node->data;
    
    // 执行步骤
    bool result = kos_kernel_step(sigma, event_pair);
    
    // 从队列中移除
    sigma->P->front = node->next;
    if (!sigma->P->front) {
        sigma->P->rear = NULL;
    }
    free(node);
    sigma->P->size--;
    
    return result;
}
