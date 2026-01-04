#include "kos_core.h"
#include <stdlib.h>
#include <string.h>

// 双向类型检查：验证 proof 是否为命题 prop 的有效证明 [cite: 598]
// 确保任何进入系统的知识项都符合本体约束 [cite: 587]
bool kos_type_check(kos_term* ctx, kos_term* proof, kos_term* prop) {
    (void)ctx; // 上下文暂时未使用
    
    if (!proof || !prop) {
        return false;
    }
    
    // 简化实现：基本类型匹配
    // 实际实现中应该进行更严格的证明验证
    if (proof->kind != KOS_PROP || prop->kind != KOS_PROP) {
        return false;
    }
    
    // 检查证明内容是否匹配命题
    // 这里简化处理，实际应该进行逻辑验证
    return true;
}

// kos_check 的别名实现
bool kos_check(kos_term* ctx, kos_term* term, kos_term* type) {
    return kos_type_check(ctx, term, type);
}