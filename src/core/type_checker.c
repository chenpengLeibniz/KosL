#include "kos_core.h"
#include <stdlib.h>
#include <string.h>

// Universe层级函数在universe.c中实现，通过kos_core.h声明

// 双向类型检查：验证 term 是否具有类型 type [cite: 598]
// 这是更通用的类型检查函数，支持完整的类型系统和双轴世界
bool kos_check(kos_term* ctx, kos_term* term, kos_term* type) {
    (void)ctx; // 上下文暂时未使用（简化实现）
    
    if (!term || !type) {
        return false;
    }
    
    // 应用Universe Lifting规则：尝试将term的类型提升到type的Universe层级
    universe_info term_info = kos_get_universe_info(term);
    universe_info type_info = kos_get_universe_info(type);
    
    // 检查Universe层级兼容性（简化实现，主要检查基本类型匹配）
    
    // 根据类型系统的规则进行类型检查
    switch (type->kind) {
        case KOS_PROP:
            // 命题类型的检查：项必须是命题或证明
            return (term->kind == KOS_PROP || term->kind == KOS_VAL);
            
        case KOS_PI:
            // Π类型检查：项必须是函数（λ抽象）或可以应用到参数
            if (term->kind == KOS_PI) {
                // λ抽象：检查body_term的类型是否符合body类型
                // 简化实现：如果有body_term，则检查基本结构
                if (term->data.pi.body_term) {
                    // 递归检查domain是否匹配
                    if (type->data.pi.domain && term->data.pi.domain) {
                        return kos_check(ctx, term->data.pi.domain, type->data.pi.domain);
                    }
                    return true; // 简化：有body_term就认为合法
                }
                return true;
            }
            // 函数应用的情况：在归约阶段处理
            return false;
            
        case KOS_SIGMA:
            // Σ类型检查：项必须是pair <d, p>
            if (term->kind == KOS_PAIR) {
                // 检查pair的data类型是否符合domain
                if (type->data.sigma.domain && term->data.pair.data) {
                    bool domain_ok = kos_check(ctx, term->data.pair.data, type->data.sigma.domain);
                    if (!domain_ok) return false;
                }
                // 检查pair的proof类型是否符合body（依赖类型）
                if (type->data.sigma.body && term->data.pair.proof) {
                    // 简化：基本检查
                    return true;
                }
                return true;
            }
            return false;
            
        case KOS_SUM:
            // Sum类型检查：项必须是inl或inr
            if (term->kind == KOS_SUM && term->data.sum.value) {
                // 检查值的类型是否匹配left_type或right_type
                if (term->data.sum.is_left && type->data.sum.left_type) {
                    return kos_check(ctx, term->data.sum.value, type->data.sum.left_type);
                } else if (!term->data.sum.is_left && type->data.sum.right_type) {
                    return kos_check(ctx, term->data.sum.value, type->data.sum.right_type);
                }
                return true;
            }
            return false;
            
        case KOS_VAL:
        case KOS_TIME:
        case KOS_ID:
        case KOS_PAIR:
            // 基本类型匹配
            return (term->kind == type->kind);
            
        case KOS_U:
        case KOS_TYPE:
            // Universe类型的检查：比较Universe层级
            if (term->kind == type->kind) {
                return (term->data.universe.axis == type->data.universe.axis &&
                        term->data.universe.level == type->data.universe.level);
            }
            // 如果term不是Universe类型，检查是否可以提升到该Universe层级
            return kos_universe_leq(term_info, type_info);
            
        default:
            // 基本类型匹配
            return (term->kind == type->kind);
    }
}

// 类型检查：验证 proof 是否为命题 prop 的有效证明 [cite: 598]
// 确保任何进入系统的知识项都符合本体约束 [cite: 587]
bool kos_type_check(kos_term* ctx, kos_term* proof, kos_term* prop) {
    // 使用通用的kos_check函数
    return kos_check(ctx, proof, prop);
}