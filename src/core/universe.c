// src/core/universe.c
// Universe层级系统实现：双轴世界的核心机制

#include "kos_core.h"
#include <stdlib.h>
#include <string.h>

// 前向声明，避免循环依赖
extern kos_term* kos_mk_universe_computational(int level);
extern kos_term* kos_mk_universe_logical(int level);

// 获取类型的Universe信息
universe_info kos_get_universe_info(kos_term* type) {
    universe_info info = {UNIVERSE_COMPUTATIONAL, 0};
    
    if (!type) {
        return info;
    }
    
    // 如果类型本身有universe信息，直接返回
    if (type->kind == KOS_U || type->kind == KOS_TYPE) {
        info.axis = type->data.universe.axis;
        info.level = type->data.universe.level;
        return info;
    }
    
    // 根据类型kind推断Universe信息
    switch (type->kind) {
        case KOS_PROP:
            // Prop : Type_1
            info.axis = UNIVERSE_LOGICAL;
            info.level = 1;
            break;
            
        case KOS_VAL:
        case KOS_TIME:
        case KOS_ID:
            // Base Sorts在U_0
            info.axis = UNIVERSE_COMPUTATIONAL;
            info.level = 0;
            break;
            
        case KOS_PI:
        case KOS_SIGMA:
        case KOS_SUM:
            // 这些类型的Universe层级需要从类型结构中计算
            // 暂时返回类型结构中存储的universe信息
            info = type->universe;
            break;
            
        default:
            // 默认使用类型结构中存储的信息
            info = type->universe;
            break;
    }
    
    return info;
}

// 检查Universe层级关系
// 返回true如果u1 <= u2（u1在u2之前或可提升到u2）
bool kos_universe_leq(universe_info u1, universe_info u2) {
    // 相同轴：检查层级
    if (u1.axis == u2.axis) {
        return u1.level <= u2.level;
    }
    
    // 不同轴：应用Universe Lifting规则
    // U_i : Type_{i+1} (计算轴可提升到逻辑轴)
    if (u1.axis == UNIVERSE_COMPUTATIONAL && u2.axis == UNIVERSE_LOGICAL) {
        return u1.level + 1 <= u2.level;
    }
    
    // Prop : U_1 (命题可嵌入到数据轴)
    if (u1.axis == UNIVERSE_LOGICAL && u1.level == 1 && 
        u2.axis == UNIVERSE_COMPUTATIONAL && u2.level == 1) {
        return true;
    }
    
    // 其他情况：不可比较
    return false;
}

// 应用Universe Lifting规则：U_i : Type_{i+1}
kos_term* kos_universe_lift_to_logic(kos_term* type) {
    if (!type) {
        return NULL;
    }
    
    universe_info info = kos_get_universe_info(type);
    
    // 如果已经是逻辑轴，直接返回
    if (info.axis == UNIVERSE_LOGICAL) {
        return kos_term_copy(type);
    }
    
    // 如果是计算轴，提升到逻辑轴
    if (info.axis == UNIVERSE_COMPUTATIONAL) {
        // 创建新的Type_{i+1}类型
        kos_term* lifted = kos_mk_universe_logical(info.level + 1);
        if (!lifted) {
            return NULL;
        }
        
        // 设置原始类型信息（用于类型检查）
        lifted->universe = info;
        return lifted;
    }
    
    return kos_term_copy(type);
}

// 应用Proposition Embedding规则：Prop ↪ U_1
kos_term* kos_prop_embed_to_data(kos_term* prop) {
    if (!prop || prop->kind != KOS_PROP) {
        return NULL;
    }
    
    // 创建U_1类型
    kos_term* embedded = kos_mk_universe_computational(1);
    if (!embedded) {
        return NULL;
    }
    
    // 保持原始Prop信息
    embedded->universe.axis = UNIVERSE_LOGICAL;
    embedded->universe.level = 1;
    
    return embedded;
}

