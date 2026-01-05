// src/core/type_builder.c
// Core 层类型构建器：提供便捷的类型构造接口

#include "kos_core.h"
#include <stdlib.h>
#include <string.h>

// 创建原子值类型
kos_term* kos_mk_atomic(const char* val, kos_term* type) {
    if (!val) {
        return NULL;
    }
    
    kos_term* term = (kos_term*)calloc(1, sizeof(kos_term));
    if (!term) {
        return NULL;
    }
    
    term->kind = KOS_VAL;
    term->data.atomic.val = (char*)malloc(strlen(val) + 1);
    if (!term->data.atomic.val) {
        free(term);
        return NULL;
    }
    
    strcpy(term->data.atomic.val, val);
    term->data.atomic.type = type;
    
    // 初始化Universe信息：Val在U_0
    term->universe.axis = UNIVERSE_COMPUTATIONAL;
    term->universe.level = 0;
    
    return term;
}

// 创建命题类型
kos_term* kos_mk_prop(const char* prop_name) {
    if (!prop_name) {
        return NULL;
    }
    
    kos_term* term = (kos_term*)calloc(1, sizeof(kos_term));
    if (!term) {
        return NULL;
    }
    
    term->kind = KOS_PROP;
    term->data.atomic.val = (char*)malloc(strlen(prop_name) + 1);
    if (!term->data.atomic.val) {
        free(term);
        return NULL;
    }
    
    strcpy(term->data.atomic.val, prop_name);
    term->data.atomic.type = NULL;
    
    // 初始化Universe信息：Prop : Type_1
    term->universe.axis = UNIVERSE_LOGICAL;
    term->universe.level = 1;
    
    return term;
}

// 创建值类型
kos_term* kos_mk_val(const char* val) {
    return kos_mk_atomic(val, NULL);
}

// 创建时间类型
kos_term* kos_mk_time(const char* time_val) {
    if (!time_val) {
        return NULL;
    }
    
    kos_term* term = (kos_term*)calloc(1, sizeof(kos_term));
    if (!term) {
        return NULL;
    }
    
    term->kind = KOS_TIME;
    term->data.atomic.val = (char*)malloc(strlen(time_val) + 1);
    if (!term->data.atomic.val) {
        free(term);
        return NULL;
    }
    
    strcpy(term->data.atomic.val, time_val);
    term->data.atomic.type = NULL;
    
    // 初始化Universe信息：Time在U_0
    term->universe.axis = UNIVERSE_COMPUTATIONAL;
    term->universe.level = 0;
    
    return term;
}

// 创建标识符类型
kos_term* kos_mk_id(const char* id_val) {
    if (!id_val) {
        return NULL;
    }
    
    kos_term* term = (kos_term*)calloc(1, sizeof(kos_term));
    if (!term) {
        return NULL;
    }
    
    term->kind = KOS_ID;
    term->data.atomic.val = (char*)malloc(strlen(id_val) + 1);
    if (!term->data.atomic.val) {
        free(term);
        return NULL;
    }
    
    strcpy(term->data.atomic.val, id_val);
    term->data.atomic.type = NULL;
    
    // 初始化Universe信息：ID在U_0
    term->universe.axis = UNIVERSE_COMPUTATIONAL;
    term->universe.level = 0;
    
    return term;
}

// 创建计算轴Universe类型 U_i
kos_term* kos_mk_universe_computational(int level) {
    kos_term* term = (kos_term*)calloc(1, sizeof(kos_term));
    if (!term) {
        return NULL;
    }
    
    term->kind = KOS_U;
    term->data.universe.axis = UNIVERSE_COMPUTATIONAL;
    term->data.universe.level = level;
    
    // Universe类型本身的Universe信息
    term->universe.axis = UNIVERSE_COMPUTATIONAL;
    term->universe.level = level + 1; // U_i : U_{i+1}
    
    return term;
}

// 创建逻辑轴Universe类型 Type_i
kos_term* kos_mk_universe_logical(int level) {
    kos_term* term = (kos_term*)calloc(1, sizeof(kos_term));
    if (!term) {
        return NULL;
    }
    
    term->kind = KOS_TYPE;
    term->data.universe.axis = UNIVERSE_LOGICAL;
    term->data.universe.level = level;
    
    // Type_i本身的Universe信息：Type_i : Type_{i+1}
    term->universe.axis = UNIVERSE_LOGICAL;
    term->universe.level = level + 1;
    
    return term;
}

// 创建对类型 <d, p> (Σ-Type的核心)
kos_term* kos_mk_pair(kos_term* data, kos_term* proof) {
    if (!data || !proof) {
        return NULL;
    }
    
    kos_term* term = (kos_term*)calloc(1, sizeof(kos_term));
    if (!term) {
        return NULL;
    }
    
    term->kind = KOS_PAIR;
    term->data.pair.data = data;
    term->data.pair.proof = proof;
    
    // Pair类型的Universe层级由data和proof的类型决定（简化实现，使用data的类型）
    if (data) {
        term->universe = data->universe;
    } else {
        term->universe.axis = UNIVERSE_COMPUTATIONAL;
        term->universe.level = 0;
    }
    
    return term;
}

// 创建依赖类型 Σ(x:A).B
kos_term* kos_mk_sigma(kos_term* domain, kos_term* body) {
    if (!domain || !body) {
        return NULL;
    }
    
    kos_term* term = (kos_term*)calloc(1, sizeof(kos_term));
    if (!term) {
        return NULL;
    }
    
    term->kind = KOS_SIGMA;
    term->data.sigma.domain = domain;
    term->data.sigma.body = body;
    
    // Σ类型构造规则：Σ(x:A).B : U_max(i,j)，其中A : U_i, B : U_j
    // 简化实现：使用domain和body的universe层级的最大值
    universe_info dom_info = domain ? domain->universe : (universe_info){UNIVERSE_COMPUTATIONAL, 0};
    universe_info body_info = body ? body->universe : (universe_info){UNIVERSE_COMPUTATIONAL, 0};
    
    if (dom_info.axis == UNIVERSE_COMPUTATIONAL && body_info.axis == UNIVERSE_COMPUTATIONAL) {
        term->universe.axis = UNIVERSE_COMPUTATIONAL;
        term->universe.level = (dom_info.level > body_info.level) ? dom_info.level : body_info.level;
    } else {
        // 如果涉及逻辑轴，使用逻辑轴（简化处理）
        term->universe.axis = UNIVERSE_LOGICAL;
        term->universe.level = (dom_info.level > body_info.level) ? dom_info.level : body_info.level;
    }
    
    return term;
}

// 创建依赖积类型 Π(x:A).B
kos_term* kos_mk_pi(kos_term* domain, kos_term* body) {
    if (!domain || !body) {
        return NULL;
    }
    
    kos_term* term = (kos_term*)calloc(1, sizeof(kos_term));
    if (!term) {
        return NULL;
    }
    
    term->kind = KOS_PI;
    term->data.pi.domain = domain;
    term->data.pi.body = body;
    term->data.pi.body_term = NULL; // 类型定义时不包含body_term
    
    // Π类型构造规则：
    // 如果body是Prop，则结果也是Prop（Impredicative）
    // 否则：Π(x:A).B : Type_max(i,j)，其中A : Type_i, B : Type_j
    universe_info dom_info = domain ? domain->universe : (universe_info){UNIVERSE_LOGICAL, 0};
    universe_info body_info = body ? body->universe : (universe_info){UNIVERSE_LOGICAL, 0};
    
    if (body && body->kind == KOS_PROP) {
        // Impredicative规则：如果B是Prop，结果也是Prop
        term->universe.axis = UNIVERSE_LOGICAL;
        term->universe.level = 1; // Prop : Type_1
    } else {
        // Predicative规则：Type_max(i,j)
        term->universe.axis = UNIVERSE_LOGICAL;
        term->universe.level = (dom_info.level > body_info.level) ? dom_info.level : body_info.level;
    }
    
    return term;
}

// 创建λ抽象（Π类型引入）：λx:A.t : Π(x:A).B
kos_term* kos_mk_lambda(kos_term* domain, kos_term* body_term) {
    if (!domain || !body_term) {
        return NULL;
    }
    
    kos_term* term = (kos_term*)calloc(1, sizeof(kos_term));
    if (!term) {
        return NULL;
    }
    
    term->kind = KOS_PI;
    term->data.pi.domain = domain;
    term->data.pi.body = NULL; // body类型在类型检查时确定
    term->data.pi.body_term = body_term; // λ抽象体
    
    return term;
}

// 创建函数应用（Π类型消除）：f a
// 注意：这是一个简化实现，实际应用需要类型检查
kos_term* kos_mk_app(kos_term* func, kos_term* arg) {
    if (!func || !arg) {
        return NULL;
    }
    
    // 函数应用实际上是一个特殊的pair结构，用于表示应用
    // 在实际归约时会进行β-reduction
    // 这里我们用一个临时的PAIR来标记应用（需要后续改进）
    kos_term* term = (kos_term*)calloc(1, sizeof(kos_term));
    if (!term) {
        return NULL;
    }
    
    // 暂时使用PAIR来标记应用（需要后续改进数据结构）
    term->kind = KOS_PAIR;
    term->data.pair.data = func;
    term->data.pair.proof = arg; // 这里proof暂时存储参数
    
    return term;
}

// 创建和类型 A + B（类型定义）
kos_term* kos_mk_sum(kos_term* left_type, kos_term* right_type) {
    if (!left_type || !right_type) {
        return NULL;
    }
    
    kos_term* term = (kos_term*)calloc(1, sizeof(kos_term));
    if (!term) {
        return NULL;
    }
    
    term->kind = KOS_SUM;
    term->data.sum.left_type = left_type;
    term->data.sum.right_type = right_type;
    term->data.sum.value = NULL;
    term->data.sum.is_left = false; // 类型定义时无值
    
    return term;
}

// 创建inl (左注入)：inl(a) : A + B
kos_term* kos_mk_inl(kos_term* left_type, kos_term* right_type, kos_term* value) {
    if (!left_type || !right_type || !value) {
        return NULL;
    }
    
    kos_term* term = (kos_term*)calloc(1, sizeof(kos_term));
    if (!term) {
        return NULL;
    }
    
    term->kind = KOS_SUM;
    term->data.sum.left_type = left_type;
    term->data.sum.right_type = right_type;
    term->data.sum.value = value;
    term->data.sum.is_left = true;
    
    return term;
}

// 创建inr (右注入)：inr(b) : A + B
kos_term* kos_mk_inr(kos_term* left_type, kos_term* right_type, kos_term* value) {
    if (!left_type || !right_type || !value) {
        return NULL;
    }
    
    kos_term* term = (kos_term*)calloc(1, sizeof(kos_term));
    if (!term) {
        return NULL;
    }
    
    term->kind = KOS_SUM;
    term->data.sum.left_type = left_type;
    term->data.sum.right_type = right_type;
    term->data.sum.value = value;
    term->data.sum.is_left = false;
    
    return term;
}

// 创建case分析（和类型消除）：case(s, x.t, y.u)
// 注意：这是一个简化实现，实际的case分析需要更复杂的结构
kos_term* kos_mk_case(kos_term* sum_term, kos_term* left_branch, kos_term* right_branch) {
    if (!sum_term || !left_branch || !right_branch) {
        return NULL;
    }
    
    // case分析需要一个特殊的数据结构
    // 暂时使用SIGMA来标记（需要后续改进）
    kos_term* term = (kos_term*)calloc(1, sizeof(kos_term));
    if (!term) {
        return NULL;
    }
    
    term->kind = KOS_SIGMA;
    term->data.sigma.domain = sum_term;
    // 暂时用body存储分支（需要改进）
    term->data.sigma.body = left_branch; // 需要重新设计
    
    return term;
}

// 创建split操作（Σ类型消除）：split(p, x.y.t)
kos_term* kos_mk_split(kos_term* pair_term, kos_term* body_term) {
    if (!pair_term || !body_term) {
        return NULL;
    }
    
    // split操作需要特殊处理
    // 暂时使用SIGMA来标记（需要后续改进）
    kos_term* term = (kos_term*)calloc(1, sizeof(kos_term));
    if (!term) {
        return NULL;
    }
    
    term->kind = KOS_SIGMA;
    term->data.sigma.domain = pair_term;
    term->data.sigma.body = body_term;
    
    return term;
}

// 复制 term（深拷贝）
kos_term* kos_term_copy(kos_term* t) {
    if (!t) {
        return NULL;
    }
    
    kos_term* copy = (kos_term*)calloc(1, sizeof(kos_term));
    if (!copy) {
        return NULL;
    }
    
    copy->kind = t->kind;
    
    switch (t->kind) {
        case KOS_VAL:
        case KOS_TIME:
        case KOS_ID:
        case KOS_PROP:
            if (t->data.atomic.val) {
                copy->data.atomic.val = (char*)malloc(strlen(t->data.atomic.val) + 1);
                if (copy->data.atomic.val) {
                    strcpy(copy->data.atomic.val, t->data.atomic.val);
                }
            }
            copy->data.atomic.type = t->data.atomic.type ? kos_term_copy(t->data.atomic.type) : NULL;
            copy->universe = t->universe;
            break;
            
        case KOS_U:
        case KOS_TYPE:
            copy->data.universe.axis = t->data.universe.axis;
            copy->data.universe.level = t->data.universe.level;
            copy->universe = t->universe;
            break;
            
        case KOS_PAIR:
            copy->data.pair.data = kos_term_copy(t->data.pair.data);
            copy->data.pair.proof = kos_term_copy(t->data.pair.proof);
            copy->universe = t->universe;
            break;
            
        case KOS_SIGMA:
            copy->data.sigma.domain = kos_term_copy(t->data.sigma.domain);
            copy->data.sigma.body = kos_term_copy(t->data.sigma.body);
            copy->universe = t->universe;
            break;
            
        case KOS_PI:
            copy->data.pi.domain = kos_term_copy(t->data.pi.domain);
            copy->data.pi.body = t->data.pi.body ? kos_term_copy(t->data.pi.body) : NULL;
            copy->data.pi.body_term = t->data.pi.body_term ? kos_term_copy(t->data.pi.body_term) : NULL;
            copy->universe = t->universe;
            break;
            
        case KOS_SUM:
            copy->data.sum.left_type = kos_term_copy(t->data.sum.left_type);
            copy->data.sum.right_type = kos_term_copy(t->data.sum.right_type);
            copy->data.sum.value = t->data.sum.value ? kos_term_copy(t->data.sum.value) : NULL;
            copy->data.sum.is_left = t->data.sum.is_left;
            copy->universe = t->universe;
            break;
    }
    
    return copy;
}

// 释放 term（递归释放所有子项）
void kos_term_free(kos_term* t) {
    if (!t) {
        return;
    }
    
    switch (t->kind) {
        case KOS_VAL:
        case KOS_TIME:
        case KOS_ID:
        case KOS_PROP:
            if (t->data.atomic.val) {
                free(t->data.atomic.val);
            }
            if (t->data.atomic.type) {
                kos_term_free(t->data.atomic.type);
            }
            break;
            
        case KOS_U:
        case KOS_TYPE:
            // Universe类型没有需要释放的子结构
            break;
            
        case KOS_PAIR:
            if (t->data.pair.data) {
                kos_term_free(t->data.pair.data);
            }
            if (t->data.pair.proof) {
                kos_term_free(t->data.pair.proof);
            }
            break;
            
        case KOS_SIGMA:
            if (t->data.sigma.domain) {
                kos_term_free(t->data.sigma.domain);
            }
            if (t->data.sigma.body) {
                kos_term_free(t->data.sigma.body);
            }
            break;
            
        case KOS_PI:
            if (t->data.pi.domain) {
                kos_term_free(t->data.pi.domain);
            }
            if (t->data.pi.body) {
                kos_term_free(t->data.pi.body);
            }
            if (t->data.pi.body_term) {
                kos_term_free(t->data.pi.body_term);
            }
            break;
            
        case KOS_SUM:
            if (t->data.sum.left_type) {
                kos_term_free(t->data.sum.left_type);
            }
            if (t->data.sum.right_type) {
                kos_term_free(t->data.sum.right_type);
            }
            if (t->data.sum.value) {
                kos_term_free(t->data.sum.value);
            }
            break;
    }
    
    free(t);
}




