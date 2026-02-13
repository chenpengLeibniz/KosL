/**
 * @file type_builder.c
 * @brief KOS Core 层类型构建器（Type Builder）
 *
 * 提供 kos_term 的便捷构造接口，支持：
 * - 原子类型：kos_mk_val, kos_mk_time, kos_mk_id, kos_mk_prop
 * - Σ/Π 类型：kos_mk_pair, kos_mk_sigma, kos_mk_pi
 * - 和类型：kos_mk_sum, kos_mk_inl, kos_mk_inr
 * - Universe：kos_mk_universe_computational, kos_mk_universe_logical
 *
 * 约束：kos_mk_sigma/mk_pi/mk_sum 要求子类型良构，否则返回 NULL。
 */

#include "kos_core.h"
#include <stdlib.h>
#include <string.h>

/** 创建原子值类型 KOS_VAL，val 为字符串，type 可选。 */
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

/** 创建命题类型 KOS_PROP（Prop : Type_1）。 */
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

/** 创建值类型 KOS_VAL，委托给 kos_mk_atomic(val, NULL)。 */
kos_term* kos_mk_val(const char* val) {
    return kos_mk_atomic(val, NULL);
}

/** 创建时间类型 KOS_TIME（U_0）。 */
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

/** 创建标识符类型 KOS_ID（U_0）。 */
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

/** 创建 Σ-Type 对 <d, p>。data 与 proof 由调用者拥有，pair 不拷贝。 */
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
// Core 层约束：domain/body 必须为良构类型，否则拒绝构造（非法类型不生成）
kos_term* kos_mk_sigma_named(const char* var_name, kos_term* domain, kos_term* body) {
    if (!domain || !body) {
        return NULL;
    }
    if (!kos_type_wellformed(domain) || !kos_type_wellformed(body)) {
        return NULL;  /* 非法类型：子类型非良构 */
    }

    kos_term* term = (kos_term*)calloc(1, sizeof(kos_term));
    if (!term) {
        return NULL;
    }
    
    term->kind = KOS_SIGMA;
    if (var_name) {
        term->data.sigma.var_name = (char*)malloc(strlen(var_name) + 1);
        if (!term->data.sigma.var_name) {
            free(term);
            return NULL;
        }
        strcpy(term->data.sigma.var_name, var_name);
    } else {
        term->data.sigma.var_name = NULL;
    }
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

// 创建依赖类型 Σ(x:A).B（默认变量名 x）
kos_term* kos_mk_sigma(kos_term* domain, kos_term* body) {
    return kos_mk_sigma_named("x", domain, body);
}

// 创建依赖积类型 Π(x:A).B
// Core 层约束：domain/body 必须为良构类型，否则拒绝构造
kos_term* kos_mk_pi_named(const char* var_name, kos_term* domain, kos_term* body) {
    if (!domain || !body) {
        return NULL;
    }
    if (!kos_type_wellformed(domain) || !kos_type_wellformed(body)) {
        return NULL;  /* 非法类型：子类型非良构 */
    }

    kos_term* term = (kos_term*)calloc(1, sizeof(kos_term));
    if (!term) {
        return NULL;
    }
    
    term->kind = KOS_PI;
    if (var_name) {
        term->data.pi.var_name = (char*)malloc(strlen(var_name) + 1);
        if (!term->data.pi.var_name) {
            free(term);
            return NULL;
        }
        strcpy(term->data.pi.var_name, var_name);
    } else {
        term->data.pi.var_name = NULL;
    }
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

// 创建依赖积类型 Π(x:A).B（默认变量名 x）
kos_term* kos_mk_pi(kos_term* domain, kos_term* body) {
    return kos_mk_pi_named("x", domain, body);
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
    term->data.pi.var_name = NULL;
    term->data.pi.domain = domain;
    term->data.pi.body = NULL; // body类型在类型检查时确定
    term->data.pi.body_term = body_term; // λ抽象体
    
    return term;
}

// 创建函数应用（Π类型消除）：f a
kos_term* kos_mk_app(kos_term* func, kos_term* arg) {
    if (!func || !arg) {
        return NULL;
    }
    
    kos_term* term = (kos_term*)calloc(1, sizeof(kos_term));
    if (!term) {
        return NULL;
    }
    
    term->kind = KOS_APP;
    term->data.app.func = func;
    term->data.app.arg = arg;
    term->universe = func->universe;
    
    return term;
}

// 创建 λ 抽象（证明项）：lam x : A . body
kos_term* kos_mk_lam(const char* var_name, kos_term* type, kos_term* body) {
    if (!var_name || !type || !body) {
        return NULL;
    }
    
    kos_term* term = (kos_term*)calloc(1, sizeof(kos_term));
    if (!term) {
        return NULL;
    }
    
    term->kind = KOS_LAM;
    term->data.lam.var_name = (char*)malloc(strlen(var_name) + 1);
    if (!term->data.lam.var_name) {
        free(term);
        return NULL;
    }
    strcpy(term->data.lam.var_name, var_name);
    term->data.lam.type = type;
    term->data.lam.body = body;
    term->universe = type->universe;
    
    return term;
}

// 创建和类型 A + B（类型定义）
// Core 层约束：left_type/right_type 必须为良构类型，否则拒绝构造
kos_term* kos_mk_sum(kos_term* left_type, kos_term* right_type) {
    if (!left_type || !right_type) {
        return NULL;
    }
    if (!kos_type_wellformed(left_type) || !kos_type_wellformed(right_type)) {
        return NULL;  /* 非法类型：子类型非良构 */
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
kos_term* kos_mk_case_named(kos_term* sum_term, const char* left_var, kos_term* left_branch,
                            const char* right_var, kos_term* right_branch) {
    if (!sum_term || !left_branch || !right_branch) {
        return NULL;
    }
    kos_term* term = (kos_term*)calloc(1, sizeof(kos_term));
    if (!term) {
        return NULL;
    }
    term->kind = KOS_CASE;
    term->data.case_term.sum = sum_term;
    if (left_var) {
        term->data.case_term.left_var = (char*)malloc(strlen(left_var) + 1);
        if (!term->data.case_term.left_var) { free(term); return NULL; }
        strcpy(term->data.case_term.left_var, left_var);
    }
    if (right_var) {
        term->data.case_term.right_var = (char*)malloc(strlen(right_var) + 1);
        if (!term->data.case_term.right_var) { free(term); return NULL; }
        strcpy(term->data.case_term.right_var, right_var);
    }
    term->data.case_term.left_body = left_branch;
    term->data.case_term.right_body = right_branch;
    return term;
}

// 创建case分析（和类型消除，默认绑定 x/y）
kos_term* kos_mk_case(kos_term* sum_term, kos_term* left_branch, kos_term* right_branch) {
    return kos_mk_case_named(sum_term, "x", left_branch, "y", right_branch);
}

// 值依赖谓词：类型构造依赖项的值
static kos_term* kos_mk_pred(term_kind kind, kos_term* left, kos_term* right) {
    if (!left || !right) return NULL;
    kos_term* term = (kos_term*)calloc(1, sizeof(kos_term));
    if (!term) return NULL;
    term->kind = kind;
    term->data.pred.left = left;
    term->data.pred.right = right;
    term->universe.axis = UNIVERSE_LOGICAL;
    term->universe.level = 1;  /* Prop : Type_1 */
    return term;
}
kos_term* kos_mk_gt(kos_term* left, kos_term* right) { return kos_mk_pred(KOS_GT, left, right); }
kos_term* kos_mk_ge(kos_term* left, kos_term* right) { return kos_mk_pred(KOS_GE, left, right); }
kos_term* kos_mk_lt(kos_term* left, kos_term* right) { return kos_mk_pred(KOS_LT, left, right); }
kos_term* kos_mk_le(kos_term* left, kos_term* right) { return kos_mk_pred(KOS_LE, left, right); }
kos_term* kos_mk_eq(kos_term* left, kos_term* right) { return kos_mk_pred(KOS_EQ, left, right); }

// 创建split操作（Σ类型消除）：split(p, x.y.t)
kos_term* kos_mk_split_named(kos_term* pair_term, const char* var1, const char* var2, kos_term* body_term) {
    if (!pair_term || !body_term) {
        return NULL;
    }
    kos_term* term = (kos_term*)calloc(1, sizeof(kos_term));
    if (!term) {
        return NULL;
    }
    term->kind = KOS_SPLIT;
    term->data.split.pair = pair_term;
    if (var1) {
        term->data.split.var1 = (char*)malloc(strlen(var1) + 1);
        if (!term->data.split.var1) { free(term); return NULL; }
        strcpy(term->data.split.var1, var1);
    }
    if (var2) {
        term->data.split.var2 = (char*)malloc(strlen(var2) + 1);
        if (!term->data.split.var2) { free(term); return NULL; }
        strcpy(term->data.split.var2, var2);
    }
    term->data.split.body = body_term;
    return term;
}

// 创建split操作（Σ类型消除，默认绑定 x/y）
kos_term* kos_mk_split(kos_term* pair_term, kos_term* body_term) {
    return kos_mk_split_named(pair_term, "x", "y", body_term);
}

// 创建 Id 类型：Id(A, a, b)
kos_term* kos_mk_id_type(kos_term* type, kos_term* left, kos_term* right) {
    if (!type || !left || !right) {
        return NULL;
    }
    kos_term* term = (kos_term*)calloc(1, sizeof(kos_term));
    if (!term) {
        return NULL;
    }
    term->kind = KOS_ID_TYPE;
    term->data.id_type.type = type;
    term->data.id_type.left = left;
    term->data.id_type.right = right;
    term->universe.axis = UNIVERSE_LOGICAL;
    term->universe.level = 1;  /* 简化：Id : Type_1 */
    return term;
}

// 创建 refl：refl(a)
kos_term* kos_mk_refl(kos_term* value) {
    if (!value) {
        return NULL;
    }
    kos_term* term = (kos_term*)calloc(1, sizeof(kos_term));
    if (!term) {
        return NULL;
    }
    term->kind = KOS_REFL;
    term->data.refl.value = value;
    term->universe.axis = UNIVERSE_LOGICAL;
    term->universe.level = 1;
    return term;
}

// 创建 let：let x : A := v in t
kos_term* kos_mk_let(const char* var_name, kos_term* type, kos_term* value, kos_term* body) {
    if (!var_name || !type || !value || !body) {
        return NULL;
    }
    kos_term* term = (kos_term*)calloc(1, sizeof(kos_term));
    if (!term) {
        return NULL;
    }
    term->kind = KOS_LET;
    term->data.let_term.var_name = (char*)malloc(strlen(var_name) + 1);
    if (!term->data.let_term.var_name) {
        free(term);
        return NULL;
    }
    strcpy(term->data.let_term.var_name, var_name);
    term->data.let_term.type = type;
    term->data.let_term.value = value;
    term->data.let_term.body = body;
    term->universe = type->universe;
    return term;
}

/** 深拷贝 term，递归复制所有子结构。调用者负责 kos_term_free。 */
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
            if (t->data.sigma.var_name) {
                copy->data.sigma.var_name = (char*)malloc(strlen(t->data.sigma.var_name) + 1);
                if (copy->data.sigma.var_name) strcpy(copy->data.sigma.var_name, t->data.sigma.var_name);
            }
            copy->data.sigma.domain = kos_term_copy(t->data.sigma.domain);
            copy->data.sigma.body = kos_term_copy(t->data.sigma.body);
            copy->universe = t->universe;
            break;
            
        case KOS_PI:
            if (t->data.pi.var_name) {
                copy->data.pi.var_name = (char*)malloc(strlen(t->data.pi.var_name) + 1);
                if (copy->data.pi.var_name) strcpy(copy->data.pi.var_name, t->data.pi.var_name);
            }
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
            
        case KOS_GT:
        case KOS_GE:
        case KOS_LT:
        case KOS_LE:
        case KOS_EQ:
            copy->data.pred.left = kos_term_copy(t->data.pred.left);
            copy->data.pred.right = kos_term_copy(t->data.pred.right);
            copy->universe = t->universe;
            break;
            
        case KOS_APP:
            copy->data.app.func = kos_term_copy(t->data.app.func);
            copy->data.app.arg = kos_term_copy(t->data.app.arg);
            copy->universe = t->universe;
            break;
            
        case KOS_LAM:
            if (t->data.lam.var_name) {
                copy->data.lam.var_name = (char*)malloc(strlen(t->data.lam.var_name) + 1);
                if (copy->data.lam.var_name) strcpy(copy->data.lam.var_name, t->data.lam.var_name);
            }
            copy->data.lam.type = kos_term_copy(t->data.lam.type);
            copy->data.lam.body = kos_term_copy(t->data.lam.body);
            copy->universe = t->universe;
            break;
        case KOS_SPLIT:
            if (t->data.split.var1) {
                copy->data.split.var1 = (char*)malloc(strlen(t->data.split.var1) + 1);
                if (copy->data.split.var1) strcpy(copy->data.split.var1, t->data.split.var1);
            }
            if (t->data.split.var2) {
                copy->data.split.var2 = (char*)malloc(strlen(t->data.split.var2) + 1);
                if (copy->data.split.var2) strcpy(copy->data.split.var2, t->data.split.var2);
            }
            copy->data.split.pair = kos_term_copy(t->data.split.pair);
            copy->data.split.body = kos_term_copy(t->data.split.body);
            copy->universe = t->universe;
            break;
        case KOS_CASE:
            if (t->data.case_term.left_var) {
                copy->data.case_term.left_var = (char*)malloc(strlen(t->data.case_term.left_var) + 1);
                if (copy->data.case_term.left_var) strcpy(copy->data.case_term.left_var, t->data.case_term.left_var);
            }
            if (t->data.case_term.right_var) {
                copy->data.case_term.right_var = (char*)malloc(strlen(t->data.case_term.right_var) + 1);
                if (copy->data.case_term.right_var) strcpy(copy->data.case_term.right_var, t->data.case_term.right_var);
            }
            copy->data.case_term.sum = kos_term_copy(t->data.case_term.sum);
            copy->data.case_term.left_body = kos_term_copy(t->data.case_term.left_body);
            copy->data.case_term.right_body = kos_term_copy(t->data.case_term.right_body);
            copy->universe = t->universe;
            break;
        case KOS_ID_TYPE:
            copy->data.id_type.type = kos_term_copy(t->data.id_type.type);
            copy->data.id_type.left = kos_term_copy(t->data.id_type.left);
            copy->data.id_type.right = kos_term_copy(t->data.id_type.right);
            copy->universe = t->universe;
            break;
        case KOS_REFL:
            copy->data.refl.value = kos_term_copy(t->data.refl.value);
            copy->universe = t->universe;
            break;
        case KOS_LET:
            if (t->data.let_term.var_name) {
                copy->data.let_term.var_name = (char*)malloc(strlen(t->data.let_term.var_name) + 1);
                if (copy->data.let_term.var_name) strcpy(copy->data.let_term.var_name, t->data.let_term.var_name);
            }
            copy->data.let_term.type = kos_term_copy(t->data.let_term.type);
            copy->data.let_term.value = kos_term_copy(t->data.let_term.value);
            copy->data.let_term.body = kos_term_copy(t->data.let_term.body);
            copy->universe = t->universe;
            break;
    }
    
    return copy;
}

/** 递归释放 term 及所有子项。 */
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
            if (t->data.sigma.var_name) free(t->data.sigma.var_name);
            if (t->data.sigma.domain) {
                kos_term_free(t->data.sigma.domain);
            }
            if (t->data.sigma.body) {
                kos_term_free(t->data.sigma.body);
            }
            break;
            
        case KOS_PI:
            if (t->data.pi.var_name) free(t->data.pi.var_name);
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
        case KOS_GT:
        case KOS_GE:
        case KOS_LT:
        case KOS_LE:
        case KOS_EQ:
            if (t->data.pred.left) kos_term_free(t->data.pred.left);
            if (t->data.pred.right) kos_term_free(t->data.pred.right);
            break;
            
        case KOS_APP:
            if (t->data.app.func) kos_term_free(t->data.app.func);
            if (t->data.app.arg) kos_term_free(t->data.app.arg);
            break;
            
        case KOS_LAM:
            if (t->data.lam.var_name) free(t->data.lam.var_name);
            if (t->data.lam.type) kos_term_free(t->data.lam.type);
            if (t->data.lam.body) kos_term_free(t->data.lam.body);
            break;
        case KOS_ID_TYPE:
            if (t->data.id_type.type) kos_term_free(t->data.id_type.type);
            if (t->data.id_type.left) kos_term_free(t->data.id_type.left);
            if (t->data.id_type.right) kos_term_free(t->data.id_type.right);
            break;
        case KOS_REFL:
            if (t->data.refl.value) kos_term_free(t->data.refl.value);
            break;
        case KOS_LET:
            if (t->data.let_term.var_name) free(t->data.let_term.var_name);
            if (t->data.let_term.type) kos_term_free(t->data.let_term.type);
            if (t->data.let_term.value) kos_term_free(t->data.let_term.value);
            if (t->data.let_term.body) kos_term_free(t->data.let_term.body);
            break;
        case KOS_SPLIT:
            if (t->data.split.var1) free(t->data.split.var1);
            if (t->data.split.var2) free(t->data.split.var2);
            if (t->data.split.pair) kos_term_free(t->data.split.pair);
            if (t->data.split.body) kos_term_free(t->data.split.body);
            break;
        case KOS_CASE:
            if (t->data.case_term.left_var) free(t->data.case_term.left_var);
            if (t->data.case_term.right_var) free(t->data.case_term.right_var);
            if (t->data.case_term.sum) kos_term_free(t->data.case_term.sum);
            if (t->data.case_term.left_body) kos_term_free(t->data.case_term.left_body);
            if (t->data.case_term.right_body) kos_term_free(t->data.case_term.right_body);
            break;
    }
    
    free(t);
}




