#include "kos_core.h"
#include <stdlib.h>
#include <string.h>

// 前向声明：替换函数（在substitution.c中实现）
extern kos_term* kos_substitute(kos_term* t, const char* var_name, kos_term* u);

// 归约函数：对项进行归约操作 [cite: 610]
// 实现β-reduction和ι-reduction规则
// 根据文档：使用 → 表示单步归约，使用 →* 表示多步归约（计算闭包）
kos_term* kos_reduce(kos_term* t) {
    if (!t) {
        return NULL;
    }
    
    // ========== β-reduction: (λx:A.t) u → t[u/x] ==========
    // 检查是否为函数应用（使用PAIR标记的应用，其中data是函数，proof是参数）
    if (t->kind == KOS_PAIR && t->data.pair.data && t->data.pair.proof) {
        kos_term* func = t->data.pair.data;
        kos_term* arg = t->data.pair.proof;
        
        // 先递归归约函数和参数（弱头归约）
        kos_term* reduced_func = kos_reduce(func);
        if (reduced_func != func) {
            kos_term_free(t->data.pair.data);
            t->data.pair.data = reduced_func;
            func = reduced_func;
        }
        
        kos_term* reduced_arg = kos_reduce(arg);
        if (reduced_arg != arg) {
            kos_term_free(t->data.pair.proof);
            t->data.pair.proof = reduced_arg;
            arg = reduced_arg;
        }
        
        // 如果函数是λ抽象（KOS_PI类型且包含body_term）
        if (func->kind == KOS_PI && func->data.pi.body_term) {
            // 执行β-reduction：替换body_term中的变量
            // 根据文档规则：(\lambda x:A.t) u → t[u/x]
            // 注意：这里简化处理，使用"x"作为变量名，实际需要从domain中提取绑定变量名
            kos_term* reduced = kos_substitute(func->data.pi.body_term, "x", arg);
            kos_term_free(t);
            return reduced;
        }
    }
    
    // ========== ι-reduction for Σ: split(<u, v>, x.y.t) → t[u/x, v/y] ==========
    // 注意：当前数据结构中split使用SIGMA标记，需要改进数据结构
    // 这里暂时简化处理，实际的split操作需要更复杂的数据结构
    
    // ========== ι-reduction for +: case(inl(u), x.t, y.v) → t[u/x] ==========
    // case(inr(w), x.t, y.v) → v[w/y]
    // 注意：当前数据结构中case使用SIGMA标记，需要改进数据结构
    // 这里暂时简化处理
    
    // ========== 递归归约子项（弱头归约策略）==========
    switch (t->kind) {
        case KOS_PAIR:
            // 对 pair 的数据部分和证明部分进行归约
            if (t->data.pair.data) {
                kos_term* reduced_data = kos_reduce(t->data.pair.data);
                if (reduced_data != t->data.pair.data) {
                    kos_term_free(t->data.pair.data);
                    t->data.pair.data = reduced_data;
                }
            }
            if (t->data.pair.proof) {
                kos_term* reduced_proof = kos_reduce(t->data.pair.proof);
                if (reduced_proof != t->data.pair.proof) {
                    kos_term_free(t->data.pair.proof);
                    t->data.pair.proof = reduced_proof;
                }
            }
            break;
            
        case KOS_SIGMA:
            // 对 sigma 的域和体进行归约
            if (t->data.sigma.domain) {
                kos_term* reduced_domain = kos_reduce(t->data.sigma.domain);
                if (reduced_domain != t->data.sigma.domain) {
                    kos_term_free(t->data.sigma.domain);
                    t->data.sigma.domain = reduced_domain;
                }
            }
            if (t->data.sigma.body) {
                kos_term* reduced_body = kos_reduce(t->data.sigma.body);
                if (reduced_body != t->data.sigma.body) {
                    kos_term_free(t->data.sigma.body);
                    t->data.sigma.body = reduced_body;
                }
            }
            break;
            
        case KOS_PI:
            // 对PI类型的domain和body_term进行归约
            if (t->data.pi.domain) {
                kos_term* reduced_domain = kos_reduce(t->data.pi.domain);
                if (reduced_domain != t->data.pi.domain) {
                    kos_term_free(t->data.pi.domain);
                    t->data.pi.domain = reduced_domain;
                }
            }
            if (t->data.pi.body) {
                kos_term* reduced_body = kos_reduce(t->data.pi.body);
                if (reduced_body != t->data.pi.body) {
                    kos_term_free(t->data.pi.body);
                    t->data.pi.body = reduced_body;
                }
            }
            if (t->data.pi.body_term) {
                kos_term* reduced_body_term = kos_reduce(t->data.pi.body_term);
                if (reduced_body_term != t->data.pi.body_term) {
                    kos_term_free(t->data.pi.body_term);
                    t->data.pi.body_term = reduced_body_term;
                }
            }
            break;
            
        case KOS_SUM:
            // 对SUM类型的类型和值进行归约
            if (t->data.sum.left_type) {
                kos_term* reduced_left = kos_reduce(t->data.sum.left_type);
                if (reduced_left != t->data.sum.left_type) {
                    kos_term_free(t->data.sum.left_type);
                    t->data.sum.left_type = reduced_left;
                }
            }
            if (t->data.sum.right_type) {
                kos_term* reduced_right = kos_reduce(t->data.sum.right_type);
                if (reduced_right != t->data.sum.right_type) {
                    kos_term_free(t->data.sum.right_type);
                    t->data.sum.right_type = reduced_right;
                }
            }
            if (t->data.sum.value) {
                kos_term* reduced_value = kos_reduce(t->data.sum.value);
                if (reduced_value != t->data.sum.value) {
                    kos_term_free(t->data.sum.value);
                    t->data.sum.value = reduced_value;
                }
            }
            break;
            
        case KOS_VAL:
        case KOS_PROP:
        default:
            // 原子项无需归约
            break;
    }
    
    return t;
}

// 归约并更新知识集
kos_term* kos_reduce_update(kos_term* K, kos_term* e) {
    if (!e) {
        return K;
    }
    
    // 简化实现：将归约后的事件添加到知识集
    // 实际实现中，这里应该是一个更复杂的数据结构操作
    kos_term* reduced_e = kos_reduce(e);
    (void)reduced_e; // 暂时未使用归约结果
    
    // 这里应该实现一个更复杂的知识集更新逻辑
    // 暂时返回原知识集（实际应该合并新知识）
    return K;
}

