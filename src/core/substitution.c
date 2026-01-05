// src/core/substitution.c
// 替换操作：t[u/x] - 在项t中将变量x替换为项u
// 这是归约操作的基础

#include "kos_core.h"
#include <stdlib.h>
#include <string.h>

// 替换操作：t[u/x]
// 在项t中，将所有自由出现的变量x替换为项u
// 注意：这是简化实现，实际需要处理变量捕获等问题
kos_term* kos_substitute(kos_term* t, const char* var_name, kos_term* u) {
    if (!t || !var_name || !u) {
        return t; // 如果参数无效，返回原项
    }
    
    // 对于原子项（VAL, TIME, ID, PROP），如果值匹配变量名，则替换
    if (t->kind == KOS_VAL || t->kind == KOS_TIME || t->kind == KOS_ID || t->kind == KOS_PROP) {
        if (t->data.atomic.val && strcmp(t->data.atomic.val, var_name) == 0) {
            // 找到变量，返回替换项u的副本
            return kos_term_copy(u);
        }
        // 否则递归处理类型字段
        if (t->data.atomic.type) {
            kos_term* new_type = kos_substitute(t->data.atomic.type, var_name, u);
            if (new_type != t->data.atomic.type) {
                kos_term* new_term = kos_term_copy(t);
                kos_term_free(new_term->data.atomic.type);
                new_term->data.atomic.type = new_type;
                return new_term;
            }
        }
        return kos_term_copy(t);
    }
    
    // 对于PAIR类型，递归替换两个组件
    if (t->kind == KOS_PAIR) {
        kos_term* new_data = kos_substitute(t->data.pair.data, var_name, u);
        kos_term* new_proof = kos_substitute(t->data.pair.proof, var_name, u);
        
        if (new_data != t->data.pair.data || new_proof != t->data.pair.proof) {
            kos_term* new_term = kos_term_copy(t);
            kos_term_free(new_term->data.pair.data);
            kos_term_free(new_term->data.pair.proof);
            new_term->data.pair.data = new_data;
            new_term->data.pair.proof = new_proof;
            return new_term;
        }
        return kos_term_copy(t);
    }
    
    // 对于SIGMA类型，递归替换domain和body
    if (t->kind == KOS_SIGMA) {
        kos_term* new_domain = kos_substitute(t->data.sigma.domain, var_name, u);
        kos_term* new_body = kos_substitute(t->data.sigma.body, var_name, u);
        
        if (new_domain != t->data.sigma.domain || new_body != t->data.sigma.body) {
            kos_term* new_term = kos_term_copy(t);
            kos_term_free(new_term->data.sigma.domain);
            kos_term_free(new_term->data.sigma.body);
            new_term->data.sigma.domain = new_domain;
            new_term->data.sigma.body = new_body;
            return new_term;
        }
        return kos_term_copy(t);
    }
    
    // 对于PI类型（函数类型或λ抽象），需要处理变量绑定
    if (t->kind == KOS_PI) {
        // 如果替换的变量名与绑定变量冲突，需要α转换（这里简化处理）
        // 简化实现：只替换body和body_term，不替换domain中的绑定变量
        kos_term* new_domain = kos_term_copy(t->data.pi.domain);
        kos_term* new_body = t->data.pi.body ? kos_substitute(t->data.pi.body, var_name, u) : NULL;
        kos_term* new_body_term = t->data.pi.body_term ? kos_substitute(t->data.pi.body_term, var_name, u) : NULL;
        
        if (new_body != t->data.pi.body || new_body_term != t->data.pi.body_term) {
            kos_term* new_term = kos_term_copy(t);
            kos_term_free(new_term->data.pi.domain);
            if (new_term->data.pi.body) kos_term_free(new_term->data.pi.body);
            if (new_term->data.pi.body_term) kos_term_free(new_term->data.pi.body_term);
            new_term->data.pi.domain = new_domain;
            new_term->data.pi.body = new_body;
            new_term->data.pi.body_term = new_body_term;
            return new_term;
        }
        kos_term_free(new_domain);
        return kos_term_copy(t);
    }
    
    // 对于SUM类型，递归替换类型和值
    if (t->kind == KOS_SUM) {
        kos_term* new_left = kos_substitute(t->data.sum.left_type, var_name, u);
        kos_term* new_right = kos_substitute(t->data.sum.right_type, var_name, u);
        kos_term* new_value = t->data.sum.value ? kos_substitute(t->data.sum.value, var_name, u) : NULL;
        
        if (new_left != t->data.sum.left_type || new_right != t->data.sum.right_type || 
            new_value != t->data.sum.value) {
            kos_term* new_term = kos_term_copy(t);
            kos_term_free(new_term->data.sum.left_type);
            kos_term_free(new_term->data.sum.right_type);
            if (new_term->data.sum.value) kos_term_free(new_term->data.sum.value);
            new_term->data.sum.left_type = new_left;
            new_term->data.sum.right_type = new_right;
            new_term->data.sum.value = new_value;
            return new_term;
        }
        return kos_term_copy(t);
    }
    
    // 对于Universe类型（U, TYPE），无需替换（Universe类型不包含变量）
    if (t->kind == KOS_U || t->kind == KOS_TYPE) {
        return kos_term_copy(t);
    }
    
    // 默认情况：复制原项
    return kos_term_copy(t);
}

