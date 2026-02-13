/**
 * @file kos_kernel_context.h
 * @brief KOS Kernel 层上下文 Γ（Gamma）：类型与谓词集合
 *
 * 对应 monograph 第 8 章「核心层：证明构造与小步演化」及证明搜索机（PSM）配置：
 *   配置 ⟨Γ; σ; G; Δ⟩ 中 Γ 为类型上下文与已选取的见证，
 *   包含所有预定义的类型、公理和函数签名（如 FailEvt, Anomaly, ProcStep,
 *   TimeOK, SpaceOK, BatchOK, mkCausalProof 等）。
 *
 * Γ 与 σ 分离：Γ 是静态的“合法性边界”，σ 是动态的知识库（事实集合）。
 * 证明搜索与类型检查时以 Γ 为类型/谓词环境，以 σ 提供候选（getProcSteps, getAnomalies）。
 */

#ifndef KOS_KERNEL_CONTEXT_H
#define KOS_KERNEL_CONTEXT_H

#include "kos_core.h"
#include <stddef.h>
#include <stdbool.h>

/* ========== Γ 绑定项：名称 -> 类型或定义 ========== */

typedef struct kos_gamma_binding {
    char* name;           /* 名称（类型名、谓词名、构造子名） */
    kos_term* type;       /* 类型或签名（深拷贝持有） */
    kos_term* body;       /* 可选：定义体（如构造子实现），NULL 表示仅类型声明 */
    struct kos_gamma_binding* next;
} kos_gamma_binding_t;

/* ========== Γ 上下文 ========== */

typedef struct kos_kernel_context {
    kos_gamma_binding_t* bindings;  /* 链表：类型与谓词集合 */
    size_t count;
} kos_kernel_context_t;

/* 类型别名：与书中 Γ 对应 */
typedef kos_kernel_context_t kos_gamma_t;

/* ========== API ========== */

/** 创建空上下文 Γ */
kos_kernel_context_t* kos_gamma_create(void);

/** 释放 Γ 及所有绑定（深拷贝的 term 会一并释放） */
void kos_gamma_free(kos_kernel_context_t* gamma);

/** 添加类型或谓词声明：name : type（body 可为 NULL） */
int kos_gamma_add(kos_kernel_context_t* gamma, const char* name, kos_term* type, kos_term* body);

/** 仅添加类型声明（无 body） */
int kos_gamma_add_type(kos_kernel_context_t* gamma, const char* name, kos_term* type);

/** 查找名称对应的类型，不存在返回 NULL（不转移所有权） */
const kos_term* kos_gamma_lookup_type(const kos_kernel_context_t* gamma, const char* name);

/** 查找名称对应的定义体，不存在或仅类型声明则返回 NULL */
const kos_term* kos_gamma_lookup_body(const kos_kernel_context_t* gamma, const char* name);

/** 绑定数量 */
size_t kos_gamma_count(const kos_kernel_context_t* gamma);

#endif /* KOS_KERNEL_CONTEXT_H */
