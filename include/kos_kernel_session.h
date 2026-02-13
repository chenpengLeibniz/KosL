/**
 * @file kos_kernel_session.h
 * @brief KOS Kernel 层统一会话：Γ、σ、trace 及知识操作
 *
 * 对应 monograph 第 8 章「核心层：证明构造与小步演化」：
 *   - Γ (Gamma): 类型与谓词集合，来自 .kos 本体
 *   - σ (sigma): 知识状态，包含类型实例（叶子项）K、逻辑时钟 TS、事件队列 P
 *   - trace: σ 的演化轨迹 T = σ₀ →^⟨e₁,p₁⟩ σ₁ → …
 *
 * 知识操作：
 *   - 从 runtime 获取事件实例，执行小步演化
 *   - 调用 kos-core prove 查找根因（RootCauseReport）
 *   - 跟踪知识演化轨迹
 */

#ifndef KOS_KERNEL_SESSION_H
#define KOS_KERNEL_SESSION_H

#include "kos_kernel.h"
#include "kos_kernel_context.h"
#include "kos_trace.h"
#include "kos_knowledge_base.h"
#include "kos_core_bridge.h"
#include <stddef.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/* ========== 统一会话（不透明类型） ==========
 * 整合 Γ、σ、trace，提供一站式知识操作 */

typedef struct kos_kernel_session kos_kernel_session_t;

/* ========== 会话生命周期 ========== */

/** 创建空会话。ctx_kos_path 可为 NULL，后续通过 kos_kernel_session_load_ontology 加载。 */
kos_kernel_session_t* kos_kernel_session_create(const char* ctx_kos_path);

/** 释放会话及所有内部资源。 */
void kos_kernel_session_free(kos_kernel_session_t* session);

/** 从 .kos 文件加载本体到 Γ（类型、谓词、构造子）。返回 0 成功。 */
int kos_kernel_session_load_ontology(kos_kernel_session_t* session, const char* kos_path);

/* ========== 访问器 ========== */

/** 获取 Γ */
kos_kernel_context_t* kos_kernel_session_get_gamma(const kos_kernel_session_t* session);

/** 获取 σ */
kos_state_t* kos_kernel_session_get_sigma(const kos_kernel_session_t* session);

/** 获取 trace */
kos_trace_t* kos_kernel_session_get_trace(const kos_kernel_session_t* session);

/** 获取知识库 KB（σ 中的实例视图，可能为 NULL） */
kos_knowledge_base_t* kos_kernel_session_get_kb(const kos_kernel_session_t* session);

/* ========== 知识操作 ========== */

/** 喂入事件对 ⟨e, p⟩，加入 σ 的队列 P。返回 0 成功。 */
int kos_kernel_session_enqueue_event(kos_kernel_session_t* session, kos_term* event_pair);

/** 执行一步演化：从队列取事件，Verify → Reduce → Update(K, TS) → 追记 trace。返回 true 成功。 */
bool kos_kernel_session_step(kos_kernel_session_t* session);

/** 查找根因：在 ctx_kos_path 上下文中对 goal_type 做证明搜索。
 * 若 proof_out 非 NULL，则通过 kos-core prove-json 返回证明项（调用者 kos_term_free）。
 * 返回 true 当且仅当找到证明。 */
bool kos_kernel_session_find_root_cause(kos_kernel_session_t* session,
                                        const char* ctx_kos_path,
                                        const char* goal_type,
                                        kos_term** proof_out,
                                        char* errmsg, size_t errmsg_size);

/** 简化版：仅检查是否能证明，不返回证明项。 */
bool kos_kernel_session_can_prove(kos_kernel_session_t* session,
                                  const char* ctx_kos_path,
                                  const char* goal_type);

#ifdef __cplusplus
}
#endif

#endif /* KOS_KERNEL_SESSION_H */
