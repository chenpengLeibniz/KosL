/**
 * @file kos_kernel_scenarios.h
 * @brief KOS-TL Kernel 层核心应用场景 API
 *
 * 支持六大核心场景：
 *   1. 根因追溯 (Root Cause Analysis)       - 回溯轨迹，形式化定位根因
 *   2. 反事实推理 (Counterfactual Reasoning) - 合法假设与可构造性推导
 *   3. 合规性决策系统 (Normative Decision)   - 每个决策可被 trace 证明合法性
 *   4. 审计与问责 (Audit and Accountability) - 可重放的证明，责任归属
 *   5. 复杂系统治理 (Complex Systems Gov.)   - 状态演化的合法性定义与追溯
 *   6. AI 治理与可信性 (AI Governance)       - AI 建议在规范范围内的可验证性
 */

#ifndef KOS_KERNEL_SCENARIOS_H
#define KOS_KERNEL_SCENARIOS_H

#include "kos_kernel.h"
#include "kos_kernel_session.h"
#include "kos_trace.h"
#include "kos_core.h"
#include <stddef.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/* =============================================================================
 * 场景 1：根因追溯 (Root Cause Analysis)
 * =============================================================================
 * 通过回溯执行轨迹精确定位根因，trace 证明每个操作的合规性。
 * 见 kos_kernel_session_find_root_cause()。
 */

/* =============================================================================
 * 场景 2：反事实推理 (Counterfactual Reasoning)
 * =============================================================================
 * "若 X 未发生，Y 是否仍会发生？" — 形式化验证的假设性分析。
 */

/** 反事实测试结果 */
typedef struct kos_counterfactual_result {
    bool factual_provable;       /* 事实场景下 goal 可证明 */
    bool counterfactual_provable;/* 反事实场景（排除某变量）下 goal 可证明 */
    bool excluded_necessary;     /* 排除的变量是因果必要的 (factual && !counterfactual) */
} kos_counterfactual_result_t;

/** 反事实推理：对比事实上下文与反事实上下文（排除 exclude_var）的证明结果。
 * ctx_factual: 完整上下文 .kos 路径
 * ctx_counterfactual: 排除某变量后的上下文 .kos 路径（如不含某异常）
 * goal_type: 目标类型（如 RootCauseReport）
 * 返回 0 成功，-1 失败；result 输出反事实结论 */
int kos_kernel_counterfactual_test(const char* ctx_factual,
                                   const char* ctx_counterfactual,
                                   const char* goal_type,
                                   kos_counterfactual_result_t* result);

/* =============================================================================
 * 场景 3：合规性决策系统 (Normative Decision Systems)
 * =============================================================================
 * 决策前验证合规性，每个决策背后有合法的 trace 证明。
 */

/** 决策合规性验证结果 */
typedef struct kos_decision_compliance {
    bool pre_holds;      /* 前置条件 Pre(e) 满足 */
    bool would_evolve;   /* 若执行，演化可成功 */
} kos_decision_compliance_t;

/** 验证决策/事件在当前状态下是否合规（Pre 是否成立）。
 * 返回 true 当且仅当 kos_verify_precondition 通过。 */
bool kos_kernel_verify_decision_compliance(kos_state_t* sigma, kos_term* event_pair);

/** 带合规性检查的入队：仅当 Pre 成立时才入队。返回 0 成功，-1 不合规或失败。 */
int kos_kernel_session_enqueue_if_compliant(kos_kernel_session_t* session,
                                            kos_term* event_pair);

/* =============================================================================
 * 场景 4：审计与问责 (Audit and Accountability)
 * =============================================================================
 * 可重放的证明，责任归属，支持监管合规与法律系统。
 */

/** 轨迹重放到已有 sigma：将 trace 中的步骤按序重放到目标 sigma。
 * 用于审计时复现演化过程。返回成功的步数，失败时返回 -1。
 * （MTK 确定性重放：使用 kos_trace_replay(trace, initial_K) 从初始 K 得到新 state。） */
int kos_trace_replay_into(const kos_trace_t* trace, kos_state_t* target_sigma);

/** 导出审计轨迹为 JSON 字符串（调用者 free）。
 * 格式：{ "steps": [ { "index", "ts_after", "event_pair_json" } ] }
 * 若 store_K_snapshot 为 true，每步可含 "K_snapshot"。 */
char* kos_trace_export_audit_json(const kos_trace_t* trace);

/** 将会话的 trace 导出为审计文件。返回 0 成功。 */
int kos_kernel_session_export_audit_trail(kos_kernel_session_t* session,
                                          const char* filepath);

/* =============================================================================
 * 场景 5：复杂系统治理 (Complex Systems Governance)
 * =============================================================================
 * 精确定义每步操作的合法性，追溯每个决策。
 */

/** 执行演化直到队列空，确保每步均合法。
 * 某步失败则停止，返回成功步数。 */
size_t kos_kernel_session_evolve_until_idle(kos_kernel_session_t* session);

/** 检查当前 sigma 是否处于合法状态（K 非空或可接受）。
 * 简化实现：K 存在即认为合法。 */
bool kos_kernel_state_is_legal(const kos_state_t* sigma);

/* =============================================================================
 * 场景 6：AI 治理与可信性 (AI Governance and Trustworthy AI)
 * =============================================================================
 * 确保 AI 决策在规范范围内，可审查与验证。
 */

/** 验证 AI 建议项是否符合目标类型（在规范范围内）。
 * 使用 kos-core check-term 验证 suggestion_term : expected_type。
 * ctx_kos_path: 规范上下文，NULL 则用空上下文。
 * 返回 true 当且仅当建议在规范范围内。 */
bool kos_kernel_verify_ai_suggestion(const char* ctx_kos_path,
                                     const char* suggestion_kos_expr,
                                     const char* expected_type_expr);

#ifdef __cplusplus
}
#endif

#endif /* KOS_KERNEL_SCENARIOS_H */
