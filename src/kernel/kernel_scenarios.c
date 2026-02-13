/**
 * @file kernel_scenarios.c
 * @brief KOS-TL Kernel 层核心应用场景实现
 *
 * 实现根因追溯、反事实推理、合规性决策、审计问责、系统治理、AI 治理等场景 API。
 */

#include "kos_kernel_scenarios.h"
#include "kos_kernel_session.h"
#include "kos_trace.h"
#include "kos_core_bridge.h"
#include "kos_core.h"
#include "kos_knowledge_base.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

/* ========== 场景 2：反事实推理 ========== */

int kos_kernel_counterfactual_test(const char* ctx_factual,
                                   const char* ctx_counterfactual,
                                   const char* goal_type,
                                   kos_counterfactual_result_t* result) {
    if (!ctx_factual || !ctx_counterfactual || !goal_type || !result) {
        return -1;
    }
    memset(result, 0, sizeof(*result));

    char err[512];
    result->factual_provable = kos_core_bridge_prove(ctx_factual, goal_type, err, sizeof(err));
    result->counterfactual_provable = kos_core_bridge_prove(ctx_counterfactual, goal_type, err, sizeof(err));
    result->excluded_necessary = result->factual_provable && !result->counterfactual_provable;
    return 0;
}

/* ========== 场景 3：合规性决策 ========== */

bool kos_kernel_verify_decision_compliance(kos_state_t* sigma, kos_term* event_pair) {
    if (!sigma || !event_pair) return false;
    return kos_verify_precondition(event_pair, sigma);
}

int kos_kernel_session_enqueue_if_compliant(kos_kernel_session_t* session,
                                            kos_term* event_pair) {
    if (!session || !event_pair) return -1;
    kos_state_t* sigma = kos_kernel_session_get_sigma(session);
    if (!sigma) return -1;
    if (!kos_kernel_verify_decision_compliance(sigma, event_pair)) {
        return -1;  /* 不合规 */
    }
    return kos_kernel_session_enqueue_event(session, event_pair);
}

/* ========== 场景 4：审计与问责 ========== */

int kos_trace_replay_into(const kos_trace_t* trace, kos_state_t* target_sigma) {
    if (!trace || !target_sigma) return -1;
    size_t n = kos_trace_length(trace);
    int success = 0;
    for (size_t i = 0; i < n; i++) {
        const kos_trace_step_t* step = kos_trace_get_step(trace, i);
        if (!step || !step->event_pair) continue;
        if (kos_kernel_step(target_sigma, step->event_pair)) {
            success++;
        } else {
            return (int)success;  /* 某步失败，返回已成功数 */
        }
    }
    return (int)success;
}

char* kos_trace_export_audit_json(const kos_trace_t* trace) {
    if (!trace) return NULL;

    size_t cap = 16384;
    char* buf = (char*)malloc(cap);
    if (!buf) return NULL;
    size_t pos = 0;

    kos_serialized* ser = NULL;
    pos += (size_t)snprintf(buf + pos, cap - pos, "{\"steps\":[");
    for (size_t i = 0; i < trace->count && pos < cap - 256; i++) {
        const kos_trace_step_t* step = &trace->steps[i];
        if (i > 0) buf[pos++] = ',';
        if (step->event_pair) {
            ser = kos_term_serialize(step->event_pair);
            if (ser && ser->data) {
                pos += (size_t)snprintf(buf + pos, cap - pos,
                    "{\"index\":%u,\"ts_after\":%d,\"event_pair\":%s}",
                    (unsigned)(i), step->ts_after, ser->data);
                kos_serialized_free(ser);
                ser = NULL;
            } else {
                pos += (size_t)snprintf(buf + pos, cap - pos,
                    "{\"index\":%u,\"ts_after\":%d,\"event_pair\":null}", (unsigned)(i), step->ts_after);
            }
        } else {
            pos += (size_t)snprintf(buf + pos, cap - pos,
                "{\"index\":%u,\"ts_after\":%d,\"event_pair\":null}", (unsigned)(i), step->ts_after);
        }
    }
    pos += (size_t)snprintf(buf + pos, cap - pos, "]}");
    buf[pos] = '\0';
    return buf;
}

int kos_kernel_session_export_audit_trail(kos_kernel_session_t* session,
                                          const char* filepath) {
    if (!session || !filepath) return -1;
    kos_trace_t* trace = kos_kernel_session_get_trace(session);
    if (!trace) return -1;
    char* json = kos_trace_export_audit_json(trace);
    if (!json) return -1;
    FILE* f = fopen(filepath, "w");
    if (!f) {
        free(json);
        return -1;
    }
    fputs(json, f);
    fclose(f);
    free(json);
    return 0;
}

/* ========== 场景 5：复杂系统治理 ========== */

size_t kos_kernel_session_evolve_until_idle(kos_kernel_session_t* session) {
    if (!session) return 0;
    size_t steps = 0;
    while (kos_kernel_session_step(session)) {
        steps++;
    }
    return steps;
}

bool kos_kernel_state_is_legal(const kos_state_t* sigma) {
    if (!sigma) return false;
    return sigma->K != NULL;
}

/* ========== 场景 6：AI 治理 ========== */

bool kos_kernel_verify_ai_suggestion(const char* ctx_kos_path,
                                     const char* suggestion_kos_expr,
                                     const char* expected_type_expr) {
    if (!suggestion_kos_expr || !expected_type_expr) return false;
    char err[512];
    /* kos_core_bridge_check_expr 检查 term : type，空上下文 */
    /* 若有 ctx，需 kos-core 支持 check-term --ctx；当前 bridge 无此 API */
    /* 简化：使用 check-expr 空上下文校验；若有 ctx 需求可后续扩展 */
    (void)ctx_kos_path;
    return kos_core_bridge_check_expr(suggestion_kos_expr, expected_type_expr, err, sizeof(err));
}
