/**
 * @file kernel_session.c
 * @brief KOS Kernel 统一会话：Γ、σ、trace 及知识操作
 *
 * 整合 kernel_context (Γ)、state (σ)、trace，提供：
 *   - 事件喂入与小步演化
 *   - 调用 kos-core prove 查找根因
 *   - 知识演化轨迹跟踪
 */

#include "kos_kernel_session.h"
#include "kos_kernel.h"
#include "kos_knowledge_base.h"
#include "kos_trace.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

struct kos_kernel_session {
    kos_kernel_context_t* gamma;
    kos_state_t* sigma;
    char* ctx_kos_path;  /* 本体 .kos 路径，用于 prove */
};

kos_kernel_session_t* kos_kernel_session_create(const char* ctx_kos_path) {
    kos_kernel_session_t* s = (kos_kernel_session_t*)calloc(1, sizeof(kos_kernel_session_t));
    if (!s) return NULL;

    s->gamma = kos_gamma_create();
    if (!s->gamma) {
        free(s);
        return NULL;
    }

    s->sigma = kos_state_create(NULL);
    if (!s->sigma) {
        kos_gamma_free(s->gamma);
        free(s);
        return NULL;
    }

    /* 创建 KB 与 trace，挂载到 σ */
    kos_knowledge_base_t* kb = kos_kb_create();
    kos_trace_t* trace = kos_trace_create(false);  /* 不保存 K 快照以省内存 */
    if (!trace) {
        if (kb) kos_kb_free(kb);
        kos_state_free(s->sigma);
        kos_gamma_free(s->gamma);
        free(s);
        return NULL;
    }
    kos_state_set_kb(s->sigma, kb);
    kos_state_set_trace(s->sigma, trace);

    if (ctx_kos_path && ctx_kos_path[0]) {
        s->ctx_kos_path = strdup(ctx_kos_path);
    }

    return s;
}

void kos_kernel_session_free(kos_kernel_session_t* session) {
    if (!session) return;
    if (session->gamma) kos_gamma_free(session->gamma);
    if (session->sigma) {
        /* σ 释放时会一并释放 KB 和 trace */
        kos_state_free(session->sigma);
    }
    if (session->ctx_kos_path) free(session->ctx_kos_path);
    free(session);
}

int kos_kernel_session_load_ontology(kos_kernel_session_t* session, const char* kos_path) {
    if (!session || !kos_path) return -1;
    char err[512];
    if (!kos_core_bridge_check_file(kos_path, err, sizeof(err))) {
        return -1;  /* 校验失败 */
    }
    if (session->ctx_kos_path) free(session->ctx_kos_path);
    session->ctx_kos_path = strdup(kos_path);
    return session->ctx_kos_path ? 0 : -1;
}

kos_kernel_context_t* kos_kernel_session_get_gamma(const kos_kernel_session_t* session) {
    return session ? session->gamma : NULL;
}

kos_state_t* kos_kernel_session_get_sigma(const kos_kernel_session_t* session) {
    return session ? session->sigma : NULL;
}

kos_trace_t* kos_kernel_session_get_trace(const kos_kernel_session_t* session) {
    return session && session->sigma ? (kos_trace_t*)kos_state_get_trace(session->sigma) : NULL;
}

kos_knowledge_base_t* kos_kernel_session_get_kb(const kos_kernel_session_t* session) {
    return session && session->sigma ? (kos_knowledge_base_t*)kos_state_get_kb(session->sigma) : NULL;
}

int kos_kernel_session_enqueue_event(kos_kernel_session_t* session, kos_term* event_pair) {
    if (!session || !session->sigma || !session->sigma->P) return -1;
    return kos_queue_enqueue(session->sigma->P, event_pair);
}

bool kos_kernel_session_step(kos_kernel_session_t* session) {
    if (!session || !session->sigma) return false;
    return kos_step(session->sigma);
}

bool kos_kernel_session_find_root_cause(kos_kernel_session_t* session,
                                         const char* ctx_kos_path,
                                         const char* goal_type,
                                         kos_term** proof_out,
                                         char* errmsg, size_t errmsg_size) {
    const char* path = ctx_kos_path ? ctx_kos_path : (session && session->ctx_kos_path ? session->ctx_kos_path : NULL);
    if (!path || !goal_type) {
        if (errmsg && errmsg_size > 0) {
            snprintf(errmsg, errmsg_size, "ctx_kos_path and goal_type required");
        }
        return false;
    }

    if (proof_out) {
        *proof_out = (kos_term*)kos_core_bridge_prove_json(path, goal_type, errmsg, errmsg_size);
        return *proof_out != NULL;
    } else {
        return kos_core_bridge_prove(path, goal_type, errmsg, errmsg_size);
    }
}

bool kos_kernel_session_can_prove(kos_kernel_session_t* session,
                                   const char* ctx_kos_path,
                                   const char* goal_type) {
    char err[512];
    return kos_kernel_session_find_root_cause(session, ctx_kos_path, goal_type, NULL, err, sizeof(err));
}
