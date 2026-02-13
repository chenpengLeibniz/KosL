// src/runtime/signal_process.c
// 外部信号处理：能构成合法事件则自动溯源找根因，否则返回错误提示

#include "kos_runtime.h"
#include "kos_kernel.h"
#include "kos_kernel_session.h"
#include "kos_kernel_context.h"
#include "kos_core_bridge.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

static kos_traceability_handler_t s_traceability_handler = NULL;
static kos_root_cause_free_t s_root_cause_free = NULL;

void kos_runtime_register_traceability_handler(kos_traceability_handler_t handler,
                                                kos_root_cause_free_t free_func) {
    s_traceability_handler = handler;
    s_root_cause_free = free_func;
}

void kos_signal_process_result_free(kos_signal_process_result_t* result) {
    if (!result) return;
    if (result->event_pair) {
        kos_term_free(result->event_pair);
        result->event_pair = NULL;
    }
    if (result->root_cause_report && s_root_cause_free) {
        s_root_cause_free(result->root_cause_report);
        result->root_cause_report = NULL;
    }
    result->success = false;
    result->errmsg[0] = '\0';
}

static bool extract_kos_from_json(const char* json, size_t len, char* out, size_t out_size) {
    if (!json || !out || out_size == 0) return false;
    const char* key = "\"_kos\"";
    const char* p = strstr(json, key);
    if (!p) return false;
    p += strlen(key);
    while (*p && *p != ':') p++;
    if (*p != ':') return false;
    p++;
    while (*p == ' ' || *p == '\t' || *p == '\n' || *p == '\r') p++;
    if (*p != '"' && *p != '\'') return false;
    char quote = *p++;
    size_t i = 0;
    while (*p && *p != quote && i + 1 < out_size) {
        if (*p == '\\' && p[1]) {
            p++;
        }
        out[i++] = *p++;
    }
    out[i] = '\0';
    return i > 0;
}

static bool signal_to_kos_expr(bitstream signal, char* out, size_t out_size) {
    if (!signal.data || signal.length == 0 || !out || out_size == 0) return false;
    size_t len = signal.length;
    if (len >= out_size) len = out_size - 1;
    memcpy(out, signal.data, len);
    out[len] = '\0';
    if (out[0] == '{') {
        char tmp[4096];
        if (len >= sizeof(tmp)) return false;
        memcpy(tmp, out, len + 1);
        if (extract_kos_from_json(tmp, len, out, out_size)) {
            return true;
        }
        return false;
    }
    return true;
}

static void gamma_add_prop_if_missing(kos_kernel_context_t* gamma, const char* name, size_t* added) {
    if (!gamma || !name || !name[0]) return;
    if (kos_gamma_lookup_type(gamma, name)) return;
    kos_term* prop = kos_mk_prop(name);
    if (!prop) return;
    if (kos_gamma_add_type(gamma, name, prop) == 0 && added) {
        (*added)++;
    }
    kos_term_free(prop);
}

static void gamma_collect_prop_types(kos_kernel_context_t* gamma, const kos_term* term, size_t* added) {
    if (!gamma || !term) return;
    switch (term->kind) {
        case KOS_PROP:
            if (term->data.atomic.val) {
                gamma_add_prop_if_missing(gamma, term->data.atomic.val, added);
            }
            break;
        case KOS_PAIR:
            gamma_collect_prop_types(gamma, term->data.pair.data, added);
            gamma_collect_prop_types(gamma, term->data.pair.proof, added);
            break;
        case KOS_SIGMA:
            gamma_collect_prop_types(gamma, term->data.sigma.domain, added);
            gamma_collect_prop_types(gamma, term->data.sigma.body, added);
            break;
        case KOS_PI:
            gamma_collect_prop_types(gamma, term->data.pi.domain, added);
            gamma_collect_prop_types(gamma, term->data.pi.body, added);
            break;
        case KOS_SUM:
            gamma_collect_prop_types(gamma, term->data.sum.left_type, added);
            gamma_collect_prop_types(gamma, term->data.sum.right_type, added);
            break;
        case KOS_APP:
            gamma_collect_prop_types(gamma, term->data.app.func, added);
            gamma_collect_prop_types(gamma, term->data.app.arg, added);
            break;
        case KOS_LAM:
            gamma_collect_prop_types(gamma, term->data.lam.type, added);
            gamma_collect_prop_types(gamma, term->data.lam.body, added);
            break;
        case KOS_ID_TYPE:
            gamma_collect_prop_types(gamma, term->data.id_type.left, added);
            gamma_collect_prop_types(gamma, term->data.id_type.right, added);
            break;
        case KOS_REFL:
            gamma_collect_prop_types(gamma, term->data.refl.value, added);
            break;
        case KOS_LET:
            gamma_collect_prop_types(gamma, term->data.let_term.type, added);
            gamma_collect_prop_types(gamma, term->data.let_term.value, added);
            gamma_collect_prop_types(gamma, term->data.let_term.body, added);
            break;
        case KOS_SPLIT:
            gamma_collect_prop_types(gamma, term->data.split.pair, added);
            gamma_collect_prop_types(gamma, term->data.split.body, added);
            break;
        case KOS_CASE:
            gamma_collect_prop_types(gamma, term->data.case_term.sum, added);
            gamma_collect_prop_types(gamma, term->data.case_term.left_body, added);
            gamma_collect_prop_types(gamma, term->data.case_term.right_body, added);
            break;
        default:
            break;
    }
}

int kos_runtime_process_signal(bitstream signal, kos_term* ontology, kos_state_t* sigma,
                               kos_signal_process_result_t* result) {
    if (!result) return -1;
    result->success = false;
    result->event_pair = NULL;
    result->root_cause_report = NULL;
    result->errmsg[0] = '\0';

    kos_term* event_pair = NULL;
    if (!kos_elab_ex(signal, ontology, &event_pair, result->errmsg, sizeof(result->errmsg))) {
        return -1;  /* 无法构成合法事件，errmsg 已填充 */
    }
    if (!event_pair) {
        snprintf(result->errmsg, sizeof(result->errmsg), "Internal error: elab succeeded but event is NULL");
        return -1;
    }

    result->success = true;
    result->event_pair = event_pair;

    /* 若 sigma 存在且已注册溯源处理器，自动开展溯源找根因 */
    if (sigma && sigma->K && s_traceability_handler) {
        const kos_term* event = event_pair->kind == KOS_PAIR ? event_pair->data.pair.data : event_pair;
        void* report = s_traceability_handler(event, sigma);
        if (report) {
            result->root_cause_report = report;
        }
    }

    return 0;
}

void kos_runtime_refine_result_free(kos_runtime_refine_result_t* result) {
    if (!result) return;
    if (result->event_pair) {
        kos_term_free(result->event_pair);
        result->event_pair = NULL;
    }
    if (result->refined_term) {
        kos_term_free(result->refined_term);
        result->refined_term = NULL;
    }
    if (result->inferred_type) {
        kos_term_free(result->inferred_type);
        result->inferred_type = NULL;
    }
    result->success = false;
    result->is_event = false;
    result->gamma_added = 0;
    result->errmsg[0] = '\0';
}

int kos_runtime_refine_signal(bitstream signal, kos_kernel_session_t* session,
                              kos_runtime_refine_result_t* result) {
    if (!result) return -1;
    result->success = false;
    result->is_event = false;
    result->gamma_added = 0;
    result->event_pair = NULL;
    result->refined_term = NULL;
    result->inferred_type = NULL;
    result->errmsg[0] = '\0';

    if (!session) {
        snprintf(result->errmsg, sizeof(result->errmsg), "Session is NULL");
        return -1;
    }
    if (!signal.data || signal.length == 0) {
        snprintf(result->errmsg, sizeof(result->errmsg), "Signal is empty");
        return -1;
    }

    kos_kernel_context_t* gamma = kos_kernel_session_get_gamma(session);
    kos_state_t* sigma = kos_kernel_session_get_sigma(session);
    char kos_expr[4096];
    if (!signal_to_kos_expr(signal, kos_expr, sizeof(kos_expr))) {
        snprintf(result->errmsg, sizeof(result->errmsg), "Signal is not a valid .kos expression");
        return -1;
    }

    char errbuf[512];
    kos_term* term = NULL;
    kos_term* type = NULL;
    if (!kos_core_bridge_infer_from_kos(kos_expr, (void**)&term, (void**)&type, errbuf, sizeof(errbuf))) {
        snprintf(result->errmsg, sizeof(result->errmsg), "%s", errbuf);
        return -1;
    }

    if (gamma) {
        gamma_collect_prop_types(gamma, term, &result->gamma_added);
        gamma_collect_prop_types(gamma, type, &result->gamma_added);
    }

    if (term && term->kind == KOS_PAIR) {
        if (sigma && sigma->P) {
            if (kos_kernel_session_enqueue_event(session, term) != 0) {
                kos_term_free(term);
                kos_term_free(type);
                snprintf(result->errmsg, sizeof(result->errmsg), "Failed to enqueue event");
                return -1;
            }
        }
        result->is_event = true;
        result->event_pair = term;
    } else {
        result->refined_term = term;
    }

    result->inferred_type = type;
    result->success = true;
    return 0;
}

int kos_runtime_refine_from_db(const char* query, kos_kernel_session_t* session,
                               kos_runtime_refine_result_t* result) {
    if (!query) return -1;
    bitstream signal;
    signal.data = (unsigned char*)query;
    signal.length = strlen(query);
    return kos_runtime_refine_signal(signal, session, result);
}
