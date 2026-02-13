// src/domain/manufacturing/traceability.c
// 质量异常追溯分析：因果索引、证据搜索、根因报告

#include "../../../include/kos_manufacturing.h"
#include "../../../include/kos_causal_trace.h"
#include "../../../include/kos_core.h"
#include "../../../include/kos_knowledge_base.h"
#include "../../../include/kos_runtime.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

static void* traceability_handler_impl(const kos_term* event, kos_state_t* sigma) {
    if (!event || !sigma || !sigma->K) return NULL;
    FailEvt fail_evt;
    if (!kos_try_extract_fail_evt_from_event(event, &fail_evt)) return NULL;
    return kos_analyze_quality_traceability(fail_evt, sigma);
}

static void wrap_root_cause_free(void* p) {
    kos_root_cause_report_free((RootCauseReport*)p);
}

void kos_manufacturing_register_traceability_handler(void) {
    kos_runtime_register_traceability_handler(traceability_handler_impl, wrap_root_cause_free);
}

// 从 kos_term 事件尝试提取 FailEvt（支持 "FailEvt(...)" 字符串或嵌套 Σ 对结构）
bool kos_try_extract_fail_evt_from_event(const kos_term* event, FailEvt* out) {
    if (!event || !out) return false;
    memset(out, 0, sizeof(FailEvt));
    if (event->kind == KOS_PROP && event->data.atomic.val) {
        const char* s = event->data.atomic.val;
        if (strncmp(s, "FailEvt(", 8) == 0) {
            char batch[64] = {0}, err[32] = {0};
            unsigned long long t = 0;
            if (sscanf(s, "FailEvt(batch=%63[^,], error=%31[^,], time=%llu)", batch, err, &t) >= 3) {
                strncpy(out->batch.batch_id, batch, sizeof(out->batch.batch_id) - 1);
                strncpy(out->error.code, err, sizeof(out->error.code) - 1);
                out->time = (Time)t;
                return true;
            }
        }
        return false;
    }
    if (event->kind == KOS_PAIR && event->data.pair.data && event->data.pair.proof) {
        const kos_term* batch_t = event->data.pair.data;
        const kos_term* rest = event->data.pair.proof;
        const char* batch_s = (batch_t->kind == KOS_ID || batch_t->kind == KOS_PROP) && batch_t->data.atomic.val
            ? batch_t->data.atomic.val : NULL;
        if (!batch_s) return false;
        strncpy(out->batch.batch_id, batch_s, sizeof(out->batch.batch_id) - 1);
        if (rest->kind != KOS_PAIR || !rest->data.pair.data || !rest->data.pair.proof) return false;
        const kos_term* err_t = rest->data.pair.data;
        rest = rest->data.pair.proof;
        const char* err_s = (err_t->kind == KOS_PROP || err_t->kind == KOS_ID) && err_t->data.atomic.val
            ? err_t->data.atomic.val : NULL;
        if (!err_s) return false;
        strncpy(out->error.code, err_s, sizeof(out->error.code) - 1);
        if (rest->kind != KOS_PAIR || !rest->data.pair.data) return false;
        const kos_term* time_t = rest->data.pair.data;
        const char* time_s = (time_t->kind == KOS_TIME || time_t->kind == KOS_PROP) && time_t->data.atomic.val
            ? time_t->data.atomic.val : NULL;
        if (!time_s) return false;
        out->time = (Time)strtoull(time_s, NULL, 10);
        return true;
    }
    return false;
}

// 搜索因果证据：从 sigma->K 或 sigma->KB 构建因果索引
kos_term* kos_search_causal_evidence(FailEvt failure, kos_state_t* sigma) {
    if (!sigma || (!sigma->K && !sigma->KB)) return NULL;
    kos_causal_index_t* idx = kos_causal_index_create();
    if (!idx) return NULL;
    if (sigma->KB) kos_causal_index_build_from_kb(idx, sigma->KB);
    if (sigma->K) kos_causal_index_build_from_K(idx, sigma->K);
    size_t n = 0;
    kos_causal_chain_t* chains = kos_causal_search_evidence(idx, failure, &n);
    kos_causal_index_free(idx);
    if (!chains || n == 0) {
        if (chains) kos_causal_chains_free(chains, n);
        return NULL;
    }
    // 取首个有效因果链，构造 kos_term 表示（CausalProof）
    kos_term* proof = kos_mk_causal_proof(
        chains[0].anomaly,
        chains[0].failure,
        chains[0].process_step
    );
    kos_causal_chains_free(chains, n);
    return proof;
}

// 分析质量追溯：搜索因果证据并生成根因报告
// 若 sigma->KB 存在则优先从 KB 构建索引，否则从 K 构建（补齐 Phase 1.2）
RootCauseReport* kos_analyze_quality_traceability(FailEvt failure, kos_state_t* sigma) {
    if (!sigma) return NULL;
    if (!sigma->K && !sigma->KB) return NULL;
    kos_causal_index_t* idx = kos_causal_index_create();
    if (!idx) return NULL;
    if (sigma->KB) {
        kos_causal_index_build_from_kb(idx, sigma->KB);
    }
    if (sigma->K) {
        kos_causal_index_build_from_K(idx, sigma->K);
    }
    size_t n = 0;
    kos_causal_chain_t* chains = kos_causal_search_evidence(idx, failure, &n);
    kos_causal_index_free(idx);
    if (!chains || n == 0) {
        if (chains) kos_causal_chains_free(chains, n);
        return NULL;
    }
    RootCauseReport* report = (RootCauseReport*)calloc(1, sizeof(RootCauseReport));
    if (!report) {
        kos_causal_chains_free(chains, n);
        return NULL;
    }
    report->failure = chains[0].failure;
    report->anomaly = chains[0].anomaly;
    report->causal_proof.anomaly = chains[0].anomaly;
    report->causal_proof.failure = chains[0].failure;
    report->causal_proof.process_step = chains[0].process_step;
    report->causal_proof.temporal_proof = kos_mk_prop("Proof(temporal)");
    report->causal_proof.spatial_proof = kos_mk_prop("Proof(spatial)");
    report->causal_proof.batch_proof = kos_mk_prop("Proof(batch)");
    report->report_proof = kos_mk_causal_proof(
        chains[0].anomaly,
        chains[0].failure,
        chains[0].process_step
    );
    kos_causal_chains_free(chains, n);
    return report;
}

void kos_root_cause_report_free(RootCauseReport* report) {
    if (!report) return;
    if (report->causal_proof.temporal_proof) kos_term_free(report->causal_proof.temporal_proof);
    if (report->causal_proof.spatial_proof) kos_term_free(report->causal_proof.spatial_proof);
    if (report->causal_proof.batch_proof) kos_term_free(report->causal_proof.batch_proof);
    if (report->report_proof) kos_term_free(report->report_proof);
    free(report);
}

// 反事实推理测试：假设异常未发生，是否仍能解释失败？
// 若移除该异常后无法找到因果链，则说明该异常对失败是"因果必要"的（反事实支持根因结论）
// 返回 true：异常是因果必要的（反事实世界无其他解释）
// 返回 false：反事实世界仍能找到因果链（存在替代原因）或事实世界无因果链
bool kos_counterfactual_test(Anomaly anomaly, FailEvt failure, kos_state_t* sigma) {
    if (!sigma || (!sigma->K && !sigma->KB)) return false;
    /* 事实世界：含该异常，应有因果链 */
    kos_causal_index_t* idx_factual = kos_causal_index_create();
    if (!idx_factual) return false;
    if (sigma->KB) kos_causal_index_build_from_kb(idx_factual, sigma->KB);
    if (sigma->K) kos_causal_index_build_from_K(idx_factual, sigma->K);
    size_t n_factual = 0;
    kos_causal_chain_t* chains_factual = kos_causal_search_evidence(idx_factual, failure, &n_factual);
    if (!chains_factual || n_factual == 0) {
        if (chains_factual) kos_causal_chains_free(chains_factual, n_factual);
        kos_causal_index_free(idx_factual);
        return false;  /* 事实世界无因果链 */
    }
    kos_causal_chains_free(chains_factual, n_factual);
    kos_causal_index_free(idx_factual);

    /* 反事实世界：排除该异常，再搜索 */
    kos_causal_index_t* idx_cf = kos_causal_index_create();
    if (!idx_cf) return false;
    if (sigma->KB) kos_causal_index_build_from_kb_excluding_anomaly(idx_cf, sigma->KB, &anomaly);
    if (sigma->K) kos_causal_index_build_from_K_excluding_anomaly(idx_cf, sigma->K, &anomaly);
    size_t n_cf = 0;
    kos_causal_chain_t* chains_cf = kos_causal_search_evidence(idx_cf, failure, &n_cf);
    bool cf_necessary = (chains_cf == NULL || n_cf == 0);  /* 反事实无链 → 异常必要 */
    if (chains_cf) kos_causal_chains_free(chains_cf, n_cf);
    kos_causal_index_free(idx_cf);
    return cf_necessary;
}
