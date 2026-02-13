// src/domain/manufacturing/causal_index.c
// 因果事件索引：存储、解析、搜索因果链

#include "../../../include/kos_causal_trace.h"
#include "../../../include/kos_manufacturing.h"
#include "../../../include/kos_knowledge_base.h"
#include "../../../include/kos_core.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

#define INIT_CAP 32

typedef struct {
    FailEvt* items;
    size_t count;
    size_t capacity;
} fail_evt_array_t;

typedef struct {
    ProcStep* items;
    size_t count;
    size_t capacity;
} proc_step_array_t;

typedef struct {
    Anomaly* items;
    size_t count;
    size_t capacity;
} anomaly_array_t;

struct kos_causal_index {
    fail_evt_array_t fail_evts;
    proc_step_array_t proc_steps;
    anomaly_array_t anomalies;
};

static void* grow_array(void* arr, size_t* cap, size_t elem_size) {
    size_t new_cap = *cap ? *cap * 2 : INIT_CAP;
    void* p = realloc(arr, new_cap * elem_size);
    if (p) *cap = new_cap;
    return p;
}

static const char* get_prop_string(const kos_term* t) {
    if (!t) return NULL;
    if (t->kind == KOS_PROP && t->data.atomic.val) return t->data.atomic.val;
    if (t->kind == KOS_PAIR && t->data.pair.data)
        return get_prop_string(t->data.pair.data);
    return NULL;
}

/* 解析 FailEvt(batch=%s, error=%s, time=%llu) */
static int parse_fail_evt(const char* s, FailEvt* out) {
    if (!s || !out || strncmp(s, "FailEvt(", 8) != 0) return -1;
    char batch[64] = {0}, err[32] = {0};
    unsigned long long t = 0;
    if (sscanf(s, "FailEvt(batch=%63[^,], error=%31[^,], time=%llu)", batch, err, &t) < 3)
        return -1;
    strncpy(out->batch.batch_id, batch, sizeof(out->batch.batch_id) - 1);
    strncpy(out->error.code, err, sizeof(out->error.code) - 1);
    out->time = (Time)t;
    out->qa_shift_proof = NULL;
    return 0;
}

/* 解析 ProcStep(batch=%s, machine=%s, start=%llu, end=%llu) */
static int parse_proc_step(const char* s, ProcStep* out) {
    if (!s || !out || strncmp(s, "ProcStep(", 9) != 0) return -1;
    char batch[64] = {0}, machine[64] = {0};
    unsigned long long start = 0, end = 0;
    if (sscanf(s, "ProcStep(batch=%63[^,], machine=%63[^,], start=%llu, end=%llu)",
               batch, machine, &start, &end) < 4)
        return -1;
    strncpy(out->batch.batch_id, batch, sizeof(out->batch.batch_id) - 1);
    strncpy(out->machine.machine_id, machine, sizeof(out->machine.machine_id) - 1);
    out->machine.line_id[0] = '\0';
    out->duration.start = (Time)start;
    out->duration.end = (Time)end;
    out->route_proof = NULL;
    return 0;
}

/* 解析 Anomaly(machine=%s, param=%s, value=%lf, time=%llu) */
static int parse_anomaly(const char* s, Anomaly* out) {
    if (!s || !out || strncmp(s, "Anomaly(", 8) != 0) return -1;
    char machine[64] = {0}, param[64] = {0};
    double value = 0;
    unsigned long long t = 0;
    if (sscanf(s, "Anomaly(machine=%63[^,], param=%63[^,], value=%lf, time=%llu)",
               machine, param, &value, &t) < 4)
        return -1;
    strncpy(out->machine.machine_id, machine, sizeof(out->machine.machine_id) - 1);
    out->machine.line_id[0] = '\0';
    strncpy(out->param.param_name, param, sizeof(out->param.param_name) - 1);
    out->value.param = out->param;
    out->value.value = value;
    out->time = (Time)t;
    return 0;
}

/* 判断两个 Anomaly 是否相同（用于反事实推理排除） */
static bool anomaly_equals(const Anomaly* a, const Anomaly* b) {
    if (!a || !b) return false;
    return strcmp(a->machine.machine_id, b->machine.machine_id) == 0 &&
           strcmp(a->param.param_name, b->param.param_name) == 0 &&
           a->value.value == b->value.value &&
           a->time == b->time;
}

static void collect_from_term(const kos_term* t, kos_causal_index_t* idx) {
    if (!t || t->kind != KOS_PAIR) return;
    const kos_term* fact = t->data.pair.data;
    const char* s = get_prop_string(fact);
    if (!s) return;
    if (strncmp(s, "FailEvt(", 8) == 0) {
        FailEvt evt;
        if (parse_fail_evt(s, &evt) == 0)
            kos_causal_index_add_fail_evt(idx, evt);
    } else if (strncmp(s, "ProcStep(", 9) == 0) {
        ProcStep step;
        if (parse_proc_step(s, &step) == 0)
            kos_causal_index_add_proc_step(idx, step);
    } else if (strncmp(s, "Anomaly(", 8) == 0) {
        Anomaly a;
        if (parse_anomaly(s, &a) == 0)
            kos_causal_index_add_anomaly(idx, a);
    }
    collect_from_term(t->data.pair.proof, idx);
}

/* 从 K 构建索引，排除指定异常（用于反事实推理） */
static void collect_from_term_excluding_anomaly(const kos_term* t, kos_causal_index_t* idx,
                                                 const Anomaly* exclude) {
    if (!t || t->kind != KOS_PAIR) return;
    const kos_term* fact = t->data.pair.data;
    const char* s = get_prop_string(fact);
    if (!s) return;
    if (strncmp(s, "FailEvt(", 8) == 0) {
        FailEvt evt;
        if (parse_fail_evt(s, &evt) == 0)
            kos_causal_index_add_fail_evt(idx, evt);
    } else if (strncmp(s, "ProcStep(", 9) == 0) {
        ProcStep step;
        if (parse_proc_step(s, &step) == 0)
            kos_causal_index_add_proc_step(idx, step);
    } else if (strncmp(s, "Anomaly(", 8) == 0) {
        Anomaly a;
        if (parse_anomaly(s, &a) == 0) {
            if (!exclude || !anomaly_equals(&a, exclude))
                kos_causal_index_add_anomaly(idx, a);
        }
    }
    collect_from_term_excluding_anomaly(t->data.pair.proof, idx, exclude);
}

kos_causal_index_t* kos_causal_index_create(void) {
    kos_causal_index_t* idx = (kos_causal_index_t*)calloc(1, sizeof(kos_causal_index_t));
    return idx;
}

void kos_causal_index_free(kos_causal_index_t* idx) {
    if (!idx) return;
    free(idx->fail_evts.items);
    free(idx->proc_steps.items);
    free(idx->anomalies.items);
    free(idx);
}

int kos_causal_index_build_from_K(kos_causal_index_t* idx, const kos_term* K) {
    if (!idx || !K) return -1;
    collect_from_term(K, idx);
    return 0;
}

/* 从 K 构建索引，排除指定异常（用于反事实推理） */
int kos_causal_index_build_from_K_excluding_anomaly(kos_causal_index_t* idx,
                                                      const kos_term* K,
                                                      const Anomaly* exclude) {
    if (!idx || !K) return -1;
    collect_from_term_excluding_anomaly(K, idx, exclude);
    return 0;
}

/* 从 KB 项中解析 ProcStep/Anomaly/FailEvt 并加入索引 */
static void add_from_prop_string(const char* s, kos_causal_index_t* idx) {
    if (!s || !idx) return;
    if (strncmp(s, "FailEvt(", 8) == 0) {
        FailEvt evt;
        if (parse_fail_evt(s, &evt) == 0)
            kos_causal_index_add_fail_evt(idx, evt);
    } else if (strncmp(s, "ProcStep(", 9) == 0) {
        ProcStep step;
        if (parse_proc_step(s, &step) == 0)
            kos_causal_index_add_proc_step(idx, step);
    } else if (strncmp(s, "Anomaly(", 8) == 0) {
        Anomaly a;
        if (parse_anomaly(s, &a) == 0)
            kos_causal_index_add_anomaly(idx, a);
    }
}

int kos_causal_index_build_from_kb(kos_causal_index_t* idx, const void* kb) {
    if (!idx || !kb) return -1;
    const kos_knowledge_base_t* k = (const kos_knowledge_base_t*)kb;
    for (kos_kb_item_t* p = k->items; p; p = p->next) {
        if (!p->term) continue;
        const char* s = get_prop_string(p->term);
        add_from_prop_string(s, idx);
    }
    return 0;
}

/* 从 KB 构建索引，排除指定异常（用于反事实推理：假设该异常未发生） */
int kos_causal_index_build_from_kb_excluding_anomaly(kos_causal_index_t* idx,
                                                      const void* kb,
                                                      const Anomaly* exclude) {
    if (!idx || !kb) return -1;
    const kos_knowledge_base_t* k = (const kos_knowledge_base_t*)kb;
    for (kos_kb_item_t* p = k->items; p; p = p->next) {
        if (!p->term) continue;
        const char* s = get_prop_string(p->term);
        if (!s) continue;
        if (strncmp(s, "Anomaly(", 8) == 0 && exclude) {
            Anomaly a;
            if (parse_anomaly(s, &a) == 0 && anomaly_equals(&a, exclude))
                continue;  /* 反事实世界：排除该异常 */
        }
        add_from_prop_string(s, idx);
    }
    return 0;
}

int kos_causal_index_add_fail_evt(kos_causal_index_t* idx, FailEvt evt) {
    if (!idx) return -1;
    if (idx->fail_evts.count >= idx->fail_evts.capacity) {
        FailEvt* p = (FailEvt*)grow_array(idx->fail_evts.items, &idx->fail_evts.capacity, sizeof(FailEvt));
        if (!p) return -1;
        idx->fail_evts.items = p;
    }
    idx->fail_evts.items[idx->fail_evts.count++] = evt;
    return 0;
}

int kos_causal_index_add_proc_step(kos_causal_index_t* idx, ProcStep step) {
    if (!idx) return -1;
    if (idx->proc_steps.count >= idx->proc_steps.capacity) {
        ProcStep* p = (ProcStep*)grow_array(idx->proc_steps.items, &idx->proc_steps.capacity, sizeof(ProcStep));
        if (!p) return -1;
        idx->proc_steps.items = p;
    }
    idx->proc_steps.items[idx->proc_steps.count++] = step;
    return 0;
}

int kos_causal_index_add_anomaly(kos_causal_index_t* idx, Anomaly anomaly) {
    if (!idx) return -1;
    if (idx->anomalies.count >= idx->anomalies.capacity) {
        Anomaly* p = (Anomaly*)grow_array(idx->anomalies.items, &idx->anomalies.capacity, sizeof(Anomaly));
        if (!p) return -1;
        idx->anomalies.items = p;
    }
    idx->anomalies.items[idx->anomalies.count++] = anomaly;
    return 0;
}

static bool causal_validity_simple(const Anomaly* a, const FailEvt* f, const ProcStep* s) {
    if (!a || !f || !s) return false;
    bool temporal = (a->time >= s->duration.start) && (a->time <= s->duration.end) &&
                    (s->duration.end < f->time);
    bool spatial = (strcmp(a->machine.machine_id, s->machine.machine_id) == 0);
    bool batch = (strcmp(s->batch.batch_id, f->batch.batch_id) == 0);
    return temporal && spatial && batch;
}

kos_causal_chain_t* kos_causal_search_evidence(kos_causal_index_t* idx,
                                                FailEvt failure,
                                                size_t* out_count) {
    if (!idx || !out_count) return NULL;
    *out_count = 0;
    kos_causal_chain_t* chains = NULL;
    size_t cap = 0, n = 0;
    for (size_t i = 0; i < idx->proc_steps.count; i++) {
        ProcStep* step = &idx->proc_steps.items[i];
        if (strcmp(step->batch.batch_id, failure.batch.batch_id) != 0) continue;
        if (step->duration.end >= failure.time) continue;
        for (size_t j = 0; j < idx->anomalies.count; j++) {
            Anomaly* a = &idx->anomalies.items[j];
            if (!causal_validity_simple(a, &failure, step)) continue;
            if (n >= cap) {
                size_t new_cap = cap ? cap * 2 : 8;
                kos_causal_chain_t* t = (kos_causal_chain_t*)realloc(chains, new_cap * sizeof(kos_causal_chain_t));
                if (!t) goto done;
                chains = t;
                cap = new_cap;
            }
            chains[n].anomaly = *a;
            chains[n].process_step = *step;
            chains[n].failure = failure;
            chains[n].valid = true;
            n++;
        }
    }
done:
    *out_count = n;
    return chains;
}

void kos_causal_chains_free(kos_causal_chain_t* chains, size_t count) {
    (void)count;
    free(chains);
}
