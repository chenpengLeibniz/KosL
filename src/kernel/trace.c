/**
 * @file trace.c
 * @brief 轨迹 T = σ₀ →^⟨e₁,p₁⟩ σ₁ → … 的实现；MTK Event Log + 确定性重放
 */

#include "kos_trace.h"
#include "kos_kernel.h"
#include "kos_core.h"
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <stdio.h>

#define TRACE_INIT_CAP 16

/* FNV-1a 64-bit，与 state_step.c 中 state_hash 折叠一致 */
static uint64_t hash_fnv1a_combine(uint64_t prev, const void* data, size_t len) {
    const uint64_t FNV_OFFSET = 14695981039346656037ULL;
    const uint64_t FNV_PRIME  = 1099511628211ULL;
    uint64_t h = prev ? prev : FNV_OFFSET;
    const unsigned char* p = (const unsigned char*)data;
    for (size_t i = 0; i < len; i++) {
        h ^= (uint64_t)p[i];
        h *= FNV_PRIME;
    }
    return h;
}

kos_trace_t* kos_trace_create(bool store_K) {
    kos_trace_t* t = (kos_trace_t*)calloc(1, sizeof(kos_trace_t));
    if (!t) return NULL;
    t->steps = NULL;
    t->count = 0;
    t->capacity = 0;
    t->store_K_snapshot = store_K;
    return t;
}

static void free_step(kos_trace_step_t* s) {
    if (!s) return;
    if (s->event_pair) kos_term_free(s->event_pair);
    if (s->K_snapshot) kos_term_free(s->K_snapshot);
}

void kos_trace_free(kos_trace_t* trace) {
    if (!trace) return;
    for (size_t i = 0; i < trace->count; i++) {
        free_step(&trace->steps[i]);
    }
    free(trace->steps);
    free(trace);
}

int kos_trace_append(kos_trace_t* trace, const struct kos_state* sigma_after, kos_term* event_pair) {
    if (!trace || !sigma_after || !event_pair || event_pair->kind != KOS_PAIR) return -1;

    if (trace->count >= trace->capacity) {
        size_t new_cap = trace->capacity ? trace->capacity * 2 : TRACE_INIT_CAP;
        kos_trace_step_t* new_steps = (kos_trace_step_t*)realloc(
            trace->steps, new_cap * sizeof(kos_trace_step_t));
        if (!new_steps) return -1;
        trace->steps = new_steps;
        trace->capacity = new_cap;
    }

    kos_trace_step_t* step = &trace->steps[trace->count];
    step->step_index = trace->count;
    step->ts_after = kos_state_get_TS(sigma_after);
    step->event_pair = kos_term_copy(event_pair);
    if (!step->event_pair) return -1;
    step->K_snapshot = NULL;
    if (trace->store_K_snapshot && sigma_after->K) {
        step->K_snapshot = kos_term_copy(sigma_after->K);
        /* 忽略 K 拷贝失败，仅不保存快照 */
    }
    trace->count++;
    return 0;
}

size_t kos_trace_length(const kos_trace_t* trace) {
    return trace ? trace->count : 0;
}

const kos_trace_step_t* kos_trace_get_step(const kos_trace_t* trace, size_t i) {
    if (!trace || i >= trace->count) return NULL;
    return &trace->steps[i];
}

/* MTK: 确定性重放。同一 trace + 同一 initial_K ⇒ 同一 σ */
kos_state_t* kos_trace_replay(const kos_trace_t* trace, kos_term* initial_K) {
    if (!trace) return NULL;
    kos_state_t* sigma = kos_state_create(initial_K ? kos_term_copy(initial_K) : NULL);
    if (!sigma) return NULL;
    /* 重放时不挂载 trace/KB，避免重复追加与副作用 */
    kos_state_set_trace(sigma, NULL);
    kos_state_set_kb(sigma, NULL);
    for (size_t i = 0; i < trace->count; i++) {
        const kos_trace_step_t* step = kos_trace_get_step(trace, i);
        if (!step || !step->event_pair) continue;
        if (!kos_kernel_step(sigma, step->event_pair)) {
            kos_state_free(sigma);
            return NULL;
        }
    }
    return sigma;
}

uint64_t kos_trace_fold_hash(const kos_trace_t* trace) {
    if (!trace) return 0;
    uint64_t h = 0;
    for (size_t i = 0; i < trace->count; i++) {
        const kos_trace_step_t* step = kos_trace_get_step(trace, i);
        if (!step || !step->event_pair) continue;
        kos_serialized* ser = kos_term_serialize(step->event_pair);
        if (ser && ser->data) {
            h = hash_fnv1a_combine(h, ser->data, ser->length);
            kos_serialized_free(ser);
        }
    }
    return h;
}

int kos_trace_save_to_file(const kos_trace_t* trace, const char* filename) {
    if (!trace || !filename) return -1;
    FILE* fp = fopen(filename, "wb");
    if (!fp) return -1;
    if (fwrite(&trace->count, sizeof(size_t), 1, fp) != 1) {
        fclose(fp);
        return -1;
    }
    for (size_t i = 0; i < trace->count; i++) {
        const kos_trace_step_t* step = kos_trace_get_step(trace, i);
        if (!step || !step->event_pair) {
            fclose(fp);
            return -1;
        }
        kos_serialized* ser = kos_term_serialize(step->event_pair);
        if (!ser || !ser->data) {
            fclose(fp);
            return -1;
        }
        if (fwrite(&ser->length, sizeof(size_t), 1, fp) != 1 ||
            fwrite(ser->data, 1, ser->length, fp) != ser->length) {
            kos_serialized_free(ser);
            fclose(fp);
            return -1;
        }
        kos_serialized_free(ser);
    }
    fclose(fp);
    return 0;
}

kos_trace_t* kos_trace_load_from_file(const char* filename) {
    if (!filename) return NULL;
    FILE* fp = fopen(filename, "rb");
    if (!fp) return NULL;
    size_t count = 0;
    if (fread(&count, sizeof(size_t), 1, fp) != 1) {
        fclose(fp);
        return NULL;
    }
    kos_trace_t* trace = kos_trace_create(false);
    if (!trace) {
        fclose(fp);
        return NULL;
    }
    if (count == 0) {
        fclose(fp);
        return trace;
    }
    if (count > trace->capacity) {
        size_t new_cap = count;
        kos_trace_step_t* new_steps = (kos_trace_step_t*)malloc(new_cap * sizeof(kos_trace_step_t));
        if (!new_steps) {
            kos_trace_free(trace);
            fclose(fp);
            return NULL;
        }
        trace->steps = new_steps;
        trace->capacity = new_cap;
    }
    for (size_t i = 0; i < count; i++) {
        size_t len = 0;
        if (fread(&len, sizeof(size_t), 1, fp) != 1) {
            kos_trace_free(trace);
            fclose(fp);
            return NULL;
        }
        if (len == 0) {
            trace->steps[i].step_index = i;
            trace->steps[i].ts_after = 0;
            trace->steps[i].event_pair = NULL;
            trace->steps[i].K_snapshot = NULL;
            trace->count++;
            continue;
        }
        char* buf = (char*)malloc(len + 1);
        if (!buf || fread(buf, 1, len, fp) != len) {
            free(buf);
            kos_trace_free(trace);
            fclose(fp);
            return NULL;
        }
        buf[len] = '\0';
        kos_term* event_pair = kos_term_deserialize(buf);
        free(buf);
        if (!event_pair) {
            kos_trace_free(trace);
            fclose(fp);
            return NULL;
        }
        trace->steps[i].step_index = i;
        trace->steps[i].ts_after = 0;
        trace->steps[i].event_pair = event_pair;
        trace->steps[i].K_snapshot = NULL;
        trace->count++;
    }
    fclose(fp);
    return trace;
}
