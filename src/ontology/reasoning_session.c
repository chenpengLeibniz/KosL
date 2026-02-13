// src/ontology/reasoning_session.c
// 实时推理会话：按本体维度的 kernel 状态、事件喂入与单步推进

#include "../../include/kos_reasoning.h"
#include "../../include/kos_kernel.h"
#include "../../include/kos_runtime.h"
#include <stdlib.h>
#include <string.h>

#define RS_INIT_CAP 16

typedef struct {
    char* ontology_id;
    kos_state_t* state;
} session_entry_t;

struct kos_reasoning_session {
    kos_ontology_registry_t* registry;
    session_entry_t* entries;
    size_t count;
    size_t capacity;
};

static int ensure_capacity(kos_reasoning_session_t* s) {
    if (s->count < s->capacity) return 0;
    size_t new_cap = s->capacity ? s->capacity * 2 : RS_INIT_CAP;
    session_entry_t* n = (session_entry_t*)realloc(
        s->entries, new_cap * sizeof(session_entry_t));
    if (!n) return -1;
    s->entries = n;
    s->capacity = new_cap;
    return 0;
}

static kos_state_t* find_state(kos_reasoning_session_t* s, const char* ontology_id) {
    for (size_t i = 0; i < s->count; i++) {
        if (strcmp(s->entries[i].ontology_id, ontology_id) == 0)
            return s->entries[i].state;
    }
    return NULL;
}

kos_reasoning_session_t* kos_reasoning_session_create(kos_ontology_registry_t* registry) {
    if (!registry) return NULL;
    kos_reasoning_session_t* s = (kos_reasoning_session_t*)calloc(1, sizeof(kos_reasoning_session_t));
    if (!s) return NULL;
    s->registry = registry;
    return s;
}

void kos_reasoning_session_free(kos_reasoning_session_t* session) {
    if (!session) return;
    for (size_t i = 0; i < session->count; i++) {
        free(session->entries[i].ontology_id);
        if (session->entries[i].state)
            kos_state_free(session->entries[i].state);
    }
    free(session->entries);
    free(session);
}

kos_state_t* kos_reasoning_get_or_create_state(kos_reasoning_session_t* session,
                                                const char* ontology_id,
                                                kos_term* initial_K) {
    if (!session || !ontology_id) return NULL;
    kos_state_t* sigma = find_state(session, ontology_id);
    if (sigma) return sigma;
    if (ensure_capacity(session) != 0) return NULL;
    sigma = kos_state_create(initial_K);
    if (!sigma) return NULL;
    session->entries[session->count].ontology_id = strdup(ontology_id);
    if (!session->entries[session->count].ontology_id) {
        kos_state_free(sigma);
        return NULL;
    }
    session->entries[session->count].state = sigma;
    session->count++;
    return sigma;
}

int kos_reasoning_feed_event(kos_reasoning_session_t* session,
                             const char* ontology_id,
                             bitstream raw_signal) {
    if (!session || !ontology_id) return -1;
    kos_state_t* sigma = kos_reasoning_get_or_create_state(session, ontology_id, NULL);
    if (!sigma) return -1;
    kos_term* event_pair = kos_elab(raw_signal, sigma->K);
    if (!event_pair) return -1;
    int r = kos_queue_enqueue(sigma->P, event_pair);
    kos_term_free(event_pair);
    return r;
}

bool kos_reasoning_tick(kos_reasoning_session_t* session, const char* ontology_id) {
    if (!session || !ontology_id) return false;
    kos_state_t* sigma = find_state(session, ontology_id);
    if (!sigma) return false;
    return kos_step(sigma);
}

size_t kos_reasoning_run_until_idle(kos_reasoning_session_t* session,
                                     const char* ontology_id,
                                     size_t max_steps) {
    if (!session || !ontology_id) return 0;
    kos_state_t* sigma = find_state(session, ontology_id);
    if (!sigma) return 0;
    size_t n = 0;
    while (n < max_steps && !kos_queue_is_empty(sigma->P)) {
        if (!kos_step(sigma)) break;
        n++;
    }
    return n;
}
