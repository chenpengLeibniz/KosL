/**
 * @file kernel_context.c
 * @brief Γ（Gamma）实现：类型与谓词集合，供证明构造与小步演化使用
 */

#include "kos_kernel_context.h"
#include <stdlib.h>
#include <string.h>

kos_kernel_context_t* kos_gamma_create(void) {
    kos_kernel_context_t* gamma = (kos_kernel_context_t*)calloc(1, sizeof(kos_kernel_context_t));
    if (!gamma) return NULL;
    gamma->bindings = NULL;
    gamma->count = 0;
    return gamma;
}

static void free_binding(kos_gamma_binding_t* b) {
    if (!b) return;
    if (b->name) free(b->name);
    if (b->type) kos_term_free(b->type);
    if (b->body) kos_term_free(b->body);
    free_binding(b->next);
    free(b);
}

void kos_gamma_free(kos_kernel_context_t* gamma) {
    if (!gamma) return;
    free_binding(gamma->bindings);
    free(gamma);
}

int kos_gamma_add(kos_kernel_context_t* gamma, const char* name, kos_term* type, kos_term* body) {
    if (!gamma || !name || !type) return -1;
    kos_gamma_binding_t* b = (kos_gamma_binding_t*)calloc(1, sizeof(kos_gamma_binding_t));
    if (!b) return -1;
    b->name = strdup(name);
    if (!b->name) { free(b); return -1; }
    b->type = kos_term_copy(type);
    if (!b->type) { free(b->name); free(b); return -1; }
    b->body = body ? kos_term_copy(body) : NULL;
    b->next = gamma->bindings;
    gamma->bindings = b;
    gamma->count++;
    return 0;
}

int kos_gamma_add_type(kos_kernel_context_t* gamma, const char* name, kos_term* type) {
    return kos_gamma_add(gamma, name, type, NULL);
}

const kos_term* kos_gamma_lookup_type(const kos_kernel_context_t* gamma, const char* name) {
    if (!gamma || !name) return NULL;
    for (kos_gamma_binding_t* b = gamma->bindings; b; b = b->next) {
        if (strcmp(b->name, name) == 0) return b->type;
    }
    return NULL;
}

const kos_term* kos_gamma_lookup_body(const kos_kernel_context_t* gamma, const char* name) {
    if (!gamma || !name) return NULL;
    for (kos_gamma_binding_t* b = gamma->bindings; b; b = b->next) {
        if (strcmp(b->name, name) == 0) return b->body;
    }
    return NULL;
}

size_t kos_gamma_count(const kos_kernel_context_t* gamma) {
    return gamma ? gamma->count : 0;
}
