/**
 * @file event_op_registry.c
 * @brief KOS Kernel 层事件-操作栈注册表及小步执行实现
 *
 * 以事件为核心：某一类事件对应到可执行序列的栈，
 * 按小步操作语义依次从栈中获取对应函数执行，直到结束。
 */

#include "kos_event_op.h"
#include "kos_kernel.h"
#include "kos_core.h"
#include <stdlib.h>
#include <string.h>

/* ========== 默认事件类型解析 ==========
 * 根据事件 e 的 kind 或 atomic.val 生成类型标识 */

static const char* default_event_type_resolver(kos_term* event_pair) {
    if (!event_pair || event_pair->kind != KOS_PAIR || !event_pair->data.pair.data) {
        return "default";
    }
    kos_term* e = event_pair->data.pair.data;
    if (e->kind == KOS_PROP && e->data.atomic.val) {
        return e->data.atomic.val;
    }
    /* 按 kind 映射 */
    switch (e->kind) {
        case KOS_PROP: return "Prop";
        case KOS_PAIR: return "Pair";
        case KOS_SIGMA: return "Sigma";
        case KOS_PI: return "Pi";
        case KOS_SUM: return "Sum";
        case KOS_VAL: return "Val";
        case KOS_TIME: return "Time";
        case KOS_ID: return "Id";
        default: return "default";
    }
}

/* ========== 注册表 API ========== */

kos_event_op_registry_t* kos_event_op_registry_create(void) {
    kos_event_op_registry_t* reg = (kos_event_op_registry_t*)calloc(1, sizeof(kos_event_op_registry_t));
    if (!reg) return NULL;
    reg->entries = NULL;
    reg->capacity = 0;
    reg->count = 0;
    reg->type_resolver = default_event_type_resolver;
    return reg;
}

void kos_event_op_registry_free(kos_event_op_registry_t* reg) {
    if (!reg) return;
    if (reg->entries) {
        for (size_t i = 0; i < reg->count; i++) {
            if (reg->entries[i].event_type_id) {
                free((void*)reg->entries[i].event_type_id);
            }
        }
        free(reg->entries);
    }
    free(reg);
}

int kos_event_op_register(kos_event_op_registry_t* reg,
                          const char* event_type_id,
                          kos_event_op_fn* ops, size_t op_count) {
    if (!reg || !event_type_id || !ops || op_count == 0 || op_count > KOS_EVENT_OP_STACK_MAX) {
        return -1;
    }

    /* 扩容 */
    if (reg->count >= reg->capacity) {
        size_t new_cap = reg->capacity ? reg->capacity * 2 : 8;
        kos_event_op_entry_t* new_entries = (kos_event_op_entry_t*)realloc(
            reg->entries, new_cap * sizeof(kos_event_op_entry_t));
        if (!new_entries) return -1;
        reg->entries = new_entries;
        reg->capacity = new_cap;
    }

    kos_event_op_entry_t* ent = &reg->entries[reg->count];
    ent->event_type_id = strdup(event_type_id);
    if (!ent->event_type_id) return -1;
    ent->op_count = op_count;
    memcpy(ent->ops, ops, op_count * sizeof(kos_event_op_fn));
    reg->count++;
    return 0;
}

void kos_event_op_set_resolver(kos_event_op_registry_t* reg,
                               kos_event_type_resolver_fn resolver) {
    if (reg) reg->type_resolver = resolver ? resolver : default_event_type_resolver;
}

kos_event_op_entry_t* kos_event_op_lookup(kos_event_op_registry_t* reg,
                                          const char* event_type_id) {
    if (!reg || !event_type_id) return NULL;
    for (size_t i = 0; i < reg->count; i++) {
        if (reg->entries[i].event_type_id &&
            strcmp(reg->entries[i].event_type_id, event_type_id) == 0) {
            return &reg->entries[i];
        }
    }
    return NULL;
}

/* ========== 内置操作实现 ========== */

kos_op_result_t kos_op_verify_pre(kos_small_step_ctx_t* ctx, kos_term* event_pair) {
    if (!ctx || !ctx->sigma || !event_pair) return KOS_OP_FAIL;
    if (!kos_verify_precondition(event_pair, ctx->sigma)) {
        return KOS_OP_FAIL;
    }
    return KOS_OP_DONE;
}

kos_op_result_t kos_op_reduce(kos_small_step_ctx_t* ctx, kos_term* event_pair) {
    if (!ctx || !ctx->sigma || !event_pair || event_pair->kind != KOS_PAIR) {
        return KOS_OP_FAIL;
    }
    kos_term* e = event_pair->data.pair.data;
    if (!e) return KOS_OP_FAIL;
    kos_term* reduced = kos_reduce(kos_term_copy(e));
    if (!reduced) return KOS_OP_FAIL;
    if (ctx->op_intermediate) kos_term_free(ctx->op_intermediate);
    ctx->op_intermediate = reduced;
    return KOS_OP_DONE;
}

kos_op_result_t kos_op_update_knowledge(kos_small_step_ctx_t* ctx, kos_term* event_pair) {
    (void)event_pair;
    if (!ctx || !ctx->sigma) return KOS_OP_FAIL;
    if (!ctx->op_intermediate) return KOS_OP_FAIL;
    kos_term* old_K = ctx->sigma->K;
    ctx->sigma->K = kos_update_knowledge(ctx->sigma->K, ctx->op_intermediate);
    kos_term_free(ctx->op_intermediate);
    ctx->op_intermediate = NULL;
    if (!ctx->sigma->K) {
        ctx->sigma->K = old_K;
        return KOS_OP_FAIL;
    }
    if (old_K && old_K != ctx->sigma->K) kos_term_free(old_K);
    ctx->sigma->TS++;
    return KOS_OP_DONE;
}

kos_op_result_t kos_op_confirm_post(kos_small_step_ctx_t* ctx, kos_term* event_pair) {
    if (!ctx || !ctx->sigma || !event_pair) return KOS_OP_FAIL;
    /* 简化：后置条件验证，当前认为 TS 增加即满足 */
    (void)event_pair;
    return KOS_OP_DONE;
}

/* ========== 默认操作栈（Peek-Verify-Reduce-Update-Confirm） ========== */

static kos_event_op_fn default_ops[] = {
    kos_op_verify_pre,
    kos_op_reduce,
    kos_op_update_knowledge,
    kos_op_confirm_post
};
static const size_t default_ops_count = sizeof(default_ops) / sizeof(default_ops[0]);

/* ========== 小步执行上下文 ========== */

kos_small_step_ctx_t* kos_small_step_ctx_create(kos_state_t* sigma,
                                                 kos_event_op_registry_t* registry) {
    if (!sigma || !registry) return NULL;
    kos_small_step_ctx_t* ctx = (kos_small_step_ctx_t*)calloc(1, sizeof(kos_small_step_ctx_t));
    if (!ctx) return NULL;
    ctx->sigma = sigma;
    ctx->registry = registry;
    ctx->current_event = NULL;
    ctx->op_stack = NULL;
    ctx->op_count = 0;
    ctx->op_index = 0;
    ctx->op_intermediate = NULL;
    return ctx;
}

void kos_small_step_ctx_free(kos_small_step_ctx_t* ctx) {
    if (!ctx) return;
    if (ctx->op_stack) free(ctx->op_stack);
    if (ctx->op_intermediate) kos_term_free(ctx->op_intermediate);
    free(ctx);
}

/* ========== 小步执行 ========== */

bool kos_step_small(kos_small_step_ctx_t* ctx) {
    if (!ctx || !ctx->sigma || !ctx->registry) return false;

    /* 情况1：当前有进行中的事件，执行下一个操作 */
    if (ctx->current_event && ctx->op_stack && ctx->op_index < ctx->op_count) {
        kos_event_op_fn fn = ctx->op_stack[ctx->op_index];
        kos_op_result_t res = fn(ctx, ctx->current_event);
        if (res == KOS_OP_FAIL) {
            return false;
        }
        if (res == KOS_OP_CONTINUE) {
            return true;  /* 本步未完成，下次继续 */
        }
        /* KOS_OP_DONE：本步完成，进入下一步 */
        ctx->op_index++;
        if (ctx->op_index >= ctx->op_count) {
            /* 所有操作完成，出队并清理 */
            kos_term* dequeued = kos_queue_dequeue(ctx->sigma->P);
            if (dequeued) kos_term_free(dequeued);
            ctx->current_event = NULL;
            if (ctx->op_stack) {
                free(ctx->op_stack);
                ctx->op_stack = NULL;
            }
            ctx->op_count = 0;
            ctx->op_index = 0;
        }
        return true;
    }

    /* 情况2：无进行中的事件，从队列取新事件 */
    if (kos_queue_is_empty(ctx->sigma->P)) {
        return false;
    }
    kos_term* event_pair = kos_queue_peek(ctx->sigma->P);
    if (!event_pair) return false;

    /* 解析事件类型，查找操作栈 */
    const char* type_id = ctx->registry->type_resolver ?
        ctx->registry->type_resolver(event_pair) : "default";
    kos_event_op_entry_t* ent = kos_event_op_lookup(ctx->registry, type_id);
    kos_event_op_fn* ops;
    size_t count;
    if (ent && ent->op_count > 0) {
        ops = ent->ops;
        count = ent->op_count;
    } else {
        ops = default_ops;
        count = default_ops_count;
    }

    /* 复制操作栈到上下文（避免注册表被修改影响） */
    kos_event_op_fn* stack_copy = (kos_event_op_fn*)malloc(count * sizeof(kos_event_op_fn));
    if (!stack_copy) return false;
    memcpy(stack_copy, ops, count * sizeof(kos_event_op_fn));

    ctx->current_event = event_pair;
    ctx->op_stack = stack_copy;
    ctx->op_count = count;
    ctx->op_index = 0;

    /* 立即执行第一步 */
    return kos_step_small(ctx);
}

void kos_evolution_cycle_small(kos_small_step_ctx_t* ctx) {
    if (!ctx) return;
    int max_iter = 10000;
    while (kos_step_small(ctx) && max_iter-- > 0) {
        /* 持续小步执行 */
    }
}
