// examples/kernel_example.c
// Kernel 层使用示例：演示状态演化和事件处理流程
//
// 演示内容：
// 1. 创建初始状态 σ = 〈K, TS=0, P〉
// 2. 构造事件对 <e, p> 并加入队列
// 3. 执行 Peek-Verify-Reduce-Confirm 演化循环
// 4. 查询状态信息（State Mirror）
// 5. 知识库（KB）：Core 验证过的依赖项集合，支持依赖图可视化
// 6. 事件驱动小步操作：事件类型 → 操作栈，按小步依次执行

#include "../include/kos_kernel.h"
#include "../include/kos_core.h"
#include "../include/kos_knowledge_base.h"
#include "../include/kos_event_op.h"
#include <stdio.h>
#include <stdlib.h>

// 辅助函数：打印状态信息
void print_state_info(const kos_state_t* sigma, const char* label) {
    printf("\n=== %s ===\n", label);
    printf("Logical Clock (TS): %d\n", kos_state_get_TS(sigma));
    printf("Knowledge Set (K): %s\n", sigma->K ? "Present" : "Empty");
    printf("Queue Size (P): %zu\n", kos_state_get_queue_size(sigma));
    printf("State Empty: %s\n", kos_state_is_empty(sigma) ? "Yes" : "No");
}

// 辅助函数：构造一个简单的事件对
kos_term* create_event_pair(const char* event_name, const char* proof_name) {
    kos_term* event_type = kos_mk_prop(event_name);
    kos_term* event_proof = kos_mk_prop(proof_name);
    
    if (!event_type || !event_proof) {
        if (event_type) kos_term_free(event_type);
        if (event_proof) kos_term_free(event_proof);
        return NULL;
    }
    
    kos_term* event_pair = kos_mk_pair(event_type, event_proof);
    if (!event_pair) {
        kos_term_free(event_type);
        kos_term_free(event_proof);
        return NULL;
    }
    
    return event_pair;
}

int main(void) {
    printf("KOS Kernel Layer Example\n");
    printf("========================\n");
    
    // ========== 步骤 1: 创建初始状态 ==========
    printf("\n[Step 1] Creating initial state...\n");
    
    // 创建初始知识集 K
    kos_term* initial_K = kos_mk_prop("InitialKnowledge");
    if (!initial_K) {
        fprintf(stderr, "ERROR: Failed to create initial knowledge\n");
        return 1;
    }
    
    // 创建初始状态 σ = 〈K, TS=0, P=∅〉
    kos_state_t* sigma = kos_state_create(initial_K);
    if (!sigma) {
        fprintf(stderr, "ERROR: Failed to create state\n");
        kos_term_free(initial_K);
        return 1;
    }
    
    print_state_info(sigma, "Initial State");
    
    // ========== 步骤 2: 构造事件并加入队列 ==========
    printf("\n[Step 2] Creating events and enqueueing...\n");
    
    // 创建多个事件对
    kos_term* event1 = create_event_pair("FailEvt", "FailEvtProof");
    kos_term* event2 = create_event_pair("SuccessEvt", "SuccessEvtProof");
    kos_term* event3 = create_event_pair("ProcessEvt", "ProcessEvtProof");
    
    if (!event1 || !event2 || !event3) {
        fprintf(stderr, "ERROR: Failed to create events\n");
        if (event1) kos_term_free(event1);
        if (event2) kos_term_free(event2);
        if (event3) kos_term_free(event3);
        kos_state_free(sigma);
        kos_term_free(initial_K);
        return 1;
    }
    
    // 将事件加入队列（严格顺序）
    if (kos_queue_enqueue(sigma->P, event1) != 0) {
        fprintf(stderr, "ERROR: Failed to enqueue event1\n");
        kos_term_free(event1);
        kos_term_free(event2);
        kos_term_free(event3);
        kos_state_free(sigma);
        kos_term_free(initial_K);
        return 1;
    }
    
    if (kos_queue_enqueue(sigma->P, event2) != 0) {
        fprintf(stderr, "ERROR: Failed to enqueue event2\n");
        kos_term_free(event2);
        kos_term_free(event3);
        kos_state_free(sigma);
        kos_term_free(initial_K);
        return 1;
    }
    
    if (kos_queue_enqueue(sigma->P, event3) != 0) {
        fprintf(stderr, "ERROR: Failed to enqueue event3\n");
        kos_term_free(event3);
        kos_state_free(sigma);
        kos_term_free(initial_K);
        return 1;
    }
    
    printf("Enqueued 3 events\n");
    print_state_info(sigma, "State After Enqueue");
    
    // 注意：事件对的所有权已转移给队列，不应再手动释放 event1/event2/event3
    
    // ========== 步骤 3: 执行单步演化 ==========
    printf("\n[Step 3] Executing single step evolution...\n");
    
    // Peek: 查看队列头部（不取出）
    kos_term* peeked = kos_queue_peek(sigma->P);
    if (peeked) {
        printf("Peeked event: %s (queue size: %zu)\n",
               peeked->kind == KOS_PAIR && peeked->data.pair.data &&
               peeked->data.pair.data->kind == KOS_PROP &&
               peeked->data.pair.data->data.atomic.val ?
               peeked->data.pair.data->data.atomic.val : "Unknown",
               kos_queue_size(sigma->P));
    }
    
    // 执行单步演化
    bool step_result = kos_step(sigma);
    printf("Step result: %s\n", step_result ? "SUCCESS" : "FAIL");
    print_state_info(sigma, "State After Step 1");
    
    // ========== 步骤 4: 执行批量演化循环 ==========
    printf("\n[Step 4] Executing evolution cycle...\n");
    
    // 执行完整的 Peek-Verify-Reduce-Confirm 循环
    bool cycle_result = kos_evolution_cycle(sigma);
    printf("Evolution cycle result: %s\n", cycle_result ? "SUCCESS" : "FAIL");
    print_state_info(sigma, "State After Evolution Cycle");
    
    // ========== 步骤 5: 直接执行事件演化 ==========
    printf("\n[Step 5] Direct event execution...\n");
    
    // 创建一个新事件并直接执行（不通过队列）
    kos_term* direct_event = create_event_pair("DirectEvt", "DirectEvtProof");
    if (direct_event) {
        bool direct_result = kos_kernel_step(sigma, direct_event);
        printf("Direct step result: %s\n", direct_result ? "SUCCESS" : "FAIL");
        print_state_info(sigma, "State After Direct Step");
        kos_term_free(direct_event);
    }
    
    // ========== 步骤 6: 查询 State Mirror ==========
    printf("\n[Step 6] Querying State Mirror...\n");
    
    const kos_term* K_view = kos_state_get_K(sigma);
    int TS_value = kos_state_get_TS(sigma);
    size_t queue_size = kos_state_get_queue_size(sigma);
    bool is_empty = kos_state_is_empty(sigma);
    
    printf("State Mirror Query Results:\n");
    printf("  K (Knowledge Set): %s\n", K_view ? "Present" : "Empty");
    printf("  TS (Logical Clock): %d\n", TS_value);
    printf("  Queue Size: %zu\n", queue_size);
    printf("  Is Empty: %s\n", is_empty ? "Yes" : "No");
    
    // ========== 步骤 7: 知识库与事件驱动小步 ==========
    printf("\n[Step 7] Knowledge Base and Event-Driven Small-Step...\n");

    /* 创建知识库并关联到状态 */
    kos_knowledge_base_t* kb = kos_kb_create();
    if (kb) {
        kos_state_set_kb(sigma, kb);
        /* 添加启动时抽取的初始项（模拟从 DB 抽取） */
        kos_term* init_t = kos_mk_prop("BootstrapFact");
        kos_term* init_A = kos_mk_prop("BootstrapType");
        if (init_t && init_A) {
            kos_kb_add_item(kb, "boot_1", init_t, init_A, 0, KOS_KB_SOURCE_BOOTSTRAP);
            kos_term_free(init_t);
            kos_term_free(init_A);
        }
        /* 添加引用 boot_1 的项（用于依赖推断） */
        kos_term* ref_t = kos_mk_prop("boot_1");  /* 项中引用 boot_1 */
        kos_term* ref_A = kos_mk_prop("DerivedType");
        if (ref_t && ref_A) {
            kos_kb_add_item(kb, "boot_2", ref_t, ref_A, 1, KOS_KB_SOURCE_BOOTSTRAP);
            kos_term_free(ref_t);
            kos_term_free(ref_A);
        }
        /* 显式添加因果依赖（事件→根因） */
        kos_kb_add_dependency_typed(kb, "boot_2", "boot_1", KOS_KB_DEP_TYPE_CAUSAL);
        /* 从项结构推断依赖（boot_2 引用 boot_1） */
        int inferred = kos_kb_infer_dependencies(kb);
        printf("  Knowledge Base: %zu items, %zu edges (inferred %d)\n",
               kb->item_count, kb->edge_count, inferred);
        /* 导出依赖图 JSON（用于可视化） */
        char* json = kos_kb_export_dependency_graph_json(kb);
        if (json) {
            printf("  Dependency Graph JSON: %.120s...\n", json);
            free(json);
        }
        /* 导出 HTML 可视化 */
        char* html = kos_kb_export_visualization_html(kb);
        if (html) {
            FILE* f = fopen("kb_visualization.html", "w");
            if (f) {
                fputs(html, f);
                fclose(f);
                printf("  HTML visualization written to kb_visualization.html\n");
            }
            free(html);
        }
    }

    /* 事件驱动小步：创建注册表和小步上下文 */
    kos_event_op_registry_t* reg = kos_event_op_registry_create();
    kos_small_step_ctx_t* step_ctx = kos_small_step_ctx_create(sigma, reg);
    if (reg && step_ctx) {
        /* 入队一个事件 */
        kos_term* evt = create_event_pair("SmallStepEvt", "SmallStepProof");
        if (evt) {
            kos_queue_enqueue(sigma->P, evt);
            kos_term_free(evt);
            /* 按小步执行（每步执行一个操作：verify/reduce/update/confirm） */
            int steps = 0;
            while (kos_step_small(step_ctx) && steps < 10) steps++;
            printf("  Event-driven small-step: %d steps executed\n", steps);
        }
        kos_small_step_ctx_free(step_ctx);
        kos_event_op_registry_free(reg);
    }

    // ========== 清理资源 ==========
    printf("\n[Cleanup] Freeing resources...\n");
    
    // 释放事件对（如果队列中还有剩余）
    while (!kos_queue_is_empty(sigma->P)) {
        kos_term* remaining = kos_queue_dequeue(sigma->P);
        if (remaining) {
            kos_term_free(remaining);
        }
    }
    
    // 释放状态（会自动释放队列和知识集）
    kos_state_free(sigma);
    
    // 释放初始知识集（注意：如果状态已释放，initial_K 可能已被释放）
    // 为了安全，我们检查一下，但通常不需要再次释放
    // kos_term_free(initial_K);  // 可能已被状态释放
    
    printf("Cleanup complete.\n");
    
    return 0;
}
