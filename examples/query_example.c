// examples/query_example.c
// KOS Query Engine 示例程序
// 演示 Phase 1 核心查询能力

#include "../include/kos_core.h"
#include "../include/kos_kernel.h"
#include "../include/kos_query.h"
#include "../include/kos_runtime.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// ========== 辅助函数：创建测试数据 ==========

// 创建失败事件实例
kos_term* create_failure_event(const char* batch_id, const char* error_code, const char* time_str) {
    kos_term* batch_term = kos_mk_id(batch_id);
    kos_term* error_term = kos_mk_id(error_code);
    kos_term* time_term = kos_mk_time(time_str);
    kos_term* proof_term = kos_mk_prop("FailureProof");
    
    kos_term* pair_t_proof = kos_mk_pair(time_term, proof_term);
    kos_term* pair_e_rest = kos_mk_pair(error_term, pair_t_proof);
    kos_term* fail_evt = kos_mk_pair(batch_term, pair_e_rest);
    
    return fail_evt;
}

// 创建异常事件实例
kos_term* create_anomaly_event(const char* machine_id, const char* param_name, 
                               const char* value_str, const char* time_str) {
    kos_term* machine_term = kos_mk_id(machine_id);
    kos_term* param_term = kos_mk_id(param_name);
    kos_term* value_term = kos_mk_val(value_str);
    kos_term* time_term = kos_mk_time(time_str);
    kos_term* proof_term = kos_mk_prop("AnomalyProof");
    
    kos_term* pair_t_proof = kos_mk_pair(time_term, proof_term);
    kos_term* pair_v_rest = kos_mk_pair(value_term, pair_t_proof);
    kos_term* pair_p_rest = kos_mk_pair(param_term, pair_v_rest);
    kos_term* anomaly_evt = kos_mk_pair(machine_term, pair_p_rest);
    
    return anomaly_evt;
}

// ========== 示例 1: 基础查询 ==========

void example_basic_query(void) {
    printf("\n=== Example 1: Basic Query ===\n");
    
    // 创建初始知识集
    kos_term* initial_K = NULL;
    
    // 添加一些失败事件到知识集
    kos_term* evt1 = create_failure_event("Batch_001", "HARD_ERR", "1697004000");
    kos_term* evt2 = create_failure_event("Batch_002", "SOFT_ERR", "1697005000");
    kos_term* evt3 = create_failure_event("Batch_003", "HARD_ERR", "1697006000");
    
    // 构建知识集（使用 Σ 链）
    initial_K = kos_mk_pair(evt1, initial_K);
    initial_K = kos_mk_pair(evt2, initial_K);
    initial_K = kos_mk_pair(evt3, initial_K);
    
    // 创建查询：查找所有 HARD_ERR 错误
    kos_query_t* query = kos_query_create("FailEvt");
    
    kos_term* error_value = kos_mk_id("HARD_ERR");
    kos_query_add_condition(query, "err", OP_EQ, error_value);
    kos_term_free(error_value);
    
    // 执行查询
    kos_query_result_t* result = kos_query_execute(query, initial_K);
    
    if (result) {
        printf("Query executed successfully. Found %zu results:\n", result->count);
        for (size_t i = 0; i < result->count; i++) {
            printf("  Result %zu: ", i + 1);
            kos_serialized* serialized = kos_term_serialize(result->results[i]);
            if (serialized && serialized->data) {
                printf("%s\n", serialized->data);
                kos_serialized_free(serialized);
            }
        }
        
        kos_query_result_free(result);
    } else {
        printf("Query execution failed.\n");
    }
    
    kos_query_free(query);
    kos_term_free(initial_K);
}

// ========== 示例 2: 时间范围查询 ==========

void example_time_range_query(void) {
    printf("\n=== Example 2: Time Range Query ===\n");
    
    // 创建时间索引
    kos_time_index_t* time_index = kos_time_index_create("t");
    
    // 添加事件到索引
    kos_term* evt1 = create_failure_event("Batch_001", "HARD_ERR", "1697004000");
    kos_term* evt2 = create_failure_event("Batch_002", "SOFT_ERR", "1697005000");
    kos_term* evt3 = create_failure_event("Batch_003", "HARD_ERR", "1697006000");
    
    kos_time_index_insert(time_index, evt1);
    kos_time_index_insert(time_index, evt2);
    kos_time_index_insert(time_index, evt3);
    
    // 查询时间范围：1697004000 到 1697005500
    kos_query_result_t* result = kos_time_index_query_range(
        time_index,
        "1697004000",
        "1697005500"
    );
    
    if (result) {
        printf("Time range query executed. Found %zu results:\n", result->count);
        for (size_t i = 0; i < result->count; i++) {
            printf("  Result %zu: ", i + 1);
            kos_serialized* serialized = kos_term_serialize(result->results[i]);
            if (serialized && serialized->data) {
                printf("%s\n", serialized->data);
                kos_serialized_free(serialized);
            }
        }
        
        kos_query_result_free(result);
    } else {
        printf("Time range query failed.\n");
    }
    
    kos_term_free(evt1);
    kos_term_free(evt2);
    kos_term_free(evt3);
    kos_time_index_free(time_index);
}

// ========== 示例 3: 关系图查询 ==========

void example_graph_query(void) {
    printf("\n=== Example 3: Graph Path Query ===\n");
    
    // 创建关系图索引（批次 -> 机器）
    kos_graph_index_t* graph = kos_graph_index_create("batch", "machine");
    
    // 创建节点和边
    kos_term* batch1 = kos_mk_id("Batch_001");
    kos_term* batch2 = kos_mk_id("Batch_002");
    kos_term* machine1 = kos_mk_id("Machine_A");
    kos_term* machine2 = kos_mk_id("Machine_B");
    
    // 添加节点
    kos_graph_index_add_node(graph, batch1, NULL);
    kos_graph_index_add_node(graph, batch2, NULL);
    kos_graph_index_add_node(graph, machine1, NULL);
    kos_graph_index_add_node(graph, machine2, NULL);
    
    // 添加边：Batch_001 -> Machine_A -> Machine_B -> Batch_002
    kos_term* rel1 = kos_mk_prop("processed_by");
    kos_term* rel2 = kos_mk_prop("next_step");
    kos_term* rel3 = kos_mk_prop("produces");
    
    kos_graph_index_add_edge(graph, batch1, machine1, rel1);
    kos_graph_index_add_edge(graph, machine1, machine2, rel2);
    kos_graph_index_add_edge(graph, machine2, batch2, rel3);
    
    // 查询路径：Batch_001 到 Batch_002
    kos_path_result_t* path = kos_query_shortest_path(graph, batch1, batch2);
    
    if (path) {
        printf("Path found (length=%zu):\n", path->length);
        for (size_t i = 0; i < path->length; i++) {
            kos_serialized* node_serialized = kos_term_serialize(path->path[i]);
            if (node_serialized && node_serialized->data) {
                printf("  Step %zu: %s\n", i + 1, node_serialized->data);
                kos_serialized_free(node_serialized);
            }
        }
        
        kos_path_result_free(path);
    } else {
        printf("No path found.\n");
    }
    
    kos_term_free(batch1);
    kos_term_free(batch2);
    kos_term_free(machine1);
    kos_term_free(machine2);
    kos_term_free(rel1);
    kos_term_free(rel2);
    kos_term_free(rel3);
    kos_graph_index_free(graph);
}

// ========== 示例 4: 结合 Kernel 层的查询 ==========

void example_kernel_query(void) {
    printf("\n=== Example 4: Query on Kernel State ===\n");
    
    // 创建 Kernel 状态
    kos_term* initial_K = NULL;
    kos_state_t* sigma = kos_state_create(initial_K);
    
    // 创建并添加事件
    kos_term* evt1 = create_failure_event("Batch_001", "HARD_ERR", "1697004000");
    kos_term* evt2 = create_failure_event("Batch_002", "SOFT_ERR", "1697005000");
    
    // 将事件添加到知识集（通过状态演化）
    sigma->K = kos_mk_pair(evt1, sigma->K);
    sigma->K = kos_mk_pair(evt2, sigma->K);
    
    // 从 Kernel 状态查询
    const kos_term* K = kos_state_get_K(sigma);
    
    kos_query_t* query = kos_query_create("FailEvt");
    kos_query_set_order_by(query, "t", true); // 按时间降序
    kos_query_set_limit(query, 10);
    
    kos_query_result_t* result = kos_query_execute(query, K);
    
    if (result) {
        printf("Query on kernel state executed. Found %zu results:\n", result->count);
        for (size_t i = 0; i < result->count; i++) {
            printf("  Result %zu: ", i + 1);
            kos_serialized* serialized = kos_term_serialize(result->results[i]);
            if (serialized && serialized->data) {
                printf("%s\n", serialized->data);
                kos_serialized_free(serialized);
            }
        }
        
        kos_query_result_free(result);
    }
    
    kos_query_free(query);
    kos_state_free(sigma);
}

// ========== 主程序 ==========

int main(void) {
    printf("========================================\n");
    printf("KOS Query Engine Demo\n");
    printf("Phase 1: Core Query Capabilities\n");
    printf("========================================\n");
    
    // 运行示例
    example_basic_query();
    example_time_range_query();
    example_graph_query();
    example_kernel_query();
    
    printf("\n========================================\n");
    printf("Demo completed successfully!\n");
    printf("========================================\n");
    
    return 0;
}
