// src/query/graph_index.c
// KOS Query Engine - Graph Index
// 关系图索引，支持图遍历和路径查询

#include "../../include/kos_query.h"
#include "../../include/kos_core.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

#define DEFAULT_HASH_CAPACITY 16

// ========== 哈希函数 ==========

// 简单的字符串哈希函数（用于节点 ID）
static size_t hash_node_id(kos_term* node_id, size_t capacity) {
    if (!node_id) {
        return 0;
    }
    
    // 提取节点 ID 的字符串表示
    char* id_str = NULL;
    if (node_id->kind == KOS_ID && node_id->data.atomic.val) {
        id_str = node_id->data.atomic.val;
    } else if (node_id->kind == KOS_VAL && node_id->data.atomic.val) {
        id_str = node_id->data.atomic.val;
    }
    
    if (!id_str) {
        return 0;
    }
    
    // 简单哈希
    size_t hash = 0;
    for (const char* p = id_str; *p; p++) {
        hash = hash * 31 + *p;
    }
    
    return hash % capacity;
}

// 比较两个节点 ID 是否相等
static bool node_id_equal(kos_term* id1, kos_term* id2) {
    if (!id1 || !id2) {
        return false;
    }
    
    char* str1 = NULL;
    char* str2 = NULL;
    
    if (id1->kind == KOS_ID && id1->data.atomic.val) {
        str1 = id1->data.atomic.val;
    } else if (id1->kind == KOS_VAL && id1->data.atomic.val) {
        str1 = id1->data.atomic.val;
    }
    
    if (id2->kind == KOS_ID && id2->data.atomic.val) {
        str2 = id2->data.atomic.val;
    } else if (id2->kind == KOS_VAL && id2->data.atomic.val) {
        str2 = id2->data.atomic.val;
    }
    
    if (!str1 || !str2) {
        return false;
    }
    
    return strcmp(str1, str2) == 0;
}

// ========== 图节点和边操作 ==========

// 创建图节点
static graph_node_t* graph_node_create(kos_term* node_id, kos_term* data) {
    graph_node_t* node = (graph_node_t*)calloc(1, sizeof(graph_node_t));
    if (!node) {
        return NULL;
    }
    
    node->node_id = node_id ? kos_term_copy(node_id) : NULL;
    node->data = data ? kos_term_copy(data) : NULL;
    node->edges = NULL;
    node->next = NULL;
    
    return node;
}

// 释放图节点
static void graph_node_free(graph_node_t* node) {
    if (!node) {
        return;
    }
    
    if (node->node_id) {
        kos_term_free(node->node_id);
    }
    
    if (node->data) {
        kos_term_free(node->data);
    }
    
    // 释放边列表
    graph_edge_t* edge = node->edges;
    while (edge) {
        graph_edge_t* next = edge->next;
        if (edge->relation_data) {
            kos_term_free(edge->relation_data);
        }
        free(edge);
        edge = next;
    }
    
    free(node);
}

// 创建图边
static graph_edge_t* graph_edge_create(graph_node_t* to, kos_term* relation_data) {
    graph_edge_t* edge = (graph_edge_t*)calloc(1, sizeof(graph_edge_t));
    if (!edge) {
        return NULL;
    }
    
    edge->to = to;
    edge->relation_data = relation_data ? kos_term_copy(relation_data) : NULL;
    edge->next = NULL;
    
    return edge;
}

// ========== 图索引 API ==========

// 创建关系图索引
kos_graph_index_t* kos_graph_index_create(const char* from_field, const char* to_field) {
    kos_graph_index_t* graph = (kos_graph_index_t*)calloc(1, sizeof(kos_graph_index_t));
    if (!graph) {
        return NULL;
    }
    
    graph->capacity = DEFAULT_HASH_CAPACITY;
    graph->nodes = (graph_node_t**)calloc(graph->capacity, sizeof(graph_node_t*));
    if (!graph->nodes) {
        free(graph);
        return NULL;
    }
    
    graph->count = 0;
    graph->from_field = strdup(from_field ? from_field : "from");
    graph->to_field = strdup(to_field ? to_field : "to");
    
    return graph;
}

// 查找或创建节点
static graph_node_t* find_or_create_node(kos_graph_index_t* graph, kos_term* node_id) {
    if (!graph || !node_id) {
        return NULL;
    }
    
    size_t hash = hash_node_id(node_id, graph->capacity);
    graph_node_t* node = graph->nodes[hash];
    
    // 在哈希链中查找
    while (node) {
        if (node_id_equal(node->node_id, node_id)) {
            return node;
        }
        node = node->next;
    }
    
    // 未找到，创建新节点
    node = graph_node_create(node_id, NULL);
    if (!node) {
        return NULL;
    }
    
    // 插入到哈希链头部
    node->next = graph->nodes[hash];
    graph->nodes[hash] = node;
    graph->count++;
    
    return node;
}

// 向图添加节点
int kos_graph_index_add_node(kos_graph_index_t* graph, kos_term* node_id, kos_term* data) {
    if (!graph || !node_id) {
        return -1;
    }
    
    graph_node_t* node = find_or_create_node(graph, node_id);
    if (!node) {
        return -1;
    }
    
    // 更新节点数据
    if (node->data) {
        kos_term_free(node->data);
    }
    node->data = data ? kos_term_copy(data) : NULL;
    
    return 0;
}

// 向图添加边（关系）
int kos_graph_index_add_edge(kos_graph_index_t* graph,
                             kos_term* from_node_id,
                             kos_term* to_node_id,
                             kos_term* relation_data) {
    if (!graph || !from_node_id || !to_node_id) {
        return -1;
    }
    
    // 查找或创建源节点和目标节点
    graph_node_t* from_node = find_or_create_node(graph, from_node_id);
    graph_node_t* to_node = find_or_create_node(graph, to_node_id);
    
    if (!from_node || !to_node) {
        return -1;
    }
    
    // 创建边并添加到源节点的边列表
    graph_edge_t* edge = graph_edge_create(to_node, relation_data);
    if (!edge) {
        return -1;
    }
    
    edge->next = from_node->edges;
    from_node->edges = edge;
    
    return 0;
}

// 查找节点
graph_node_t* kos_graph_index_find_node(kos_graph_index_t* graph, kos_term* node_id) {
    if (!graph || !node_id) {
        return NULL;
    }
    
    size_t hash = hash_node_id(node_id, graph->capacity);
    graph_node_t* node = graph->nodes[hash];
    
    while (node) {
        if (node_id_equal(node->node_id, node_id)) {
            return node;
        }
        node = node->next;
    }
    
    return NULL;
}

// ========== 图遍历算法 ==========

// BFS 队列节点
typedef struct bfs_queue_node {
    graph_node_t* node;
    kos_term** path;           // 路径节点数组
    size_t path_length;        // 路径长度
    struct bfs_queue_node* next;
} bfs_queue_node_t;

// BFS 队列
typedef struct {
    bfs_queue_node_t* front;
    bfs_queue_node_t* rear;
} bfs_queue_t;

static bfs_queue_t* bfs_queue_create(void) {
    return (bfs_queue_t*)calloc(1, sizeof(bfs_queue_t));
}

static void bfs_queue_enqueue(bfs_queue_t* queue, graph_node_t* node, kos_term** path, size_t path_length) {
    bfs_queue_node_t* qnode = (bfs_queue_node_t*)calloc(1, sizeof(bfs_queue_node_t));
    if (!qnode) {
        return;
    }
    
    qnode->node = node;
    qnode->path_length = path_length;
    qnode->path = (kos_term**)malloc(path_length * sizeof(kos_term*));
    if (qnode->path) {
        for (size_t i = 0; i < path_length; i++) {
            qnode->path[i] = path[i] ? kos_term_copy(path[i]) : NULL;
        }
    }
    qnode->next = NULL;
    
    if (!queue->rear) {
        queue->front = queue->rear = qnode;
    } else {
        queue->rear->next = qnode;
        queue->rear = qnode;
    }
}

static bfs_queue_node_t* bfs_queue_dequeue(bfs_queue_t* queue) {
    if (!queue->front) {
        return NULL;
    }
    
    bfs_queue_node_t* qnode = queue->front;
    queue->front = queue->front->next;
    if (!queue->front) {
        queue->rear = NULL;
    }
    
    return qnode;
}

static void bfs_queue_free(bfs_queue_t* queue) {
    while (queue->front) {
        bfs_queue_node_t* qnode = bfs_queue_dequeue(queue);
        if (qnode) {
            if (qnode->path) {
                for (size_t i = 0; i < qnode->path_length; i++) {
                    if (qnode->path[i]) {
                        kos_term_free(qnode->path[i]);
                    }
                }
                free(qnode->path);
            }
            free(qnode);
        }
    }
    free(queue);
}

// 广度优先搜索（BFS）
kos_path_result_t* kos_graph_index_bfs(kos_graph_index_t* graph,
                                      kos_term* start_node_id,
                                      kos_term* target_node_id,
                                      int max_depth) {
    if (!graph || !start_node_id || !target_node_id) {
        return NULL;
    }
    
    graph_node_t* start_node = kos_graph_index_find_node(graph, start_node_id);
    if (!start_node) {
        return NULL;
    }
    
    // 如果起始节点就是目标节点
    if (node_id_equal(start_node->node_id, target_node_id)) {
        kos_path_result_t* result = (kos_path_result_t*)calloc(1, sizeof(kos_path_result_t));
        if (result) {
            result->path = (kos_term**)malloc(sizeof(kos_term*));
            result->path[0] = kos_term_copy(start_node->node_id);
            result->length = 1;
            result->total_cost = 0;
        }
        return result;
    }
    
    bfs_queue_t* queue = bfs_queue_create();
    if (!queue) {
        return NULL;
    }
    
    // 初始化路径
    kos_term** initial_path = (kos_term**)malloc(sizeof(kos_term*));
    initial_path[0] = kos_term_copy(start_node->node_id);
    
    bfs_queue_enqueue(queue, start_node, initial_path, 1);
    free(initial_path);
    
    // 访问标记（简化实现：使用节点 ID 比较）
    // 实际应该使用哈希集合
    
    while (queue->front) {
        bfs_queue_node_t* qnode = bfs_queue_dequeue(queue);
        if (!qnode) {
            break;
        }
        
        graph_node_t* current = qnode->node;
        
        // 检查深度限制
        if (max_depth > 0 && qnode->path_length >= (size_t)max_depth) {
            // 释放当前节点
            if (qnode->path) {
                for (size_t i = 0; i < qnode->path_length; i++) {
                    if (qnode->path[i]) {
                        kos_term_free(qnode->path[i]);
                    }
                }
                free(qnode->path);
            }
            free(qnode);
            continue;
        }
        
        // 遍历当前节点的所有边
        graph_edge_t* edge = current->edges;
        while (edge) {
            graph_node_t* next_node = edge->to;
            
            // 检查是否到达目标节点
            if (node_id_equal(next_node->node_id, target_node_id)) {
                // 找到路径
                kos_path_result_t* result = (kos_path_result_t*)calloc(1, sizeof(kos_path_result_t));
                if (result) {
                    result->length = qnode->path_length + 1;
                    result->path = (kos_term**)malloc(result->length * sizeof(kos_term*));
                    if (result->path) {
                        // 复制路径
                        for (size_t i = 0; i < qnode->path_length; i++) {
                            result->path[i] = kos_term_copy(qnode->path[i]);
                        }
                        result->path[qnode->path_length] = kos_term_copy(next_node->node_id);
                    }
                    result->total_cost = (int)result->length;
                }
                
                // 清理队列
                bfs_queue_free(queue);
                
                // 释放当前节点
                if (qnode->path) {
                    for (size_t i = 0; i < qnode->path_length; i++) {
                        if (qnode->path[i]) {
                            kos_term_free(qnode->path[i]);
                        }
                    }
                    free(qnode->path);
                }
                free(qnode);
                
                return result;
            }
            
            // 添加到队列（简化实现：不检查重复访问）
            kos_term** new_path = (kos_term**)malloc((qnode->path_length + 1) * sizeof(kos_term*));
            if (new_path) {
                for (size_t i = 0; i < qnode->path_length; i++) {
                    new_path[i] = kos_term_copy(qnode->path[i]);
                }
                new_path[qnode->path_length] = kos_term_copy(next_node->node_id);
                
                bfs_queue_enqueue(queue, next_node, new_path, qnode->path_length + 1);
                
                free(new_path);
            }
            
            edge = edge->next;
        }
        
        // 释放当前节点
        if (qnode->path) {
            for (size_t i = 0; i < qnode->path_length; i++) {
                if (qnode->path[i]) {
                    kos_term_free(qnode->path[i]);
                }
            }
            free(qnode->path);
        }
        free(qnode);
    }
    
    bfs_queue_free(queue);
    
    // 未找到路径
    return NULL;
}

// 深度优先搜索（DFS）- 简化实现，类似 BFS
kos_path_result_t* kos_graph_index_dfs(kos_graph_index_t* graph,
                                      kos_term* start_node_id,
                                      kos_term* target_node_id,
                                      int max_depth) {
    // 简化实现：使用 BFS，实际应该使用递归或栈
    return kos_graph_index_bfs(graph, start_node_id, target_node_id, max_depth);
}

// ========== 路径查询 API ==========

// 执行路径查询
kos_path_result_t* kos_query_path(kos_graph_index_t* graph,
                                  kos_term* from_node,
                                  kos_term* to_node,
                                  int max_depth) {
    return kos_graph_index_bfs(graph, from_node, to_node, max_depth);
}

// 执行最短路径查询
kos_path_result_t* kos_query_shortest_path(kos_graph_index_t* graph,
                                           kos_term* from_node,
                                           kos_term* to_node) {
    // BFS 天然找到最短路径
    return kos_graph_index_bfs(graph, from_node, to_node, -1);
}

// 释放路径查询结果
void kos_path_result_free(kos_path_result_t* result) {
    if (!result) {
        return;
    }
    
    if (result->path) {
        for (size_t i = 0; i < result->length; i++) {
            if (result->path[i]) {
                kos_term_free(result->path[i]);
            }
        }
        free(result->path);
    }
    
    free(result);
}

// ========== 图索引清理 ==========

// 释放图索引
void kos_graph_index_free(kos_graph_index_t* graph) {
    if (!graph) {
        return;
    }
    
    if (graph->nodes) {
        for (size_t i = 0; i < graph->capacity; i++) {
            graph_node_t* node = graph->nodes[i];
            while (node) {
                graph_node_t* next = node->next;
                graph_node_free(node);
                node = next;
            }
        }
        free(graph->nodes);
    }
    
    if (graph->from_field) {
        free(graph->from_field);
    }
    
    if (graph->to_field) {
        free(graph->to_field);
    }
    
    free(graph);
}
