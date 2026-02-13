// include/kos_query.h
// KOS Query Engine API - Phase 1: Core Query Capabilities
// 对标 Palantir 动态本体工具的核心查询能力

#ifndef KOS_QUERY_H
#define KOS_QUERY_H

#include "kos_core.h"
#include "kos_kernel.h"
#include <stdbool.h>
#include <stddef.h>

// ========== 查询语言类型定义 ==========

// 查询操作类型
typedef enum {
    QUERY_SELECT,      // SELECT 查询
    QUERY_WHERE,       // WHERE 条件
    QUERY_ORDER_BY,    // ORDER BY 排序
    QUERY_LIMIT,       // LIMIT 限制
    QUERY_TIME_RANGE,  // 时间范围查询
    QUERY_PATH,        // 路径查询
    QUERY_SHORTEST_PATH // 最短路径查询
} query_op_type_t;

// 聚合操作类型
typedef enum {
    AGG_NONE,    // 无聚合
    AGG_SUM,     // SUM
    AGG_AVG,     // AVG
    AGG_MAX,     // MAX
    AGG_MIN,     // MIN
    AGG_COUNT    // COUNT
} aggregation_op_t;

// 比较操作符
typedef enum {
    OP_EQ,       // =
    OP_NE,       // !=
    OP_LT,       // <
    OP_LE,       // <=
    OP_GT,       // >
    OP_GE,       // >=
    OP_IN,       // IN
    OP_BETWEEN,  // BETWEEN
    OP_LIKE      // LIKE
} comparison_op_t;

// 查询条件结构
typedef struct query_condition {
    char* field_name;           // 字段名（如 "FailEvt.t"）
    comparison_op_t op;          // 比较操作符
    kos_term* value;             // 比较值
    struct query_condition* next; // 下一个条件（AND 连接）
} query_condition_t;

// 查询语句结构
typedef struct {
    char* type_name;             // 要查询的类型名（如 "FailEvt"）
    query_condition_t* conditions; // WHERE 条件列表
    char* order_by_field;       // ORDER BY 字段
    bool order_desc;            // 是否降序
    size_t limit;               // LIMIT 数量（0 表示无限制）
    aggregation_op_t aggregation; // 聚合操作
    char* aggregation_field;   // 聚合字段
} kos_query_t;

// 查询结果结构
typedef struct {
    kos_term** results;         // 查询结果数组
    size_t count;               // 结果数量
    size_t capacity;            // 数组容量
} kos_query_result_t;

// ========== 时间序列索引 ==========

// 时间索引节点（B+树节点）
typedef struct time_index_node {
    char* time_value;           // 时间值（字符串格式）
    kos_term* data;             // 关联的数据项
    struct time_index_node* left;
    struct time_index_node* right;
    int height;                 // 树高度（用于平衡）
} time_index_node_t;

// 时间序列索引
typedef struct {
    time_index_node_t* root;    // B+树根节点
    size_t count;              // 索引项数量
    char* time_field;          // 时间字段名（如 "t"）
} kos_time_index_t;

// ========== 关系图索引 ==========

// 图节点
typedef struct graph_node {
    kos_term* node_id;          // 节点标识符
    kos_term* data;             // 节点数据
    struct graph_edge* edges;   // 出边列表
    struct graph_node* next;    // 下一个节点（哈希表链）
} graph_node_t;

// 图边
typedef struct graph_edge {
    graph_node_t* to;           // 目标节点
    kos_term* relation_data;    // 关系数据
    struct graph_edge* next;    // 下一条边
} graph_edge_t;

// 关系图索引
typedef struct {
    graph_node_t** nodes;       // 节点哈希表
    size_t capacity;           // 哈希表容量
    size_t count;              // 节点数量
    char* from_field;          // 源字段名（如 "batch"）
    char* to_field;            // 目标字段名（如 "machine"）
} kos_graph_index_t;

// 路径查询结果
typedef struct {
    kos_term** path;            // 路径节点数组
    size_t length;              // 路径长度
    int total_cost;             // 总代价（用于最短路径）
} kos_path_result_t;

// ========== 查询 API ==========

// --- 查询构建 ---

// 创建查询对象
kos_query_t* kos_query_create(const char* type_name);

// 添加 WHERE 条件
int kos_query_add_condition(kos_query_t* query,
                           const char* field_name,
                           comparison_op_t op,
                           kos_term* value);

// 设置 ORDER BY
int kos_query_set_order_by(kos_query_t* query, const char* field_name, bool desc);

// 设置 LIMIT
int kos_query_set_limit(kos_query_t* query, size_t limit);

// 设置聚合操作
int kos_query_set_aggregation(kos_query_t* query, aggregation_op_t op, const char* field_name);

// 释放查询对象
void kos_query_free(kos_query_t* query);

// --- 查询执行 ---

// 执行查询（基于知识集 K）
kos_query_result_t* kos_query_execute(kos_query_t* query, const kos_term* K);

// 执行时间范围查询
kos_query_result_t* kos_query_time_range(kos_time_index_t* index,
                                        const char* start_time,
                                        const char* end_time);

// 执行路径查询
kos_path_result_t* kos_query_path(kos_graph_index_t* graph,
                                  kos_term* from_node,
                                  kos_term* to_node,
                                  int max_depth);

// 执行最短路径查询
kos_path_result_t* kos_query_shortest_path(kos_graph_index_t* graph,
                                           kos_term* from_node,
                                           kos_term* to_node);

// 释放查询结果
void kos_query_result_free(kos_query_result_t* result);

// 释放路径查询结果
void kos_path_result_free(kos_path_result_t* result);

// --- 查询解析（KOS-QL）---

// 从字符串解析查询（KOS-QL 语法）
kos_query_t* kos_query_parse(const char* query_str);

// KOS-QL 示例语法：
// SELECT FailEvt FROM Knowledge K WHERE FailEvt.t BETWEEN '2023-10-01' AND '2023-10-31'
// SELECT AnomalyEvt FROM Knowledge K WHERE AnomalyEvt.machine = 'HeatTreatment_03'
// SELECT * FROM Knowledge K ORDER BY FailEvt.t DESC LIMIT 10

// --- 时间序列索引 API ---

// 创建时间序列索引
kos_time_index_t* kos_time_index_create(const char* time_field);

// 向索引插入数据
int kos_time_index_insert(kos_time_index_t* index, kos_term* data);

// 从索引删除数据
int kos_time_index_delete(kos_time_index_t* index, const char* time_value);

// 时间范围查询
kos_query_result_t* kos_time_index_query_range(kos_time_index_t* index,
                                               const char* start_time,
                                               const char* end_time);

// 释放时间索引
void kos_time_index_free(kos_time_index_t* index);

// --- 关系图索引 API ---

// 创建关系图索引
kos_graph_index_t* kos_graph_index_create(const char* from_field, const char* to_field);

// 向图添加节点
int kos_graph_index_add_node(kos_graph_index_t* graph, kos_term* node_id, kos_term* data);

// 向图添加边（关系）
int kos_graph_index_add_edge(kos_graph_index_t* graph,
                             kos_term* from_node_id,
                             kos_term* to_node_id,
                             kos_term* relation_data);

// 查找节点
graph_node_t* kos_graph_index_find_node(kos_graph_index_t* graph, kos_term* node_id);

// 广度优先搜索（BFS）
kos_path_result_t* kos_graph_index_bfs(kos_graph_index_t* graph,
                                       kos_term* start_node,
                                       kos_term* target_node,
                                       int max_depth);

// 深度优先搜索（DFS）
kos_path_result_t* kos_graph_index_dfs(kos_graph_index_t* graph,
                                       kos_term* start_node,
                                       kos_term* target_node,
                                       int max_depth);

// 释放图索引
void kos_graph_index_free(kos_graph_index_t* graph);

// --- 辅助函数 ---

// 从知识集 K 中提取指定类型的所有实例
kos_query_result_t* kos_extract_instances_by_type(const kos_term* K, const char* type_name);

// 从 Σ 类型实例中提取字段值
kos_term* kos_extract_field(kos_term* instance, const char* field_path);

// 比较两个时间值（字符串格式）
int kos_compare_time(const char* time1, const char* time2);

#endif // KOS_QUERY_H
