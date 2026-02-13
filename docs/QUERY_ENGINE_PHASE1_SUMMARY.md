# KOS Query Engine - Phase 1 实现总结
# KOS Query Engine - Phase 1 Implementation Summary

[中文](#中文) | [English](#english)

---

<a name="中文"></a>
## 中文

## 概述

Phase 1 核心查询能力已成功实现，为 KOS-TL 系统提供了对标 Palantir 动态本体工具的基础查询功能。

## 已实现功能

### 1. 查询 API (`include/kos_query.h`)

- ✅ 查询构建 API：`kos_query_create`, `kos_query_add_condition`, `kos_query_set_order_by`, `kos_query_set_limit`
- ✅ 查询执行 API：`kos_query_execute`（基于知识集 K）
- ✅ 时间范围查询 API：`kos_query_time_range`
- ✅ 路径查询 API：`kos_query_path`, `kos_query_shortest_path`
- ✅ 辅助函数：`kos_extract_instances_by_type`, `kos_extract_field`, `kos_compare_time`

### 2. 查询执行引擎 (`src/query/query_executor.c`)

- ✅ 从知识集 K 中提取指定类型的实例
- ✅ WHERE 条件过滤（支持 `=`, `!=`, `BETWEEN` 等操作符）
- ✅ ORDER BY 排序（按时间字段）
- ✅ LIMIT 限制结果数量
- ✅ 查询结果管理（内存分配和释放）

**实现特点**：
- 递归遍历知识集 K（Σ 链结构）
- 支持嵌套 Σ 类型的字段提取
- 时间值比较（支持 Unix 时间戳和 ISO 8601 格式）

### 3. 时间序列索引 (`src/query/time_index.c`)

- ✅ AVL 树实现的时间索引
- ✅ 时间范围查询（`kos_time_index_query_range`）
- ✅ 自动平衡树结构
- ✅ 支持高效的时间范围查询（O(log n + m)，m 为结果数量）

**实现特点**：
- 使用 AVL 树保证平衡性
- 中序遍历实现范围查询
- 支持动态插入和查询

### 4. 关系图索引 (`src/query/graph_index.c`)

- ✅ 哈希表实现的图索引
- ✅ 节点和边的添加（`kos_graph_index_add_node`, `kos_graph_index_add_edge`）
- ✅ 广度优先搜索（BFS）实现路径查询
- ✅ 最短路径查询（`kos_query_shortest_path`）
- ✅ 支持最大深度限制

**实现特点**：
- 使用哈希表存储节点（O(1) 查找）
- BFS 算法保证找到最短路径
- 支持路径结果的可视化输出

### 5. 查询解析器 (`src/query/query_parser.c`)

- ✅ 查询解析框架（占位实现）
- ✅ 辅助函数：`parse_comparison_op`, `parse_aggregation_op`

**待完善**：
- 完整的 KOS-QL 词法分析（可以使用 Flex）
- 完整的 KOS-QL 语法分析（可以使用 Bison）
- 语义分析和查询优化

### 6. 示例程序 (`examples/query_example.c`)

- ✅ 基础查询示例
- ✅ 时间范围查询示例
- ✅ 关系图路径查询示例
- ✅ 结合 Kernel 层的查询示例

## 文件结构

```
include/
└── kos_query.h              # 查询引擎 API 头文件

src/query/
├── query_executor.c         # 查询执行引擎
├── query_parser.c           # 查询解析器（占位实现）
├── time_index.c             # 时间序列索引
└── graph_index.c            # 关系图索引

examples/
└── query_example.c          # 查询引擎示例程序
```

## 编译和运行

```bash
# 编译
cd build
cmake --build . --config Release --target query_example

# 运行
./bin/Release/query_example.exe
```

## 使用示例

### 基础查询

```c
// 创建查询
kos_query_t* query = kos_query_create("FailEvt");

// 添加条件
kos_term* error_value = kos_mk_id("HARD_ERR");
kos_query_add_condition(query, "err", OP_EQ, error_value);

// 设置排序和限制
kos_query_set_order_by(query, "t", true);  // 按时间降序
kos_query_set_limit(query, 10);

// 执行查询
kos_query_result_t* result = kos_query_execute(query, K);

// 处理结果
for (size_t i = 0; i < result->count; i++) {
    // 处理 result->results[i]
}

// 清理
kos_query_result_free(result);
kos_query_free(query);
```

### 时间范围查询

```c
// 创建时间索引
kos_time_index_t* index = kos_time_index_create("t");

// 插入数据
kos_time_index_insert(index, event_data);

// 查询时间范围
kos_query_result_t* result = kos_time_index_query_range(
    index, "1697004000", "1697005500"
);
```

### 关系图路径查询

```c
// 创建图索引
kos_graph_index_t* graph = kos_graph_index_create("batch", "machine");

// 添加节点和边
kos_graph_index_add_node(graph, node_id, node_data);
kos_graph_index_add_edge(graph, from_node, to_node, relation_data);

// 查询最短路径
kos_path_result_t* path = kos_query_shortest_path(graph, start, end);
```

## 已知限制和待完善

1. **查询解析器**：
   - 当前只有占位实现
   - 需要完整的 KOS-QL 词法和语法分析
   - 建议使用 Flex/Bison 实现

2. **字段提取**：
   - 当前实现简化，只支持提取第一个字段
   - 需要支持完整的字段路径解析（如 "FailEvt.batch.err"）

3. **类型匹配**：
   - 当前实现简化，没有完整的类型检查
   - 需要结合 Core 层的类型检查器验证类型匹配

4. **查询优化**：
   - 当前没有查询优化器
   - 需要实现索引选择、查询计划优化等

5. **聚合操作**：
   - 当前只定义了接口，未实现聚合逻辑
   - 需要实现 SUM、AVG、MAX、MIN、COUNT 等聚合函数

## 下一步计划（Phase 2）

根据 `docs/PALANTIR_ALIGNMENT_ROADMAP.md`，下一步应该实现：

1. **动态本体管理**：
   - 本体版本管理
   - 运行时本体更新
   - 变更影响分析

2. **查询优化**：
   - 查询计划优化器
   - 索引自动选择
   - 查询缓存

3. **完整 KOS-QL 解析器**：
   - 使用 Flex/Bison 实现完整的词法和语法分析
   - 支持复杂的查询语法

---

<a name="english"></a>
## English

## Overview

Phase 1 core query capabilities have been successfully implemented, providing KOS-TL with foundational query functionality aligned with Palantir-style dynamic ontology tools.

## Implemented Features

### 1. Query API (`include/kos_query.h`)

- ✅ Query construction API: `kos_query_create`, `kos_query_add_condition`, `kos_query_set_order_by`, `kos_query_set_limit`
- ✅ Query execution API: `kos_query_execute` (based on knowledge set K)
- ✅ Time range query API: `kos_query_time_range`
- ✅ Path query API: `kos_query_path`, `kos_query_shortest_path`
- ✅ Helper functions: `kos_extract_instances_by_type`, `kos_extract_field`, `kos_compare_time`

### 2. Query Executor (`src/query/query_executor.c`)

- ✅ Extract instances of specified type from knowledge set K
- ✅ WHERE condition filtering (supports `=`, `!=`, `BETWEEN`, etc.)
- ✅ ORDER BY sorting (by time field)
- ✅ LIMIT result count
- ✅ Query result management (memory allocation and deallocation)

**Implementation Features**:
- Recursive traversal of knowledge set K (Σ-chain structure)
- Support for nested Σ-type field extraction
- Time value comparison (supports Unix timestamps and ISO 8601 format)

### 3. Time Series Index (`src/query/time_index.c`)

- ✅ AVL tree-based time index
- ✅ Time range queries (`kos_time_index_query_range`)
- ✅ Automatic tree balancing
- ✅ Efficient time range queries (O(log n + m), m is result count)

**Implementation Features**:
- Uses AVL tree to ensure balance
- In-order traversal for range queries
- Supports dynamic insertion and querying

### 4. Graph Index (`src/query/graph_index.c`)

- ✅ Hash table-based graph index
- ✅ Node and edge addition (`kos_graph_index_add_node`, `kos_graph_index_add_edge`)
- ✅ Breadth-first search (BFS) for path queries
- ✅ Shortest path queries (`kos_query_shortest_path`)
- ✅ Maximum depth limit support

**Implementation Features**:
- Uses hash table for node storage (O(1) lookup)
- BFS algorithm guarantees shortest path
- Supports visualization of path results

### 5. Query Parser (`src/query/query_parser.c`)

- ✅ Query parser framework (placeholder implementation)
- ✅ Helper functions: `parse_comparison_op`, `parse_aggregation_op`

**To Be Completed**:
- Full KOS-QL lexical analysis (can use Flex)
- Full KOS-QL syntax analysis (can use Bison)
- Semantic analysis and query optimization

### 6. Example Program (`examples/query_example.c`)

- ✅ Basic query example
- ✅ Time range query example
- ✅ Graph path query example
- ✅ Query integrated with Kernel layer example

## File Structure

```
include/
└── kos_query.h              # Query engine API header

src/query/
├── query_executor.c         # Query executor
├── query_parser.c           # Query parser (placeholder)
├── time_index.c             # Time series index
└── graph_index.c            # Graph index

examples/
└── query_example.c          # Query engine example program
```

## Build and Run

```bash
# Build
cd build
cmake --build . --config Release --target query_example

# Run
./bin/Release/query_example.exe
```

## Usage Examples

### Basic Query

```c
// Create query
kos_query_t* query = kos_query_create("FailEvt");

// Add condition
kos_term* error_value = kos_mk_id("HARD_ERR");
kos_query_add_condition(query, "err", OP_EQ, error_value);

// Set ordering and limit
kos_query_set_order_by(query, "t", true);  // Descending by time
kos_query_set_limit(query, 10);

// Execute query
kos_query_result_t* result = kos_query_execute(query, K);

// Process results
for (size_t i = 0; i < result->count; i++) {
    // Process result->results[i]
}

// Cleanup
kos_query_result_free(result);
kos_query_free(query);
```

### Time Range Query

```c
// Create time index
kos_time_index_t* index = kos_time_index_create("t");

// Insert data
kos_time_index_insert(index, event_data);

// Query time range
kos_query_result_t* result = kos_time_index_query_range(
    index, "1697004000", "1697005500"
);
```

### Graph Path Query

```c
// Create graph index
kos_graph_index_t* graph = kos_graph_index_create("batch", "machine");

// Add nodes and edges
kos_graph_index_add_node(graph, node_id, node_data);
kos_graph_index_add_edge(graph, from_node, to_node, relation_data);

// Query shortest path
kos_path_result_t* path = kos_query_shortest_path(graph, start, end);
```

## Known Limitations and Future Work

1. **Query Parser**:
   - Currently only placeholder implementation
   - Needs full KOS-QL lexical and syntax analysis
   - Recommend using Flex/Bison

2. **Field Extraction**:
   - Current implementation simplified, only supports first field extraction
   - Needs full field path parsing support (e.g., "FailEvt.batch.err")

3. **Type Matching**:
   - Current implementation simplified, no full type checking
   - Needs integration with Core layer type checker for type validation

4. **Query Optimization**:
   - Currently no query optimizer
   - Needs index selection, query plan optimization, etc.

5. **Aggregation Operations**:
   - Currently only interface defined, aggregation logic not implemented
   - Needs implementation of SUM, AVG, MAX, MIN, COUNT aggregation functions

## Next Steps (Phase 2)

According to `docs/PALANTIR_ALIGNMENT_ROADMAP.md`, next steps should include:

1. **Dynamic Ontology Management**:
   - Ontology version management
   - Runtime ontology updates
   - Change impact analysis

2. **Query Optimization**:
   - Query plan optimizer
   - Automatic index selection
   - Query caching

3. **Full KOS-QL Parser**:
   - Use Flex/Bison for full lexical and syntax analysis
   - Support complex query syntax
