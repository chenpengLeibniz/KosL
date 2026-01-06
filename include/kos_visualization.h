// include/kos_visualization.h
// 类型依存关系可视化工具

#ifndef KOS_VISUALIZATION_H
#define KOS_VISUALIZATION_H

#include "kos_ontology.h"
#include <stdbool.h>

// ========== 类型依存关系提取 ==========

// 类型引用信息（用于构建依存关系图）
typedef struct {
    char* type_name;           // 被引用的类型名称
    char* reference_kind;      // 引用方式："domain", "body", "left", "right"
    int depth;                 // 在类型树中的深度
} TypeReference;

// 类型依存关系图
typedef struct {
    char* type_name;              // 类型名称
    TypeReference* dependencies;  // 依赖的类型列表
    size_t dependency_count;      // 依赖数量
    char* type_kind;              // 类型种类："ID", "TIME", "PROP", "SIGMA", "PI", "SUM"
} TypeDependencyNode;

// 类型依存关系图（完整的图结构）
typedef struct {
    TypeDependencyNode* nodes;    // 节点数组
    size_t node_count;            // 节点数量
    size_t capacity;              // 数组容量
} TypeDependencyGraph;

// 从类型本体提取依存关系图
TypeDependencyGraph* kos_visualization_extract_dependencies(TypeOntology* ontology);

// 释放依存关系图
void kos_visualization_free_graph(TypeDependencyGraph* graph);

// ========== GraphViz DOT 格式输出 ==========

// 生成GraphViz DOT格式的字符串
// 返回的字符串需要使用free()释放
char* kos_visualization_generate_dot(TypeDependencyGraph* graph);

// 生成GraphViz DOT文件
// 返回0表示成功，-1表示失败
int kos_visualization_save_dot(TypeDependencyGraph* graph, const char* filename);

// ========== HTML 可视化（使用Mermaid.js） ==========

// 生成HTML可视化页面（使用Mermaid.js）
// 返回的字符串需要使用free()释放
char* kos_visualization_generate_html(TypeDependencyGraph* graph, const char* title);

// 生成HTML可视化文件
// 返回0表示成功，-1表示失败
int kos_visualization_save_html(TypeDependencyGraph* graph, const char* filename, const char* title);

// ========== 文本树状图输出 ==========

// 生成文本树状图字符串
// 返回的字符串需要使用free()释放
char* kos_visualization_generate_text_tree(TypeDependencyGraph* graph);

// 打印文本树状图到控制台
void kos_visualization_print_text_tree(TypeDependencyGraph* graph);

// ========== JSON 依存关系输出 ==========

// 生成JSON格式的依存关系数据
// 返回的字符串需要使用free()释放
char* kos_visualization_generate_json(TypeDependencyGraph* graph);

// 生成JSON格式的依存关系文件
// 返回0表示成功，-1表示失败
int kos_visualization_save_json(TypeDependencyGraph* graph, const char* filename);

// ========== 便捷函数 ==========

// 从JSON文件直接生成可视化文件（自动检测格式）
// format: "dot", "html", "text", "json", "all"
// 返回0表示成功，-1表示失败
int kos_visualization_from_json_file(const char* input_json_file, 
                                     const char* output_file_prefix,
                                     const char* format);

#endif // KOS_VISUALIZATION_H













