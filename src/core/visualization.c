// src/core/visualization.c
// 类型依存关系可视化实现

#include "../../include/kos_visualization.h"
#include "../../include/kos_core.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <ctype.h>

// ========== 辅助函数：从kos_term提取类型名称 ==========

// 从ID/TIME/PROP类型的term中提取类型名称
static char* extract_type_name_from_term(kos_term* term) {
    if (!term) {
        return NULL;
    }
    
    switch (term->kind) {
        case KOS_ID:
        case KOS_TIME:
        case KOS_PROP:
            if (term->data.atomic.val) {
                size_t len = strlen(term->data.atomic.val);
                char* name = (char*)malloc(len + 1);
                if (name) {
                    strcpy(name, term->data.atomic.val);
                }
                return name;
            }
            break;
        default:
            break;
    }
    
    return NULL;
}

// 递归收集类型引用
static void collect_type_references_recursive(kos_term* term, TypeReference** refs, size_t* count, size_t* capacity, int depth) {
    if (!term) {
        return;
    }
    
    // 扩展数组容量
    if (*count >= *capacity) {
        *capacity = (*capacity == 0) ? 16 : *capacity * 2;
        *refs = (TypeReference*)realloc(*refs, *capacity * sizeof(TypeReference));
        if (!*refs) {
            return;
        }
    }
    
    // 根据term类型处理
    switch (term->kind) {
        case KOS_ID:
        case KOS_TIME:
        case KOS_PROP: {
            char* type_name = extract_type_name_from_term(term);
            if (type_name && strlen(type_name) > 0) {
                TypeReference* ref = &(*refs)[*count];
                ref->type_name = type_name;
                ref->reference_kind = (char*)malloc(16);
                if (ref->reference_kind) {
                    strcpy(ref->reference_kind, "direct");
                }
                ref->depth = depth;
                (*count)++;
            }
            break;
        }
        
        case KOS_SIGMA: {
            // 处理domain和body
            if (term->data.sigma.domain) {
                collect_type_references_recursive(term->data.sigma.domain, refs, count, capacity, depth + 1);
            }
            if (term->data.sigma.body) {
                collect_type_references_recursive(term->data.sigma.body, refs, count, capacity, depth + 1);
            }
            break;
        }
        
        case KOS_PI: {
            // 处理domain和body
            if (term->data.pi.domain) {
                collect_type_references_recursive(term->data.pi.domain, refs, count, capacity, depth + 1);
            }
            if (term->data.pi.body) {
                collect_type_references_recursive(term->data.pi.body, refs, count, capacity, depth + 1);
            }
            break;
        }
        
        case KOS_SUM: {
            // 处理left和right
            if (term->data.sum.left_type) {
                collect_type_references_recursive(term->data.sum.left_type, refs, count, capacity, depth + 1);
            }
            if (term->data.sum.right_type) {
                collect_type_references_recursive(term->data.sum.right_type, refs, count, capacity, depth + 1);
            }
            break;
        }
        
        default:
            break;
    }
}

// 获取类型种类的字符串表示
static const char* get_type_kind_string(kos_term* term) {
    if (!term) {
        return "UNKNOWN";
    }
    
    switch (term->kind) {
        case KOS_ID: return "ID";
        case KOS_TIME: return "TIME";
        case KOS_PROP: return "PROP";
        case KOS_SIGMA: return "SIGMA";
        case KOS_PI: return "PI";
        case KOS_SUM: return "SUM";
        case KOS_U: return "U";
        case KOS_TYPE: return "TYPE";
        default: return "UNKNOWN";
    }
}

// ========== 依存关系提取 ==========

TypeDependencyGraph* kos_visualization_extract_dependencies(TypeOntology* ontology) {
    if (!ontology) {
        return NULL;
    }
    
    TypeDependencyGraph* graph = (TypeDependencyGraph*)malloc(sizeof(TypeDependencyGraph));
    if (!graph) {
        return NULL;
    }
    
    graph->node_count = 0;
    graph->capacity = ontology->type_count;
    graph->nodes = (TypeDependencyNode*)malloc(graph->capacity * sizeof(TypeDependencyNode));
    if (!graph->nodes) {
        free(graph);
        return NULL;
    }
    
    // 为每个类型定义创建节点
    for (size_t i = 0; i < ontology->type_count; i++) {
        TypeDefinition* def = &ontology->type_definitions[i];
        TypeDependencyNode* node = &graph->nodes[graph->node_count];
        
        // 设置类型名称
        size_t name_len = strlen(def->name);
        node->type_name = (char*)malloc(name_len + 1);
        if (node->type_name) {
            strcpy(node->type_name, def->name);
        }
        
        // 设置类型种类
        const char* kind_str = get_type_kind_string(def->type_def);
        node->type_kind = (char*)malloc(strlen(kind_str) + 1);
        if (node->type_kind) {
            strcpy(node->type_kind, kind_str);
        }
        
        // 收集依赖
        node->dependency_count = 0;
        node->dependencies = NULL;
        size_t dep_capacity = 0;
        
        collect_type_references_recursive(def->type_def, &node->dependencies, 
                                         &node->dependency_count, &dep_capacity, 0);
        
        graph->node_count++;
    }
    
    return graph;
}

void kos_visualization_free_graph(TypeDependencyGraph* graph) {
    if (!graph) {
        return;
    }
    
    for (size_t i = 0; i < graph->node_count; i++) {
        TypeDependencyNode* node = &graph->nodes[i];
        if (node->type_name) free(node->type_name);
        if (node->type_kind) free(node->type_kind);
        
        for (size_t j = 0; j < node->dependency_count; j++) {
            TypeReference* ref = &node->dependencies[j];
            if (ref->type_name) free(ref->type_name);
            if (ref->reference_kind) free(ref->reference_kind);
        }
        
        if (node->dependencies) {
            free(node->dependencies);
        }
    }
    
    if (graph->nodes) {
        free(graph->nodes);
    }
    
    free(graph);
}

// ========== GraphViz DOT 格式输出 ==========

char* kos_visualization_generate_dot(TypeDependencyGraph* graph) {
    if (!graph) {
        return NULL;
    }
    
    size_t buffer_size = 8192;
    char* buffer = (char*)malloc(buffer_size);
    if (!buffer) {
        return NULL;
    }
    
    int pos = 0;
    
    // DOT文件头
    pos += snprintf(buffer + pos, buffer_size - pos, "digraph TypeDependencies {\n");
    pos += snprintf(buffer + pos, buffer_size - pos, "  rankdir=TB;\n");
    pos += snprintf(buffer + pos, buffer_size - pos, "  node [shape=box, style=rounded];\n\n");
    
    // 定义节点样式（按类型种类）
    pos += snprintf(buffer + pos, buffer_size - pos, "  // Node styles by type kind\n");
    pos += snprintf(buffer + pos, buffer_size - pos, "  node [color=black, fillcolor=lightblue, fontname=Arial];\n\n");
    
    // 生成节点和边
    for (size_t i = 0; i < graph->node_count; i++) {
        TypeDependencyNode* node = &graph->nodes[i];
        
        // 根据类型种类设置颜色
        const char* fillcolor = "lightblue";
        if (strcmp(node->type_kind, "SIGMA") == 0) {
            fillcolor = "lightgreen";
        } else if (strcmp(node->type_kind, "PI") == 0) {
            fillcolor = "lightyellow";
        } else if (strcmp(node->type_kind, "ID") == 0 || strcmp(node->type_kind, "TIME") == 0) {
            fillcolor = "lightgray";
        }
        
        // 节点定义：类型名称 [label="类型名称 (类型种类)", fillcolor=颜色]
        pos += snprintf(buffer + pos, buffer_size - pos, "  \"%s\" [label=\"%s\\n(%s)\", fillcolor=%s];\n", 
                       node->type_name, node->type_name, node->type_kind, fillcolor);
        
        // 生成依赖边
        for (size_t j = 0; j < node->dependency_count; j++) {
            TypeReference* ref = &node->dependencies[j];
            // 只连接已定义的节点（避免连接到未定义的类型）
            bool target_exists = false;
            for (size_t k = 0; k < graph->node_count; k++) {
                if (strcmp(graph->nodes[k].type_name, ref->type_name) == 0) {
                    target_exists = true;
                    break;
                }
            }
            
            if (target_exists) {
                pos += snprintf(buffer + pos, buffer_size - pos, "  \"%s\" -> \"%s\";\n", 
                               node->type_name, ref->type_name);
            }
        }
    }
    
    pos += snprintf(buffer + pos, buffer_size - pos, "}\n");
    
    return buffer;
}

int kos_visualization_save_dot(TypeDependencyGraph* graph, const char* filename) {
    if (!graph || !filename) {
        return -1;
    }
    
    char* dot_content = kos_visualization_generate_dot(graph);
    if (!dot_content) {
        return -1;
    }
    
    FILE* file = fopen(filename, "w");
    if (!file) {
        free(dot_content);
        return -1;
    }
    
    fprintf(file, "%s", dot_content);
    fclose(file);
    free(dot_content);
    
    return 0;
}

// ========== HTML 可视化（使用Mermaid.js） ==========

char* kos_visualization_generate_html(TypeDependencyGraph* graph, const char* title) {
    if (!graph) {
        return NULL;
    }
    
    const char* page_title = title ? title : "Type Dependency Graph";
    
    size_t buffer_size = 16384;
    char* buffer = (char*)malloc(buffer_size);
    if (!buffer) {
        return NULL;
    }
    
    int pos = 0;
    
    // HTML头部
    pos += snprintf(buffer + pos, buffer_size - pos, 
                   "<!DOCTYPE html>\n"
                   "<html>\n"
                   "<head>\n"
                   "  <meta charset=\"UTF-8\">\n"
                   "  <title>%s</title>\n"
                   "  <script src=\"https://cdn.jsdelivr.net/npm/mermaid/dist/mermaid.min.js\"></script>\n"
                   "  <style>\n"
                   "    body { font-family: Arial, sans-serif; margin: 20px; }\n"
                   "    h1 { color: #333; }\n"
                   "    #mermaid-diagram { margin: 20px 0; }\n"
                   "  </style>\n"
                   "</head>\n"
                   "<body>\n"
                   "  <h1>%s</h1>\n"
                   "  <div id=\"mermaid-diagram\"></div>\n"
                   "  <script>\n"
                   "    mermaid.initialize({ startOnLoad: true, theme: 'default' });\n"
                   "  </script>\n"
                   "  <pre class=\"mermaid\">\n"
                   "graph TB\n", page_title, page_title);
    
    // 生成Mermaid图表定义
    for (size_t i = 0; i < graph->node_count; i++) {
        TypeDependencyNode* node = &graph->nodes[i];
        
        // 节点定义（使用样式类）
        const char* style_class = "";
        if (strcmp(node->type_kind, "SIGMA") == 0) {
            style_class = ":::sigma";
        } else if (strcmp(node->type_kind, "PI") == 0) {
            style_class = ":::pi";
        } else if (strcmp(node->type_kind, "ID") == 0 || strcmp(node->type_kind, "TIME") == 0) {
            style_class = ":::base";
        }
        
        // 生成依赖边
        for (size_t j = 0; j < node->dependency_count; j++) {
            TypeReference* ref = &node->dependencies[j];
            // 检查目标节点是否存在
            bool target_exists = false;
            for (size_t k = 0; k < graph->node_count; k++) {
                if (strcmp(graph->nodes[k].type_name, ref->type_name) == 0) {
                    target_exists = true;
                    break;
                }
            }
            
            if (target_exists) {
                pos += snprintf(buffer + pos, buffer_size - pos, "    %s --> %s\n", 
                               node->type_name, ref->type_name);
            }
        }
    }
    
    // 添加样式定义
    pos += snprintf(buffer + pos, buffer_size - pos, 
                   "    classDef sigma fill:#90EE90,stroke:#333,stroke-width:2px\n"
                   "    classDef pi fill:#FFE4B5,stroke:#333,stroke-width:2px\n"
                   "    classDef base fill:#D3D3D3,stroke:#333,stroke-width:2px\n"
                   "    classDef prop fill:#87CEEB,stroke:#333,stroke-width:2px\n");
    
    // HTML尾部
    pos += snprintf(buffer + pos, buffer_size - pos, 
                   "  </pre>\n"
                   "</body>\n"
                   "</html>\n");
    
    return buffer;
}

int kos_visualization_save_html(TypeDependencyGraph* graph, const char* filename, const char* title) {
    if (!graph || !filename) {
        return -1;
    }
    
    char* html_content = kos_visualization_generate_html(graph, title);
    if (!html_content) {
        return -1;
    }
    
    FILE* file = fopen(filename, "w");
    if (!file) {
        free(html_content);
        return -1;
    }
    
    fprintf(file, "%s", html_content);
    fclose(file);
    free(html_content);
    
    return 0;
}

// ========== 文本树状图输出 ==========

char* kos_visualization_generate_text_tree(TypeDependencyGraph* graph) {
    if (!graph) {
        return NULL;
    }
    
    size_t buffer_size = 8192;
    char* buffer = (char*)malloc(buffer_size);
    if (!buffer) {
        return NULL;
    }
    
    int pos = 0;
    
    pos += snprintf(buffer + pos, buffer_size - pos, "Type Dependency Graph\n");
    pos += snprintf(buffer + pos, buffer_size - pos, "====================\n\n");
    
    for (size_t i = 0; i < graph->node_count; i++) {
        TypeDependencyNode* node = &graph->nodes[i];
        
        pos += snprintf(buffer + pos, buffer_size - pos, "%s (%s)\n", node->type_name, node->type_kind);
        
        if (node->dependency_count > 0) {
            for (size_t j = 0; j < node->dependency_count; j++) {
                TypeReference* ref = &node->dependencies[j];
                // 检查目标节点是否存在
                bool target_exists = false;
                for (size_t k = 0; k < graph->node_count; k++) {
                    if (strcmp(graph->nodes[k].type_name, ref->type_name) == 0) {
                        target_exists = true;
                        break;
                    }
                }
                
                if (target_exists) {
                    pos += snprintf(buffer + pos, buffer_size - pos, "  └─> %s\n", ref->type_name);
                }
            }
        } else {
            pos += snprintf(buffer + pos, buffer_size - pos, "  (no dependencies)\n");
        }
        
        pos += snprintf(buffer + pos, buffer_size - pos, "\n");
    }
    
    return buffer;
}

void kos_visualization_print_text_tree(TypeDependencyGraph* graph) {
    char* text = kos_visualization_generate_text_tree(graph);
    if (text) {
        printf("%s", text);
        free(text);
    }
}

// ========== JSON 依存关系输出 ==========

char* kos_visualization_generate_json(TypeDependencyGraph* graph) {
    if (!graph) {
        return NULL;
    }
    
    size_t buffer_size = 8192;
    char* buffer = (char*)malloc(buffer_size);
    if (!buffer) {
        return NULL;
    }
    
    int pos = 0;
    
    pos += snprintf(buffer + pos, buffer_size - pos, "{\n");
    pos += snprintf(buffer + pos, buffer_size - pos, "  \"nodes\": [\n");
    
    for (size_t i = 0; i < graph->node_count; i++) {
        TypeDependencyNode* node = &graph->nodes[i];
        
        pos += snprintf(buffer + pos, buffer_size - pos, "    {\n");
        pos += snprintf(buffer + pos, buffer_size - pos, "      \"name\": \"%s\",\n", node->type_name);
        pos += snprintf(buffer + pos, buffer_size - pos, "      \"kind\": \"%s\",\n", node->type_kind);
        pos += snprintf(buffer + pos, buffer_size - pos, "      \"dependencies\": [\n");
        
        for (size_t j = 0; j < node->dependency_count; j++) {
            TypeReference* ref = &node->dependencies[j];
            bool target_exists = false;
            for (size_t k = 0; k < graph->node_count; k++) {
                if (strcmp(graph->nodes[k].type_name, ref->type_name) == 0) {
                    target_exists = true;
                    break;
                }
            }
            
            if (target_exists) {
                pos += snprintf(buffer + pos, buffer_size - pos, "        \"%s\"", ref->type_name);
                if (j < node->dependency_count - 1) {
                    pos += snprintf(buffer + pos, buffer_size - pos, ",");
                }
                pos += snprintf(buffer + pos, buffer_size - pos, "\n");
            }
        }
        
        pos += snprintf(buffer + pos, buffer_size - pos, "      ]\n");
        pos += snprintf(buffer + pos, buffer_size - pos, "    }");
        if (i < graph->node_count - 1) {
            pos += snprintf(buffer + pos, buffer_size - pos, ",");
        }
        pos += snprintf(buffer + pos, buffer_size - pos, "\n");
    }
    
    pos += snprintf(buffer + pos, buffer_size - pos, "  ]\n");
    pos += snprintf(buffer + pos, buffer_size - pos, "}\n");
    
    return buffer;
}

int kos_visualization_save_json(TypeDependencyGraph* graph, const char* filename) {
    if (!graph || !filename) {
        return -1;
    }
    
    char* json_content = kos_visualization_generate_json(graph);
    if (!json_content) {
        return -1;
    }
    
    FILE* file = fopen(filename, "w");
    if (!file) {
        free(json_content);
        return -1;
    }
    
    fprintf(file, "%s", json_content);
    fclose(file);
    free(json_content);
    
    return 0;
}

// ========== 便捷函数 ==========

int kos_visualization_from_json_file(const char* input_json_file, 
                                     const char* output_file_prefix,
                                     const char* format) {
    if (!input_json_file || !output_file_prefix) {
        return -1;
    }
    
    // 加载本体
    TypeOntology* ontology = kos_ontology_load_from_file(input_json_file);
    if (!ontology) {
        return -1;
    }
    
    // 提取依存关系
    TypeDependencyGraph* graph = kos_visualization_extract_dependencies(ontology);
    if (!graph) {
        kos_ontology_free(ontology);
        return -1;
    }
    
    int result = 0;
    
    // 根据格式生成输出
    if (!format || strcmp(format, "all") == 0 || strcmp(format, "dot") == 0) {
        char dot_filename[256];
        snprintf(dot_filename, sizeof(dot_filename), "%s.dot", output_file_prefix);
        if (kos_visualization_save_dot(graph, dot_filename) != 0) {
            result = -1;
        }
    }
    
    if (!format || strcmp(format, "all") == 0 || strcmp(format, "html") == 0) {
        char html_filename[256];
        snprintf(html_filename, sizeof(html_filename), "%s.html", output_file_prefix);
        if (kos_visualization_save_html(graph, html_filename, "Type Dependency Graph") != 0) {
            result = -1;
        }
    }
    
    if (!format || strcmp(format, "all") == 0 || strcmp(format, "text") == 0) {
        char text_filename[256];
        snprintf(text_filename, sizeof(text_filename), "%s.txt", output_file_prefix);
        char* text = kos_visualization_generate_text_tree(graph);
        if (text) {
            FILE* file = fopen(text_filename, "w");
            if (file) {
                fprintf(file, "%s", text);
                fclose(file);
            }
            free(text);
        }
    }
    
    if (!format || strcmp(format, "all") == 0 || strcmp(format, "json") == 0) {
        char json_filename[256];
        snprintf(json_filename, sizeof(json_filename), "%s_dependencies.json", output_file_prefix);
        if (kos_visualization_save_json(graph, json_filename) != 0) {
            result = -1;
        }
    }
    
    kos_visualization_free_graph(graph);
    kos_ontology_free(ontology);
    
    return result;
}




























