// tools/generate_html_visualization.c
// 从本体JSON文件生成HTML可视化
// 使用制造业本体初始化函数（会自动从文件加载）

#include "../include/kos_visualization.h"
#include "../include/kos_ontology.h"
#include "../include/kos_manufacturing.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// 声明制造业本体初始化函数
extern TypeOntology* kos_manufacturing_ontology_init(void);

int main(int argc, char* argv[]) {
    const char* output_html = "manufacturing_ontology_visualization.html";
    const char* output_dot = "manufacturing_ontology_dependencies.dot";
    const char* output_json = "manufacturing_ontology_dependencies.json";
    const char* output_text = "manufacturing_ontology_tree.txt";
    
    // 解析命令行参数
    if (argc > 1) {
        output_html = argv[1];
    }
    
    printf("=== Manufacturing Ontology HTML Visualization Generator ===\n\n");
    printf("Initializing ontology (will load from file if exists)...\n");
    
    // 使用制造业本体初始化函数，它会自动从文件加载（如果存在）
    TypeOntology* ontology = kos_manufacturing_ontology_init();
    
    if (!ontology) {
        fprintf(stderr, "Error: Failed to initialize ontology\n");
        return 1;
    }
    
    printf("Loaded ontology: %s\n", ontology->domain_name);
    printf("Type definitions: %zu\n\n", ontology->type_count);
    
    // 提取类型依赖关系
    printf("Extracting type dependencies...\n");
    TypeDependencyGraph* graph = kos_visualization_extract_dependencies(ontology);
    if (!graph) {
        fprintf(stderr, "Error: Failed to extract dependencies\n");
        kos_ontology_free(ontology);
        return 1;
    }
    
    printf("Found %zu type nodes\n\n", graph->node_count);
    
    // 生成可视化文件
    printf("Generating visualization files...\n");
    
    // 1. 生成HTML文件（主要输出）
    printf("  - Generating HTML file: %s\n", output_html);
    if (kos_visualization_save_html(graph, output_html, 
                                    "Manufacturing Ontology Type Dependencies") != 0) {
        fprintf(stderr, "Error: Failed to generate HTML file\n");
        kos_visualization_free_graph(graph);
        kos_ontology_free(ontology);
        return 1;
    }
    
    // 2. 生成GraphViz DOT文件
    printf("  - Generating DOT file: %s\n", output_dot);
    if (kos_visualization_save_dot(graph, output_dot) != 0) {
        fprintf(stderr, "Warning: Failed to generate DOT file\n");
    }
    
    // 3. 生成JSON依赖文件
    printf("  - Generating JSON file: %s\n", output_json);
    if (kos_visualization_save_json(graph, output_json) != 0) {
        fprintf(stderr, "Warning: Failed to generate JSON file\n");
    }
    
    // 4. 生成文本树
    printf("  - Generating text tree: %s\n", output_text);
    char* text_tree = kos_visualization_generate_text_tree(graph);
    if (text_tree) {
        FILE* file = fopen(output_text, "w");
        if (file) {
            fprintf(file, "%s", text_tree);
            fclose(file);
        }
        free(text_tree);
    } else {
        fprintf(stderr, "Warning: Failed to generate text tree\n");
    }
    
    printf("\n=== Visualization Generation Complete ===\n");
    printf("Files generated:\n");
    printf("  - %s (HTML with Mermaid.js - OPEN THIS IN BROWSER)\n", output_html);
    printf("  - %s (GraphViz DOT format)\n", output_dot);
    printf("  - %s (Dependency data in JSON)\n", output_json);
    printf("  - %s (Text tree format)\n", output_text);
    printf("\nOpen %s in a web browser to view the interactive visualization.\n", output_html);
    
    // 清理资源
    kos_visualization_free_graph(graph);
    kos_ontology_free(ontology);
    
    return 0;
}


