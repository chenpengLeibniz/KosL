// tools/generate_visualization_from_json.c
// 从JSON文件加载本体并生成可视化HTML文件

#include "../../include/kos_ontology.h"
#include "../../include/kos_visualization.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int main(int argc, char* argv[]) {
    const char* json_file = "manufacturing_ontology.json";
    const char* output_html = "manufacturing_ontology_visualization.html";
    
    if (argc > 1) {
        json_file = argv[1];
    }
    if (argc > 2) {
        output_html = argv[2];
    }
    
    printf("=== Ontology Visualization Generator ===\n\n");
    printf("Loading ontology from: %s\n", json_file);
    
    // 加载本体（注意：当前kos_ontology_deserialize是占位符，需要实现）
    // 为了演示，我们使用初始化函数来创建本体
    // 实际使用时需要实现完整的JSON反序列化
    
    // 临时方案：使用初始化函数（会从文件加载或创建默认本体）
    extern TypeOntology* kos_manufacturing_ontology_init(void);
    TypeOntology* ontology = kos_manufacturing_ontology_init();
    
    if (!ontology) {
        fprintf(stderr, "Error: Failed to load ontology\n");
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
    
    printf("Found %zu type references\n", graph->reference_count);
    printf("Found %zu dependencies\n\n", graph->dependency_count);
    
    // 生成HTML可视化文件
    printf("Generating HTML visualization: %s\n", output_html);
    int result = kos_visualization_generate_html(graph, output_html, "Manufacturing Ontology Type Dependencies");
    
    if (result != 0) {
        fprintf(stderr, "Error: Failed to generate HTML file\n");
        kos_visualization_free_graph(graph);
        kos_ontology_free(ontology);
        return 1;
    }
    
    printf("HTML visualization generated successfully!\n\n");
    
    // 同时生成其他格式
    printf("Generating additional formats...\n");
    
    // DOT格式
    char dot_file[256];
    snprintf(dot_file, sizeof(dot_file), "%s.dot", output_html);
    kos_visualization_generate_dot(graph, dot_file);
    printf("  - DOT file: %s\n", dot_file);
    
    // JSON格式
    char json_output[256];
    snprintf(json_output, sizeof(json_output), "%s_dependencies.json", output_html);
    kos_visualization_generate_json(graph, json_output);
    printf("  - JSON file: %s\n", json_output);
    
    // 文本树格式
    char text_file[256];
    snprintf(text_file, sizeof(text_file), "%s_tree.txt", output_html);
    kos_visualization_generate_text_tree(graph, text_file);
    printf("  - Text tree: %s\n", text_file);
    
    printf("\n=== Visualization Generation Complete ===\n");
    
    // 清理资源
    kos_visualization_free_graph(graph);
    kos_ontology_free(ontology);
    
    return 0;
}
