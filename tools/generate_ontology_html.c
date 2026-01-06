// tools/generate_ontology_html.c
// 从JSON文件加载本体并生成HTML可视化文件

#include "../../include/kos_ontology.h"
#include "../../include/kos_visualization.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int main(int argc, char* argv[]) {
    const char* json_file = "manufacturing_ontology.json";
    const char* output_html = "ontology_visualization.html";
    
    // 允许通过命令行参数指定文件
    if (argc >= 2) {
        json_file = argv[1];
    }
    if (argc >= 3) {
        output_html = argv[2];
    }
    
    printf("=== Ontology HTML Visualization Generator ===\n\n");
    printf("Loading ontology from: %s\n", json_file);
    
    // 从文件加载本体
    TypeOntology* ontology = kos_ontology_load_from_file(json_file);
    if (!ontology) {
        printf("ERROR: Failed to load ontology from file: %s\n", json_file);
        printf("Note: kos_ontology_deserialize is not yet fully implemented.\n");
        printf("Please use test_visualization.exe which creates ontology programmatically.\n");
        return 1;
    }
    
    printf("Loaded ontology: %s\n", ontology->domain_name);
    printf("Type definitions: %zu\n\n", ontology->type_count);
    
    // 提取类型依赖关系
    printf("Extracting type dependencies...\n");
    TypeDependencyGraph* graph = kos_visualization_extract_dependencies(ontology);
    if (!graph) {
        printf("ERROR: Failed to extract dependencies\n");
        kos_ontology_free(ontology);
        return 1;
    }
    
    printf("Found %zu type references\n", graph->reference_count);
    printf("Found %zu dependencies\n\n", graph->dependency_count);
    
    // 生成HTML可视化
    printf("Generating HTML visualization: %s\n", output_html);
    int result = kos_visualization_generate_html(graph, output_html);
    if (result != 0) {
        printf("ERROR: Failed to generate HTML file\n");
        kos_visualization_free_graph(graph);
        kos_ontology_free(ontology);
        return 1;
    }
    
    printf("HTML visualization generated successfully!\n");
    printf("Open %s in a web browser to view the type dependency graph.\n", output_html);
    
    // 清理资源
    kos_visualization_free_graph(graph);
    kos_ontology_free(ontology);
    
    return 0;
}


