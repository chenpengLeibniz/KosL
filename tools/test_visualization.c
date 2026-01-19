// tools/test_visualization.c
// 测试可视化功能（从程序初始化本体，而不是从JSON加载）

#include "../include/kos_visualization.h"
#include "../include/kos_ontology.h"
#include "../include/kos_core.h"
#include <stdio.h>
#include <stdlib.h>

// 声明制造业本体初始化函数
extern TypeOntology* kos_manufacturing_ontology_init(void);

int main() {
    printf("=== Testing Type Dependency Visualization ===\n\n");
    
    // 初始化制造业本体
    printf("1. Initializing ontology...\n");
    TypeOntology* ontology = kos_manufacturing_ontology_init();
    if (!ontology) {
        printf("ERROR: Failed to initialize ontology\n");
        return 1;
    }
    printf("   ✓ Ontology initialized (%zu types)\n\n", ontology->type_count);
    
    // 提取依存关系
    printf("2. Extracting type dependencies...\n");
    TypeDependencyGraph* graph = kos_visualization_extract_dependencies(ontology);
    if (!graph) {
        printf("ERROR: Failed to extract dependencies\n");
        kos_ontology_free(ontology);
        return 1;
    }
    printf("   ✓ Dependencies extracted (%zu nodes)\n\n", graph->node_count);
    
    // 打印文本树
    printf("3. Text tree visualization:\n");
    printf("   =========================\n");
    kos_visualization_print_text_tree(graph);
    printf("\n");
    
    // 生成各种格式的可视化
    printf("4. Generating visualization files...\n");
    
    // DOT格式
    if (kos_visualization_save_dot(graph, "output.dot") == 0) {
        printf("   ✓ GraphViz DOT: output.dot\n");
    } else {
        printf("   ✗ Failed to generate DOT\n");
    }
    
    // HTML格式
    if (kos_visualization_save_html(graph, "output.html", "Manufacturing Type Dependency Graph") == 0) {
        printf("   ✓ HTML (Mermaid): output.html\n");
    } else {
        printf("   ✗ Failed to generate HTML\n");
    }
    
    // JSON格式
    if (kos_visualization_save_json(graph, "output_dependencies.json") == 0) {
        printf("   ✓ JSON dependencies: output_dependencies.json\n");
    } else {
        printf("   ✗ Failed to generate JSON\n");
    }
    
    // 清理
    kos_visualization_free_graph(graph);
    kos_ontology_free(ontology);
    
    printf("\n=== Test Complete ===\n");
    return 0;
}




























