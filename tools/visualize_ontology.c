// tools/visualize_ontology.c
// 类型依存关系可视化工具
// 用法: visualize_ontology <input_json_file> [output_prefix] [format]
// format: dot, html, text, json, all (默认: all)

#include "../include/kos_visualization.h"
#include "../include/kos_ontology.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void print_usage(const char* program_name) {
    printf("Usage: %s <input_json_file> [output_prefix] [format]\n", program_name);
    printf("\n");
    printf("Arguments:\n");
    printf("  input_json_file  : Path to the ontology JSON file\n");
    printf("  output_prefix    : Output file prefix (default: output)\n");
    printf("  format           : Output format: dot, html, text, json, all (default: all)\n");
    printf("\n");
    printf("Examples:\n");
    printf("  %s manufacturing_ontology.json\n", program_name);
    printf("  %s manufacturing_ontology.json output dot\n", program_name);
    printf("  %s manufacturing_ontology.json output html\n", program_name);
}

int main(int argc, char* argv[]) {
    if (argc < 2) {
        print_usage(argv[0]);
        return 1;
    }
    
    const char* input_file = argv[1];
    const char* output_prefix = (argc >= 3) ? argv[2] : "output";
    const char* format = (argc >= 4) ? argv[3] : "all";
    
    printf("=== KOS Type Dependency Visualization Tool ===\n\n");
    printf("Input file: %s\n", input_file);
    printf("Output prefix: %s\n", output_prefix);
    printf("Format: %s\n\n", format);
    
    // 从JSON文件生成可视化
    int result = kos_visualization_from_json_file(input_file, output_prefix, format);
    
    if (result == 0) {
        printf("✓ Visualization generated successfully!\n\n");
        
        if (strcmp(format, "all") == 0 || strcmp(format, "dot") == 0) {
            printf("  - GraphViz DOT: %s.dot\n", output_prefix);
            printf("    (Use 'dot -Tpng %s.dot -o %s.png' to generate PNG)\n", output_prefix, output_prefix);
        }
        if (strcmp(format, "all") == 0 || strcmp(format, "html") == 0) {
            printf("  - HTML (Mermaid): %s.html\n", output_prefix);
            printf("    (Open in web browser to view)\n");
        }
        if (strcmp(format, "all") == 0 || strcmp(format, "text") == 0) {
            printf("  - Text tree: %s.txt\n", output_prefix);
        }
        if (strcmp(format, "all") == 0 || strcmp(format, "json") == 0) {
            printf("  - JSON dependencies: %s_dependencies.json\n", output_prefix);
        }
        
        return 0;
    } else {
        printf("✗ Error: Failed to generate visualization\n");
        return 1;
    }
}













