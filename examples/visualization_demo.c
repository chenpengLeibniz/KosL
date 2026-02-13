// examples/visualization_demo.c
// Phase 8: Enhanced Visualization Demo
// 演示查询结果可视化和查询历史管理功能

#include "../include/kos_visualization_enhanced.h"
#include "../include/kos_query.h"
#include "../include/kos_core.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifdef _WIN32
#include <windows.h>
#define sleep_ms(ms) Sleep(ms)
#else
#include <unistd.h>
#define sleep_ms(ms) usleep((ms) * 1000)
#endif

// 创建示例查询结果
static kos_query_result_t* create_sample_query_result(void) {
    kos_query_result_t* result = (kos_query_result_t*)calloc(1, sizeof(kos_query_result_t));
    if (!result) {
        return NULL;
    }
    
    result->capacity = 10;
    result->count = 5;
    result->results = (kos_term**)calloc(5, sizeof(kos_term*));
    
    // 创建示例数据
    result->results[0] = kos_mk_atomic("Event1", NULL);
    result->results[1] = kos_mk_atomic("Event2", NULL);
    result->results[2] = kos_mk_atomic("Event3", NULL);
    result->results[3] = kos_mk_atomic("Event4", NULL);
    result->results[4] = kos_mk_atomic("Event5", NULL);
    
    return result;
}

// 演示查询结果表格可视化
static void demo_table_visualization(void) {
    printf("=== Table Visualization Demo ===\n\n");
    
    kos_query_result_t* result = create_sample_query_result();
    if (!result) {
        printf("Failed to create sample result\n");
        return;
    }
    
    kos_table_style_t style = kos_table_style_default();
    style.show_header = true;
    style.show_border = true;
    
    printf("1. Printing query result as table:\n");
    kos_visualization_print_table(result, &style);
    
    printf("\n2. Saving to CSV file: result.csv\n");
    kos_visualization_save_csv(result, "result.csv");
    printf("   CSV file saved successfully\n");
    
    kos_query_result_free(result);
    printf("\n");
}

// 演示图表可视化
static void demo_chart_visualization(void) {
    printf("=== Chart Visualization Demo ===\n\n");
    
    kos_query_result_t* result = create_sample_query_result();
    if (!result) {
        printf("Failed to create sample result\n");
        return;
    }
    
    kos_chart_config_t config = kos_chart_config_default(KOS_CHART_BAR);
    config.title = "Sample Query Results";
    config.width = 60;
    config.height = 20;
    
    printf("1. Printing ASCII bar chart:\n");
    kos_visualization_print_chart(result, &config, NULL, NULL);
    
    printf("\n2. Generating HTML chart: chart.html\n");
    kos_visualization_save_html_chart(result, &config, NULL, NULL, "chart.html");
    printf("   HTML chart saved successfully\n");
    
    kos_query_result_free(result);
    printf("\n");
}

// 演示关系图可视化
static void demo_graph_visualization(void) {
    printf("=== Relationship Graph Visualization Demo ===\n\n");
    
    kos_query_result_t* result = create_sample_query_result();
    if (!result) {
        printf("Failed to create sample result\n");
        return;
    }
    
    printf("1. Building relationship graph:\n");
    kos_relationship_graph_t* graph = kos_visualization_build_relationship_graph(
        result, "from", "to", NULL);
    
    if (graph) {
        printf("   Graph created with %zu nodes and %zu edges\n",
               graph->node_count, graph->edge_count);
        
        printf("\n2. Saving graph as DOT file: graph.dot\n");
        kos_visualization_save_graph_dot(graph, "graph.dot");
        printf("   DOT file saved successfully\n");
        
        printf("\n3. Saving graph as HTML file: graph.html\n");
        kos_visualization_save_graph_html(graph, "graph.html");
        printf("   HTML file saved successfully\n");
        
        kos_visualization_free_relationship_graph(graph);
    }
    
    kos_query_result_free(result);
    printf("\n");
}

// 演示查询历史管理
static void demo_query_history(void) {
    printf("=== Query History Management Demo ===\n\n");
    
    // 创建查询历史管理器
    kos_query_history_t* history = kos_query_history_create(100);
    if (!history) {
        printf("Failed to create query history\n");
        return;
    }
    
    // 创建示例查询
    kos_query_t* query1 = kos_query_create("Event");
    kos_query_set_limit(query1, 10);
    
    kos_query_result_t* result1 = create_sample_query_result();
    
    // 添加查询历史
    printf("1. Adding query history entries:\n");
    kos_query_history_add(history, "query_001", "SELECT * FROM Event LIMIT 10",
                          query1, result1, 15.5, false);
    printf("   Added query_001\n");
    
    kos_query_t* query2 = kos_query_create("Event");
    kos_query_set_order_by(query2, "timestamp", false);
    kos_query_result_t* result2 = create_sample_query_result();
    
    kos_query_history_add(history, "query_002", "SELECT * FROM Event ORDER BY timestamp",
                          query2, result2, 8.2, true);
    printf("   Added query_002 (cached)\n");
    
    printf("\n2. Total history entries: %zu\n", history->count);
    
    // 查找查询历史
    printf("\n3. Finding query by ID:\n");
    kos_query_history_entry_t* entry = kos_query_history_find_by_id(history, "query_001");
    if (entry) {
        printf("   Found: %s\n", entry->query_string ? entry->query_string : "");
        printf("   Execution time: %.2f ms\n", entry->execution_time_ms);
        printf("   Result count: %zu\n", entry->result_count);
    }
    
    // 获取最近查询
    printf("\n4. Recent queries:\n");
    size_t recent_count = 0;
    kos_query_history_entry_t* recent = kos_query_history_get_recent(history, 2, &recent_count);
    if (recent) {
        for (size_t i = 0; i < recent_count; i++) {
            printf("   - %s (%.2f ms)\n",
                   recent[i].query_id ? recent[i].query_id : "",
                   recent[i].execution_time_ms);
        }
    }
    
    // 导出为JSON
    printf("\n5. Exporting history to JSON:\n");
    char* json = kos_query_history_export_json(history);
    if (json) {
        printf("   JSON length: %zu bytes\n", strlen(json));
        free(json);
    }
    
    // 保存到文件
    printf("\n6. Saving history to file: query_history.json\n");
    kos_query_history_save(history, "query_history.json");
    printf("   History file saved successfully\n");
    
    kos_query_free(query1);
    kos_query_free(query2);
    kos_query_result_free(result1);
    kos_query_result_free(result2);
    kos_query_history_free(history);
    printf("\n");
}

// 演示可视化仪表板
static void demo_dashboard(void) {
    printf("=== Dashboard Visualization Demo ===\n\n");
    
    kos_query_result_t* result = create_sample_query_result();
    if (!result) {
        printf("Failed to create sample result\n");
        return;
    }
    
    printf("1. Generating dashboard HTML: dashboard.html\n");
    int ret = kos_visualization_save_dashboard_html(
        result, "dashboard.html", "Query Results Dashboard",
        true, true, true);
    
    if (ret == 0) {
        printf("   Dashboard saved successfully\n");
        printf("   Open dashboard.html in a web browser to view\n");
    } else {
        printf("   Failed to save dashboard\n");
    }
    
    kos_query_result_free(result);
    printf("\n");
}

int main(void) {
    printf("========================================\n");
    printf("KOS-TL Enhanced Visualization Demo\n");
    printf("Phase 8: Query Result Visualization & Query History\n");
    printf("========================================\n\n");
    
    demo_table_visualization();
    demo_chart_visualization();
    demo_graph_visualization();
    demo_query_history();
    demo_dashboard();
    
    printf("========================================\n");
    printf("Demo completed successfully!\n");
    printf("Generated files:\n");
    printf("  - result.csv (CSV table)\n");
    printf("  - chart.html (HTML chart)\n");
    printf("  - graph.dot (GraphViz DOT)\n");
    printf("  - graph.html (HTML graph)\n");
    printf("  - query_history.json (Query history)\n");
    printf("  - dashboard.html (Complete dashboard)\n");
    printf("========================================\n");
    
    return 0;
}
