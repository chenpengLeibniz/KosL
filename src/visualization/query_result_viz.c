// src/visualization/query_result_viz.c
// Phase 8: Query Result Visualization
// 查询结果可视化：表格、图表、关系图

#include "../../include/kos_visualization_enhanced.h"
#include "../../include/kos_core.h"
#include "../../include/kos_query.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <math.h>

#define INITIAL_BUFFER_SIZE 4096
#define MAX_FIELD_NAME_LEN 256

// 扩展字符串缓冲区
static char* append_string(char* buffer, size_t* capacity, size_t* pos, const char* str) {
    if (!str) return buffer;
    
    size_t len = strlen(str);
    while (*pos + len + 1 >= *capacity) {
        *capacity *= 2;
        buffer = (char*)realloc(buffer, *capacity);
        if (!buffer) return NULL;
    }
    
    strcpy(buffer + *pos, str);
    *pos += len;
    return buffer;
}

// 从kos_term提取字符串值
static char* term_to_string(kos_term* term) {
    if (!term) {
        char* str = (char*)malloc(5);
        if (str) strcpy(str, "NULL");
        return str;
    }
    
    switch (term->kind) {
        case KOS_VAL:
        case KOS_ID:
        case KOS_TIME:
            if (term->data.atomic.val) {
                size_t len = strlen(term->data.atomic.val);
                char* str = (char*)malloc(len + 1);
                if (str) strcpy(str, term->data.atomic.val);
                return str;
            }
            break;
        default:
            break;
    }
    
    char* str = (char*)malloc(32);
    if (str) snprintf(str, 32, "<%d>", term->kind);
    return str;
}

// 创建默认表格样式
kos_table_style_t kos_table_style_default(void) {
    kos_table_style_t style = {0};
    style.show_header = true;
    style.show_border = true;
    style.max_column_width = 30;
    style.separator = "|";
    return style;
}

// 将查询结果格式化为表格字符串
char* kos_visualization_query_result_to_table(
    kos_query_result_t* result,
    const kos_table_style_t* style) {
    
    if (!result || !style) {
        return NULL;
    }
    
    size_t capacity = INITIAL_BUFFER_SIZE;
    size_t pos = 0;
    char* buffer = (char*)malloc(capacity);
    if (!buffer) return NULL;
    buffer[0] = '\0';
    
    // 简化实现：假设所有结果都是相同类型的Σ类型
    if (result->count == 0) {
        append_string(buffer, &capacity, &pos, "No results\n");
        return buffer;
    }
    
    // 提取字段名（简化：假设第一个结果包含所有字段）
    kos_term* first_result = result->results[0];
    if (!first_result || first_result->kind != KOS_SIGMA) {
        append_string(buffer, &capacity, &pos, "Unsupported result type\n");
        return buffer;
    }
    
    // 生成表头（简化实现）
    if (style->show_header) {
        append_string(buffer, &capacity, &pos, "Result Table\n");
        if (style->show_border) {
            append_string(buffer, &capacity, &pos, "----------------------------------------\n");
        }
    }
    
    // 生成数据行
    for (size_t i = 0; i < result->count && i < 100; i++) { // 限制显示前100行
        char row[512] = {0};
        snprintf(row, sizeof(row), "Row %zu: ", i + 1);
        append_string(buffer, &capacity, &pos, row);
        
        char* val_str = term_to_string(result->results[i]);
        append_string(buffer, &capacity, &pos, val_str);
        append_string(buffer, &capacity, &pos, "\n");
        free(val_str);
    }
    
    if (result->count > 100) {
        char footer[64];
        snprintf(footer, sizeof(footer), "... (%zu more rows)\n", result->count - 100);
        append_string(buffer, &capacity, &pos, footer);
    }
    
    return buffer;
}

// 打印查询结果为表格
void kos_visualization_print_table(
    kos_query_result_t* result,
    const kos_table_style_t* style) {
    
    char* table_str = kos_visualization_query_result_to_table(result, style);
    if (table_str) {
        printf("%s", table_str);
        free(table_str);
    }
}

// 将查询结果保存为CSV文件
int kos_visualization_save_csv(
    kos_query_result_t* result,
    const char* filename) {
    
    if (!result || !filename) {
        return -1;
    }
    
    FILE* fp = fopen(filename, "w");
    if (!fp) {
        return -1;
    }
    
    // 写入CSV头（简化）
    fprintf(fp, "Index,Value\n");
    
    // 写入数据
    for (size_t i = 0; i < result->count; i++) {
        char* val_str = term_to_string(result->results[i]);
        fprintf(fp, "%zu,%s\n", i, val_str);
        free(val_str);
    }
    
    fclose(fp);
    return 0;
}

// 创建默认图表配置
kos_chart_config_t kos_chart_config_default(kos_chart_type_t type) {
    kos_chart_config_t config = {0};
    config.type = type;
    config.title = "Query Result Chart";
    config.x_label = "Category";
    config.y_label = "Value";
    config.width = 60;
    config.height = 20;
    config.show_legend = true;
    return config;
}

// 生成ASCII图表字符串
char* kos_visualization_generate_ascii_chart(
    kos_query_result_t* result,
    const kos_chart_config_t* config,
    const char* value_field,
    const char* category_field) {
    
    if (!result || !config) {
        return NULL;
    }
    
    size_t capacity = INITIAL_BUFFER_SIZE;
    size_t pos = 0;
    char* buffer = (char*)malloc(capacity);
    if (!buffer) return NULL;
    buffer[0] = '\0';
    
    // 添加标题
    char title_line[256];
    snprintf(title_line, sizeof(title_line), "%s\n", config->title);
    append_string(buffer, &capacity, &pos, title_line);
    
    // 简化实现：生成基本的ASCII柱状图
    if (result->count == 0) {
        append_string(buffer, &capacity, &pos, "No data to display\n");
        return buffer;
    }
    
    // 计算最大值（用于缩放）
    double max_val = 1.0;
    for (size_t i = 0; i < result->count && i < 20; i++) {
        // 简化：假设值是数字
        max_val = fmax(max_val, 1.0);
    }
    
    // 生成柱状图
    append_string(buffer, &capacity, &pos, "\n");
    for (size_t i = 0; i < result->count && i < 10; i++) {
        char bar[128];
        int bar_length = (int)(20.0 * (1.0 / max_val)); // 简化：固定长度
        snprintf(bar, sizeof(bar), "Item %zu: ", i + 1);
        append_string(buffer, &capacity, &pos, bar);
        
        for (int j = 0; j < bar_length; j++) {
            append_string(buffer, &capacity, &pos, "█");
        }
        append_string(buffer, &capacity, &pos, "\n");
    }
    
    return buffer;
}

// 打印ASCII图表
void kos_visualization_print_chart(
    kos_query_result_t* result,
    const kos_chart_config_t* config,
    const char* value_field,
    const char* category_field) {
    
    char* chart_str = kos_visualization_generate_ascii_chart(result, config, value_field, category_field);
    if (chart_str) {
        printf("%s", chart_str);
        free(chart_str);
    }
}

// 生成HTML图表
char* kos_visualization_generate_html_chart(
    kos_query_result_t* result,
    const kos_chart_config_t* config,
    const char* value_field,
    const char* category_field) {
    
    if (!result || !config) {
        return NULL;
    }
    
    size_t capacity = INITIAL_BUFFER_SIZE * 2;
    size_t pos = 0;
    char* buffer = (char*)malloc(capacity);
    if (!buffer) return NULL;
    buffer[0] = '\0';
    
    // 生成HTML模板（使用Chart.js）
    const char* html_template = 
        "<!DOCTYPE html>\n"
        "<html><head>\n"
        "<title>%s</title>\n"
        "<script src=\"https://cdn.jsdelivr.net/npm/chart.js\"></script>\n"
        "</head><body>\n"
        "<h1>%s</h1>\n"
        "<canvas id=\"chart\"></canvas>\n"
        "<script>\n"
        "const ctx = document.getElementById('chart').getContext('2d');\n"
        "const chart = new Chart(ctx, {\n"
        "  type: '%s',\n"
        "  data: { labels: [], datasets: [{ label: '%s', data: [] }] },\n"
        "  options: { responsive: true }\n"
        "});\n"
        "</script>\n"
        "</body></html>\n";
    
    const char* chart_type_str = "bar";
    switch (config->type) {
        case KOS_CHART_BAR: chart_type_str = "bar"; break;
        case KOS_CHART_LINE: chart_type_str = "line"; break;
        case KOS_CHART_PIE: chart_type_str = "pie"; break;
        case KOS_CHART_SCATTER: chart_type_str = "scatter"; break;
    }
    
    char html[4096];
    snprintf(html, sizeof(html), html_template,
             config->title, config->title, chart_type_str, config->y_label);
    
    append_string(buffer, &capacity, &pos, html);
    
    return buffer;
}

// 保存HTML图表文件
int kos_visualization_save_html_chart(
    kos_query_result_t* result,
    const kos_chart_config_t* config,
    const char* value_field,
    const char* category_field,
    const char* filename) {
    
    char* html = kos_visualization_generate_html_chart(result, config, value_field, category_field);
    if (!html) {
        return -1;
    }
    
    FILE* fp = fopen(filename, "w");
    if (!fp) {
        free(html);
        return -1;
    }
    
    fprintf(fp, "%s", html);
    fclose(fp);
    free(html);
    
    return 0;
}

// 从查询结果构建关系图
kos_relationship_graph_t* kos_visualization_build_relationship_graph(
    kos_query_result_t* result,
    const char* from_field,
    const char* to_field,
    const char* relation_field) {
    
    if (!result || !from_field || !to_field) {
        return NULL;
    }
    
    kos_relationship_graph_t* graph = (kos_relationship_graph_t*)calloc(1, sizeof(kos_relationship_graph_t));
    if (!graph) {
        return NULL;
    }
    
    graph->title = "Relationship Graph";
    graph->nodes = (kos_graph_node_t*)calloc(result->count * 2, sizeof(kos_graph_node_t));
    graph->edges = (kos_graph_edge_t*)calloc(result->count, sizeof(kos_graph_edge_t));
    
    if (!graph->nodes || !graph->edges) {
        kos_visualization_free_relationship_graph(graph);
        return NULL;
    }
    
    // 简化实现：为每个结果创建节点和边
    for (size_t i = 0; i < result->count; i++) {
        kos_term* item = result->results[i];
        if (item && item->kind == KOS_SIGMA) {
            // 创建节点
            graph->nodes[graph->node_count].data = item;
            graph->nodes[graph->node_count].label = "Node";
            graph->nodes[graph->node_count].x = (float)(i % 10) / 10.0f;
            graph->nodes[graph->node_count].y = (float)(i / 10) / 10.0f;
            graph->node_count++;
            
            // 创建边（简化：连接到下一个节点）
            if (i + 1 < result->count) {
                graph->edges[graph->edge_count].from = &graph->nodes[i];
                graph->edges[graph->edge_count].to = &graph->nodes[i + 1];
                graph->edges[graph->edge_count].label = "relation";
                graph->edge_count++;
            }
        }
    }
    
    return graph;
}

// 生成GraphViz DOT格式的关系图
char* kos_visualization_graph_to_dot(kos_relationship_graph_t* graph) {
    if (!graph) {
        return NULL;
    }
    
    size_t capacity = INITIAL_BUFFER_SIZE;
    size_t pos = 0;
    char* buffer = (char*)malloc(capacity);
    if (!buffer) return NULL;
    buffer[0] = '\0';
    
    append_string(buffer, &capacity, &pos, "digraph G {\n");
    append_string(buffer, &capacity, &pos, "  label=\"");
    append_string(buffer, &capacity, &pos, graph->title ? graph->title : "Graph");
    append_string(buffer, &capacity, &pos, "\";\n");
    
    // 添加节点
    for (size_t i = 0; i < graph->node_count; i++) {
        char node_line[256];
        snprintf(node_line, sizeof(node_line), "  n%zu [label=\"%s\"];\n",
                 i, graph->nodes[i].label ? graph->nodes[i].label : "Node");
        append_string(buffer, &capacity, &pos, node_line);
    }
    
    // 添加边
    for (size_t i = 0; i < graph->edge_count; i++) {
        char edge_line[256];
        size_t from_idx = graph->edges[i].from - graph->nodes;
        size_t to_idx = graph->edges[i].to - graph->nodes;
        snprintf(edge_line, sizeof(edge_line), "  n%zu -> n%zu",
                 from_idx, to_idx);
        if (graph->edges[i].label) {
            snprintf(edge_line + strlen(edge_line), sizeof(edge_line) - strlen(edge_line),
                    " [label=\"%s\"]", graph->edges[i].label);
        }
        append_string(buffer, &capacity, &pos, edge_line);
        append_string(buffer, &capacity, &pos, ";\n");
    }
    
    append_string(buffer, &capacity, &pos, "}\n");
    
    return buffer;
}

// 生成HTML关系图
char* kos_visualization_graph_to_html(kos_relationship_graph_t* graph) {
    if (!graph) {
        return NULL;
    }
    
    size_t capacity = INITIAL_BUFFER_SIZE * 2;
    size_t pos = 0;
    char* buffer = (char*)malloc(capacity);
    if (!buffer) return NULL;
    buffer[0] = '\0';
    
    // 生成HTML模板（使用vis.js）
    const char* html_template =
        "<!DOCTYPE html>\n"
        "<html><head>\n"
        "<title>%s</title>\n"
        "<script type=\"text/javascript\" src=\"https://unpkg.com/vis-network/standalone/umd/vis-network.min.js\"></script>\n"
        "<style>#network { width: 100%%; height: 600px; }</style>\n"
        "</head><body>\n"
        "<h1>%s</h1>\n"
        "<div id=\"network\"></div>\n"
        "<script>\n"
        "const nodes = new vis.DataSet([]);\n"
        "const edges = new vis.DataSet([]);\n"
        "const data = { nodes: nodes, edges: edges };\n"
        "const options = {};\n"
        "const network = new vis.Network(document.getElementById('network'), data, options);\n"
        "</script>\n"
        "</body></html>\n";
    
    char html[4096];
    snprintf(html, sizeof(html), html_template,
             graph->title ? graph->title : "Graph",
             graph->title ? graph->title : "Graph");
    
    append_string(buffer, &capacity, &pos, html);
    
    return buffer;
}

// 保存关系图为DOT文件
int kos_visualization_save_graph_dot(kos_relationship_graph_t* graph, const char* filename) {
    char* dot = kos_visualization_graph_to_dot(graph);
    if (!dot) {
        return -1;
    }
    
    FILE* fp = fopen(filename, "w");
    if (!fp) {
        free(dot);
        return -1;
    }
    
    fprintf(fp, "%s", dot);
    fclose(fp);
    free(dot);
    
    return 0;
}

// 保存关系图为HTML文件
int kos_visualization_save_graph_html(kos_relationship_graph_t* graph, const char* filename) {
    char* html = kos_visualization_graph_to_html(graph);
    if (!html) {
        return -1;
    }
    
    FILE* fp = fopen(filename, "w");
    if (!fp) {
        free(html);
        return -1;
    }
    
    fprintf(fp, "%s", html);
    fclose(fp);
    free(html);
    
    return 0;
}

// 释放关系图
void kos_visualization_free_relationship_graph(kos_relationship_graph_t* graph) {
    if (!graph) {
        return;
    }
    
    if (graph->nodes) {
        free(graph->nodes);
    }
    
    if (graph->edges) {
        free(graph->edges);
    }
    
    if (graph->title) {
        free((void*)graph->title);
    }
    
    free(graph);
}

// 查询结果转JSON
char* kos_visualization_query_result_to_json(kos_query_result_t* result) {
    if (!result) {
        return NULL;
    }
    
    size_t capacity = INITIAL_BUFFER_SIZE;
    size_t pos = 0;
    char* buffer = (char*)malloc(capacity);
    if (!buffer) return NULL;
    buffer[0] = '\0';
    
    append_string(buffer, &capacity, &pos, "{\"count\":");
    char count_str[32];
    snprintf(count_str, sizeof(count_str), "%zu", result->count);
    append_string(buffer, &capacity, &pos, count_str);
    append_string(buffer, &capacity, &pos, ",\"results\":[");
    
    for (size_t i = 0; i < result->count && i < 100; i++) {
        if (i > 0) {
            append_string(buffer, &capacity, &pos, ",");
        }
        char* val_str = term_to_string(result->results[i]);
        append_string(buffer, &capacity, &pos, "{\"value\":\"");
        append_string(buffer, &capacity, &pos, val_str);
        append_string(buffer, &capacity, &pos, "\"}");
        free(val_str);
    }
    
    append_string(buffer, &capacity, &pos, "]}");
    
    return buffer;
}

// 生成完整的可视化仪表板HTML
char* kos_visualization_generate_dashboard_html(
    kos_query_result_t* result,
    const char* title,
    bool include_table,
    bool include_chart,
    bool include_graph) {
    
    if (!result) {
        return NULL;
    }
    
    size_t capacity = INITIAL_BUFFER_SIZE * 4;
    size_t pos = 0;
    char* buffer = (char*)malloc(capacity);
    if (!buffer) return NULL;
    buffer[0] = '\0';
    
    const char* html_header =
        "<!DOCTYPE html>\n"
        "<html><head>\n"
        "<title>%s</title>\n"
        "<script src=\"https://cdn.jsdelivr.net/npm/chart.js\"></script>\n"
        "<style>body { font-family: Arial; margin: 20px; } .section { margin: 20px 0; }</style>\n"
        "</head><body>\n"
        "<h1>%s</h1>\n";
    
    char header[512];
    snprintf(header, sizeof(header), html_header,
             title ? title : "Query Dashboard",
             title ? title : "Query Dashboard");
    append_string(buffer, &capacity, &pos, header);
    
    if (include_table) {
        append_string(buffer, &capacity, &pos, "<div class=\"section\"><h2>Table</h2><pre>");
        char* table = kos_visualization_query_result_to_table(result, &(kos_table_style_t){.show_header=true, .show_border=true});
        if (table) {
            append_string(buffer, &capacity, &pos, table);
            free(table);
        }
        append_string(buffer, &capacity, &pos, "</pre></div>");
    }
    
    if (include_chart) {
        append_string(buffer, &capacity, &pos, "<div class=\"section\"><h2>Chart</h2>");
        kos_chart_config_t config = kos_chart_config_default(KOS_CHART_BAR);
        char* chart_html = kos_visualization_generate_html_chart(result, &config, NULL, NULL);
        if (chart_html) {
            append_string(buffer, &capacity, &pos, chart_html);
            free(chart_html);
        }
        append_string(buffer, &capacity, &pos, "</div>");
    }
    
    if (include_graph) {
        append_string(buffer, &capacity, &pos, "<div class=\"section\"><h2>Relationship Graph</h2>");
        kos_relationship_graph_t* graph = kos_visualization_build_relationship_graph(result, "from", "to", NULL);
        if (graph) {
            char* graph_html = kos_visualization_graph_to_html(graph);
            if (graph_html) {
                append_string(buffer, &capacity, &pos, graph_html);
                free(graph_html);
            }
            kos_visualization_free_relationship_graph(graph);
        }
        append_string(buffer, &capacity, &pos, "</div>");
    }
    
    append_string(buffer, &capacity, &pos, "</body></html>");
    
    return buffer;
}

// 保存可视化仪表板HTML
int kos_visualization_save_dashboard_html(
    kos_query_result_t* result,
    const char* filename,
    const char* title,
    bool include_table,
    bool include_chart,
    bool include_graph) {
    
    char* html = kos_visualization_generate_dashboard_html(result, title, include_table, include_chart, include_graph);
    if (!html) {
        return -1;
    }
    
    FILE* fp = fopen(filename, "w");
    if (!fp) {
        free(html);
        return -1;
    }
    
    fprintf(fp, "%s", html);
    fclose(fp);
    free(html);
    
    return 0;
}
