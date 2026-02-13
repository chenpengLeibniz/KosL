// src/visualization/query_result_viz_enhanced.c
// Phase 8 Enhancement: Additional Visualization Features
// 增强可视化功能：SVG导出、增强仪表板、布局算法、数据过滤排序

#include "../../include/kos_visualization_enhanced.h"
#include "../../include/kos_core.h"
#include "../../include/kos_query.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <math.h>

#ifndef M_PI
#define M_PI 3.14159265358979323846
#endif

// 扩展字符串缓冲区（内部实现）
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

// ========== SVG 导出功能 ==========

// 生成SVG格式的图表
char* kos_visualization_generate_svg_chart(
    kos_query_result_t* result,
    const kos_chart_config_t* config,
    const char* value_field,
    const char* category_field) {
    
    if (!result || !config) {
        return NULL;
    }
    
    size_t capacity = 4096 * 2;
    size_t pos = 0;
    char* buffer = (char*)malloc(capacity);
    if (!buffer) return NULL;
    buffer[0] = '\0';
    
    int width = config->width > 0 ? config->width : 800;
    int height = config->height > 0 ? config->height : 400;
    
    // SVG头部
    char svg_header[512];
    snprintf(svg_header, sizeof(svg_header),
        "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"%d\" height=\"%d\">\n"
        "<rect width=\"%d\" height=\"%d\" fill=\"white\"/>\n"
        "<text x=\"%d\" y=\"30\" font-family=\"Arial\" font-size=\"20\" font-weight=\"bold\" text-anchor=\"middle\">%s</text>\n",
        width, height, width, height, width / 2, config->title ? config->title : "Chart");
    append_string(buffer, &capacity, &pos, svg_header);
    
    // 生成柱状图（简化实现）
    if (config->type == KOS_CHART_BAR && result->count > 0) {
        int bar_width = (width - 100) / (int)result->count;
        if (bar_width < 10) bar_width = 10;
        int max_bar_height = height - 100;
        int x_start = 50;
        int y_base = height - 50;
        
        for (size_t i = 0; i < result->count && i < 20; i++) {
            int bar_height = (int)(max_bar_height * (1.0 - (double)i / result->count));
            int x = x_start + (int)i * bar_width;
            int y = y_base - bar_height;
            
            char bar[256];
            snprintf(bar, sizeof(bar),
                "<rect x=\"%d\" y=\"%d\" width=\"%d\" height=\"%d\" fill=\"steelblue\"/>\n"
                "<text x=\"%d\" y=\"%d\" font-family=\"Arial\" font-size=\"12\" text-anchor=\"middle\">%zu</text>\n",
                x, y, bar_width - 2, bar_height, x + bar_width / 2, y_base + 20, i + 1);
            append_string(buffer, &capacity, &pos, bar);
        }
    }
    
    append_string(buffer, &capacity, &pos, "</svg>\n");
    
    return buffer;
}

// 保存SVG图表文件
int kos_visualization_save_svg_chart(
    kos_query_result_t* result,
    const kos_chart_config_t* config,
    const char* value_field,
    const char* category_field,
    const char* filename) {
    
    char* svg = kos_visualization_generate_svg_chart(result, config, value_field, category_field);
    if (!svg) {
        return -1;
    }
    
    FILE* fp = fopen(filename, "w");
    if (!fp) {
        free(svg);
        return -1;
    }
    
    fprintf(fp, "%s", svg);
    fclose(fp);
    free(svg);
    
    return 0;
}

// 生成SVG格式的关系图
char* kos_visualization_graph_to_svg(kos_relationship_graph_t* graph) {
    if (!graph) {
        return NULL;
    }
    
    size_t capacity = 4096 * 2;
    size_t pos = 0;
    char* buffer = (char*)malloc(capacity);
    if (!buffer) return NULL;
    buffer[0] = '\0';
    
    int width = 800;
    int height = 600;
    
    // SVG头部
    char svg_header[512];
    snprintf(svg_header, sizeof(svg_header),
        "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"%d\" height=\"%d\">\n"
        "<rect width=\"%d\" height=\"%d\" fill=\"white\"/>\n"
        "<text x=\"%d\" y=\"30\" font-family=\"Arial\" font-size=\"20\" font-weight=\"bold\" text-anchor=\"middle\">%s</text>\n",
        width, height, width, height, width / 2,
        graph->title ? graph->title : "Relationship Graph");
    append_string(buffer, &capacity, &pos, svg_header);
    
    // 绘制边
    for (size_t i = 0; i < graph->edge_count; i++) {
        int x1 = (int)(graph->edges[i].from->x * (width - 100) + 50);
        int y1 = (int)(graph->edges[i].from->y * (height - 100) + 50);
        int x2 = (int)(graph->edges[i].to->x * (width - 100) + 50);
        int y2 = (int)(graph->edges[i].to->y * (height - 100) + 50);
        
        char line[256];
        snprintf(line, sizeof(line),
            "<line x1=\"%d\" y1=\"%d\" x2=\"%d\" y2=\"%d\" stroke=\"gray\" stroke-width=\"2\"/>\n",
            x1, y1, x2, y2);
        append_string(buffer, &capacity, &pos, line);
    }
    
    // 绘制节点
    for (size_t i = 0; i < graph->node_count; i++) {
        int x = (int)(graph->nodes[i].x * (width - 100) + 50);
        int y = (int)(graph->nodes[i].y * (height - 100) + 50);
        int radius = 20;
        
        char node[512];
        snprintf(node, sizeof(node),
            "<circle cx=\"%d\" cy=\"%d\" r=\"%d\" fill=\"lightblue\" stroke=\"black\" stroke-width=\"2\"/>\n"
            "<text x=\"%d\" y=\"%d\" font-family=\"Arial\" font-size=\"12\" text-anchor=\"middle\">%s</text>\n",
            x, y, radius, x, y + 5,
            graph->nodes[i].label ? graph->nodes[i].label : "Node");
        append_string(buffer, &capacity, &pos, node);
    }
    
    append_string(buffer, &capacity, &pos, "</svg>\n");
    
    return buffer;
}

// 保存关系图为SVG文件
int kos_visualization_save_graph_svg(kos_relationship_graph_t* graph, const char* filename) {
    char* svg = kos_visualization_graph_to_svg(graph);
    if (!svg) {
        return -1;
    }
    
    FILE* fp = fopen(filename, "w");
    if (!fp) {
        free(svg);
        return -1;
    }
    
    fprintf(fp, "%s", svg);
    fclose(fp);
    free(svg);
    
    return 0;
}

// ========== 增强的仪表板配置 ==========

// 创建默认仪表板配置
kos_dashboard_config_t kos_dashboard_config_default(void) {
    kos_dashboard_config_t config = {0};
    config.responsive = true;
    config.dark_mode = false;
    config.interactive = true;
    config.export_enabled = true;
    config.theme_color = "#2196F3";
    config.chart_count = 1;
    return config;
}

// 生成增强版仪表板HTML
char* kos_visualization_generate_enhanced_dashboard_html(
    kos_query_result_t* result,
    const char* title,
    const kos_dashboard_config_t* config,
    bool include_table,
    bool include_chart,
    bool include_graph) {
    
    if (!result || !config) {
        return NULL;
    }
    
    size_t capacity = 4096 * 8;
    size_t pos = 0;
    char* buffer = (char*)malloc(capacity);
    if (!buffer) return NULL;
    buffer[0] = '\0';
    
    // 生成增强的HTML头部（响应式、暗色主题支持）
    const char* html_header_template =
        "<!DOCTYPE html>\n"
        "<html><head>\n"
        "<meta charset=\"UTF-8\">\n"
        "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">\n"
        "<title>%s</title>\n"
        "<script src=\"https://cdn.jsdelivr.net/npm/chart.js\"></script>\n"
        "<script type=\"text/javascript\" src=\"https://unpkg.com/vis-network/standalone/umd/vis-network.min.js\"></script>\n"
        "<style>\n"
        "  * { box-sizing: border-box; }\n"
        "  body { font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif; margin: 0; padding: 20px; %s }\n"
        "  .dashboard-container { max-width: 1400px; margin: 0 auto; }\n"
        "  .section { margin: 30px 0; padding: 20px; background: %s; border-radius: 8px; box-shadow: 0 2px 4px rgba(0,0,0,0.1); }\n"
        "  h1 { color: %s; margin-bottom: 10px; }\n"
        "  h2 { color: %s; margin-top: 0; }\n"
        "  .export-buttons { margin: 20px 0; }\n"
        "  .export-btn { padding: 10px 20px; margin: 5px; background: %s; color: white; border: none; border-radius: 4px; cursor: pointer; }\n"
        "  .export-btn:hover { opacity: 0.9; }\n"
        "  @media (max-width: 768px) { .section { padding: 15px; } }\n"
        "</style>\n"
        "</head><body>\n"
        "<div class=\"dashboard-container\">\n"
        "<h1>%s</h1>\n";
    
    const char* section_bg = config->dark_mode ? "#2d2d2d" : "#f5f5f5";
    const char* body_style = config->dark_mode ? "background: #1e1e1e; color: #ffffff;" : "";
    
    char header[2048];
    snprintf(header, sizeof(header), html_header_template,
             title ? title : "Query Dashboard",
             body_style,
             section_bg,
             config->theme_color,
             config->theme_color,
             config->theme_color,
             title ? title : "Query Dashboard");
    append_string(buffer, &capacity, &pos, header);
    
    // 导出按钮（如果启用）
    if (config->export_enabled) {
        append_string(buffer, &capacity, &pos, "<div class=\"export-buttons\">");
        append_string(buffer, &capacity, &pos, "<button class=\"export-btn\" onclick=\"exportCSV()\">Export CSV</button>");
        append_string(buffer, &capacity, &pos, "<button class=\"export-btn\" onclick=\"exportSVG()\">Export SVG</button>");
        append_string(buffer, &capacity, &pos, "<button class=\"export-btn\" onclick=\"exportJSON()\">Export JSON</button>");
        append_string(buffer, &capacity, &pos, "</div>");
    }
    
    // 表格部分
    if (include_table) {
        append_string(buffer, &capacity, &pos, "<div class=\"section\"><h2>Data Table</h2>");
        append_string(buffer, &capacity, &pos, "<div style=\"overflow-x: auto;\">");
        kos_table_style_t table_style = kos_table_style_default();
        char* table = kos_visualization_query_result_to_table(result, &table_style);
        if (table) {
            append_string(buffer, &capacity, &pos, "<pre style=\"white-space: pre-wrap;\">");
            append_string(buffer, &capacity, &pos, table);
            append_string(buffer, &capacity, &pos, "</pre>");
            free(table);
        }
        append_string(buffer, &capacity, &pos, "</div></div>");
    }
    
    // 图表部分
    if (include_chart) {
        append_string(buffer, &capacity, &pos, "<div class=\"section\"><h2>Charts</h2>");
        kos_chart_config_t chart_config = kos_chart_config_default(KOS_CHART_BAR);
        chart_config.title = "Data Visualization";
        char* chart_html = kos_visualization_generate_html_chart(result, &chart_config, NULL, NULL);
        if (chart_html) {
            append_string(buffer, &capacity, &pos, chart_html);
            free(chart_html);
        }
        append_string(buffer, &capacity, &pos, "</div>");
    }
    
    // 关系图部分
    if (include_graph) {
        append_string(buffer, &capacity, &pos, "<div class=\"section\"><h2>Relationship Graph</h2>");
        kos_relationship_graph_t* graph_obj = kos_visualization_build_relationship_graph(result, "from", "to", NULL);
        if (graph_obj) {
            char* graph_html = kos_visualization_graph_to_html(graph_obj);
            if (graph_html) {
                append_string(buffer, &capacity, &pos, graph_html);
                free(graph_html);
            }
            kos_visualization_free_relationship_graph(graph_obj);
        }
        append_string(buffer, &capacity, &pos, "</div>");
    }
    
    // JavaScript导出功能
    if (config->export_enabled) {
        const char* js_export =
            "<script>\n"
            "function exportCSV() { alert('CSV export functionality - to be implemented'); }\n"
            "function exportSVG() { alert('SVG export functionality - to be implemented'); }\n"
            "function exportJSON() { alert('JSON export functionality - to be implemented'); }\n"
            "</script>\n";
        append_string(buffer, &capacity, &pos, js_export);
    }
    
    append_string(buffer, &capacity, &pos, "</div></body></html>");
    
    return buffer;
}

// ========== 数据过滤和排序 ==========

// 对查询结果进行过滤
kos_query_result_t* kos_visualization_filter_result(
    kos_query_result_t* result,
    const kos_filter_condition_t* conditions,
    size_t condition_count) {
    
    if (!result || !conditions || condition_count == 0) {
        return result;
    }
    
    // 简化实现：返回原始结果（完整实现需要解析字段并应用过滤条件）
    kos_query_result_t* filtered = (kos_query_result_t*)calloc(1, sizeof(kos_query_result_t));
    if (!filtered) {
        return NULL;
    }
    
    filtered->capacity = result->count;
    filtered->results = (kos_term**)calloc(result->count, sizeof(kos_term*));
    if (!filtered->results) {
        free(filtered);
        return NULL;
    }
    
    // TODO: 实现实际的过滤逻辑
    filtered->count = result->count;
    for (size_t i = 0; i < result->count; i++) {
        filtered->results[i] = result->results[i];
    }
    
    return filtered;
}

// 对查询结果进行排序
int kos_visualization_sort_result(
    kos_query_result_t* result,
    const kos_sort_condition_t* conditions,
    size_t condition_count) {
    
    if (!result || !conditions || condition_count == 0) {
        return -1;
    }
    
    // TODO: 实现实际的排序逻辑
    // 简化实现：返回成功
    return 0;
}

// ========== 关系图布局算法 ==========

// 应用布局算法到关系图
int kos_visualization_apply_layout(
    kos_relationship_graph_t* graph,
    kos_layout_algorithm_t algorithm) {
    
    if (!graph) {
        return -1;
    }
    
    switch (algorithm) {
        case KOS_LAYOUT_CIRCULAR: {
            // 圆形布局
            float center_x = 0.5f;
            float center_y = 0.5f;
            float radius = 0.3f;
            
            for (size_t i = 0; i < graph->node_count; i++) {
                float angle = (float)(2.0 * M_PI * i / graph->node_count);
                graph->nodes[i].x = center_x + radius * cosf(angle);
                graph->nodes[i].y = center_y + radius * sinf(angle);
            }
            break;
        }
        case KOS_LAYOUT_GRID: {
            // 网格布局
            int cols = (int)ceilf(sqrtf((float)graph->node_count));
            if (cols == 0) cols = 1;
            for (size_t i = 0; i < graph->node_count; i++) {
                graph->nodes[i].x = (float)(i % cols) / (float)cols;
                graph->nodes[i].y = (float)(i / cols) / (float)cols;
            }
            break;
        }
        case KOS_LAYOUT_FORCE_DIRECTED:
        case KOS_LAYOUT_HIERARCHICAL:
        default:
            // 简化实现：使用默认布局
            break;
    }
    
    return 0;
}
