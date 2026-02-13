// include/kos_visualization_enhanced.h
// Phase 8: Enhanced Visualization
// 增强可视化模块：查询结果可视化、查询历史管理

#ifndef KOS_VISUALIZATION_ENHANCED_H
#define KOS_VISUALIZATION_ENHANCED_H

#include "kos_core.h"
#include "kos_query.h"
#include <stdbool.h>
#include <stddef.h>
#include <time.h>

// ========== 查询结果可视化 ==========

// 表格样式配置
typedef struct {
    bool show_header;           // 是否显示表头
    bool show_border;          // 是否显示边框
    int max_column_width;      // 最大列宽（0表示无限制）
    const char* separator;     // 列分隔符（默认 "|"）
} kos_table_style_t;

// 图表类型
typedef enum {
    KOS_CHART_BAR,             // 柱状图
    KOS_CHART_LINE,            // 折线图
    KOS_CHART_PIE,             // 饼图
    KOS_CHART_SCATTER          // 散点图
} kos_chart_type_t;

// 图表配置
typedef struct {
    kos_chart_type_t type;     // 图表类型
    const char* title;         // 图表标题
    const char* x_label;       // X轴标签
    const char* y_label;       // Y轴标签
    int width;                 // 图表宽度（字符数）
    int height;                // 图表高度（行数）
    bool show_legend;         // 是否显示图例
} kos_chart_config_t;

// 关系图节点
typedef struct {
    kos_term* data;            // 节点数据
    const char* label;          // 节点标签
    float x, y;                // 位置坐标（归一化 0.0-1.0）
    int color;                  // 颜色索引
} kos_graph_node_t;

// 关系图边
typedef struct {
    kos_graph_node_t* from;    // 起始节点
    kos_graph_node_t* to;      // 目标节点
    const char* label;          // 边标签
    int weight;                // 权重（用于显示粗细）
} kos_graph_edge_t;

// 关系图
typedef struct {
    kos_graph_node_t* nodes;  // 节点数组
    size_t node_count;         // 节点数量
    kos_graph_edge_t* edges;   // 边数组
    size_t edge_count;         // 边数量
    const char* title;          // 图标题
} kos_relationship_graph_t;

// ========== 查询历史管理 ==========

// 查询历史记录
typedef struct {
    char* query_id;            // 查询ID（唯一标识）
    char* query_string;        // 查询字符串（KOS-QL）
    kos_query_t* query_object; // 查询对象
    time_t executed_at;        // 执行时间
    size_t result_count;       // 结果数量
    double execution_time_ms; // 执行时间（毫秒）
    bool cached;               // 是否来自缓存
    kos_query_result_t* result; // 查询结果（可选，可能已释放）
} kos_query_history_entry_t;

// 查询历史管理器
typedef struct {
    kos_query_history_entry_t* entries; // 历史记录数组
    size_t count;                       // 记录数量
    size_t capacity;                    // 数组容量
    size_t max_entries;                 // 最大记录数（0表示无限制）
} kos_query_history_t;

// ========== 查询结果可视化 API ==========

// --- 表格输出 ---

// 创建默认表格样式
kos_table_style_t kos_table_style_default(void);

// 将查询结果格式化为表格字符串
char* kos_visualization_query_result_to_table(
    kos_query_result_t* result,
    const kos_table_style_t* style
);

// 打印查询结果为表格
void kos_visualization_print_table(
    kos_query_result_t* result,
    const kos_table_style_t* style
);

// 将查询结果保存为CSV文件
int kos_visualization_save_csv(
    kos_query_result_t* result,
    const char* filename
);

// --- 图表输出 ---

// 创建默认图表配置
kos_chart_config_t kos_chart_config_default(kos_chart_type_t type);

// 生成ASCII图表字符串（用于控制台显示）
char* kos_visualization_generate_ascii_chart(
    kos_query_result_t* result,
    const kos_chart_config_t* config,
    const char* value_field,      // 值字段名
    const char* category_field    // 分类字段名（用于分组）
);

// 打印ASCII图表
void kos_visualization_print_chart(
    kos_query_result_t* result,
    const kos_chart_config_t* config,
    const char* value_field,
    const char* category_field
);

// 生成HTML图表（使用Chart.js）
char* kos_visualization_generate_html_chart(
    kos_query_result_t* result,
    const kos_chart_config_t* config,
    const char* value_field,
    const char* category_field
);

// 保存HTML图表文件
int kos_visualization_save_html_chart(
    kos_query_result_t* result,
    const kos_chart_config_t* config,
    const char* value_field,
    const char* category_field,
    const char* filename
);

// --- 关系图输出 ---

// 从查询结果构建关系图
kos_relationship_graph_t* kos_visualization_build_relationship_graph(
    kos_query_result_t* result,
    const char* from_field,     // 起始节点字段
    const char* to_field,       // 目标节点字段
    const char* relation_field  // 关系字段（可选）
);

// 生成GraphViz DOT格式的关系图
char* kos_visualization_graph_to_dot(kos_relationship_graph_t* graph);

// 生成HTML关系图（使用D3.js或vis.js）
char* kos_visualization_graph_to_html(kos_relationship_graph_t* graph);

// 保存关系图为DOT文件
int kos_visualization_save_graph_dot(kos_relationship_graph_t* graph, const char* filename);

// 保存关系图为HTML文件
int kos_visualization_save_graph_html(kos_relationship_graph_t* graph, const char* filename);

// 释放关系图
void kos_visualization_free_relationship_graph(kos_relationship_graph_t* graph);

// ========== 查询历史管理 API ==========

// 创建查询历史管理器
kos_query_history_t* kos_query_history_create(size_t max_entries);

// 释放查询历史管理器
void kos_query_history_free(kos_query_history_t* history);

// 添加查询历史记录
int kos_query_history_add(
    kos_query_history_t* history,
    const char* query_id,
    const char* query_string,
    kos_query_t* query_object,
    kos_query_result_t* result,
    double execution_time_ms,
    bool cached
);

// 根据查询ID查找历史记录
kos_query_history_entry_t* kos_query_history_find_by_id(
    kos_query_history_t* history,
    const char* query_id
);

// 获取最近N条查询历史
kos_query_history_entry_t* kos_query_history_get_recent(
    kos_query_history_t* history,
    size_t count,
    size_t* actual_count
);

// 根据时间范围查询历史记录
kos_query_history_entry_t* kos_query_history_query_by_time_range(
    kos_query_history_t* history,
    time_t start_time,
    time_t end_time,
    size_t* count
);

// 删除查询历史记录
int kos_query_history_remove(kos_query_history_t* history, const char* query_id);

// 清空查询历史
int kos_query_history_clear(kos_query_history_t* history);

// 导出查询历史为JSON
char* kos_query_history_export_json(kos_query_history_t* history);

// 保存查询历史到文件
int kos_query_history_save(kos_query_history_t* history, const char* filename);

// 从文件加载查询历史
kos_query_history_t* kos_query_history_load(const char* filename);

// ========== Web API 集成（JSON格式） ==========

// 查询结果转JSON（用于Web前端）
char* kos_visualization_query_result_to_json(kos_query_result_t* result);

// 查询历史转JSON（用于Web前端）
char* kos_query_history_to_json(kos_query_history_t* history);

// 生成完整的可视化页面HTML（包含查询结果表格、图表、关系图）
char* kos_visualization_generate_dashboard_html(
    kos_query_result_t* result,
    const char* title,
    bool include_table,
    bool include_chart,
    bool include_graph
);

// 保存可视化仪表板HTML
int kos_visualization_save_dashboard_html(
    kos_query_result_t* result,
    const char* filename,
    const char* title,
    bool include_table,
    bool include_chart,
    bool include_graph
);

// ========== SVG 导出功能 ==========

// 生成SVG格式的图表
char* kos_visualization_generate_svg_chart(
    kos_query_result_t* result,
    const kos_chart_config_t* config,
    const char* value_field,
    const char* category_field
);

// 保存SVG图表文件
int kos_visualization_save_svg_chart(
    kos_query_result_t* result,
    const kos_chart_config_t* config,
    const char* value_field,
    const char* category_field,
    const char* filename
);

// 生成SVG格式的关系图
char* kos_visualization_graph_to_svg(kos_relationship_graph_t* graph);

// 保存关系图为SVG文件
int kos_visualization_save_graph_svg(kos_relationship_graph_t* graph, const char* filename);

// ========== 增强的仪表板配置 ==========

// 仪表板配置选项
typedef struct {
    bool responsive;              // 响应式布局
    bool dark_mode;               // 暗色主题
    bool interactive;             // 交互式功能
    bool export_enabled;          // 启用导出功能
    const char* theme_color;      // 主题颜色
    int chart_count;              // 图表数量（用于多图表布局）
} kos_dashboard_config_t;

// 创建默认仪表板配置
kos_dashboard_config_t kos_dashboard_config_default(void);

// 生成增强版仪表板HTML（支持更多配置选项）
char* kos_visualization_generate_enhanced_dashboard_html(
    kos_query_result_t* result,
    const char* title,
    const kos_dashboard_config_t* config,
    bool include_table,
    bool include_chart,
    bool include_graph
);

// ========== 数据过滤和排序 ==========

// 过滤条件
typedef struct {
    const char* field_name;       // 字段名
    const char* operator;         // 操作符（">", "<", "=", "!=", "contains"等）
    const char* value;            // 比较值
} kos_filter_condition_t;

// 排序方向
typedef enum {
    KOS_SORT_ASC,                 // 升序
    KOS_SORT_DESC                 // 降序
} kos_sort_direction_t;

// 排序条件
typedef struct {
    const char* field_name;       // 字段名
    kos_sort_direction_t direction; // 排序方向
} kos_sort_condition_t;

// 对查询结果进行过滤
kos_query_result_t* kos_visualization_filter_result(
    kos_query_result_t* result,
    const kos_filter_condition_t* conditions,
    size_t condition_count
);

// 对查询结果进行排序
int kos_visualization_sort_result(
    kos_query_result_t* result,
    const kos_sort_condition_t* conditions,
    size_t condition_count
);

// ========== 关系图布局算法 ==========

// 布局算法类型
typedef enum {
    KOS_LAYOUT_FORCE_DIRECTED,    // 力导向布局
    KOS_LAYOUT_HIERARCHICAL,      // 层次布局
    KOS_LAYOUT_CIRCULAR,          // 圆形布局
    KOS_LAYOUT_GRID               // 网格布局
} kos_layout_algorithm_t;

// 应用布局算法到关系图
int kos_visualization_apply_layout(
    kos_relationship_graph_t* graph,
    kos_layout_algorithm_t algorithm
);

#endif // KOS_VISUALIZATION_ENHANCED_H
