# 类型依存关系可视化指南

## 概述

KOS类型系统可视化工具用于分析和可视化类型定义之间的依存关系。工具支持多种输出格式，包括GraphViz DOT、HTML（Mermaid.js）、文本树状图和JSON格式。

## 功能特性

- **类型依存关系提取**：自动从类型本体中提取类型之间的依存关系
- **多种输出格式**：
  - **GraphViz DOT**：可生成PNG/SVG图像
  - **HTML (Mermaid.js)**：交互式Web可视化
  - **文本树状图**：控制台友好的文本输出
  - **JSON**：机器可读的依存关系数据

## 使用方法

### 1. 测试可视化功能

使用测试程序直接从程序中初始化本体并生成可视化：

```bash
# 编译后运行
cd build/bin/Release
./test_visualization.exe
```

这将生成以下文件：
- `output.dot` - GraphViz DOT格式
- `output.html` - HTML可视化（使用Mermaid.js）
- `output_dependencies.json` - JSON格式的依存关系数据

### 2. 从JSON文件生成可视化

```bash
# 使用visualize_ontology工具（需要实现JSON反序列化）
./visualize_ontology.exe manufacturing_ontology.json output all
```

参数说明：
- `input_json_file`：输入的JSON本体文件路径
- `output_prefix`：输出文件前缀（可选，默认：output）
- `format`：输出格式（可选，默认：all）
  - `dot`：只生成GraphViz DOT文件
  - `html`：只生成HTML文件
  - `text`：只生成文本文件
  - `json`：只生成JSON文件
  - `all`：生成所有格式

## 输出格式说明

### GraphViz DOT格式

生成 `.dot` 文件，可以使用GraphViz工具生成图像：

```bash
# 生成PNG图像
dot -Tpng output.dot -o output.png

# 生成SVG图像
dot -Tsvg output.dot -o output.svg

# 生成PDF
dot -Tpdf output.dot -o output.pdf
```

GraphViz需要单独安装：
- Windows: 从 https://graphviz.org/download/ 下载安装
- Linux: `sudo apt-get install graphviz`
- macOS: `brew install graphviz`

### HTML格式（Mermaid.js）

生成的HTML文件可以直接在Web浏览器中打开查看交互式图形。

特点：
- 使用Mermaid.js库进行渲染
- 支持缩放和交互
- 不同颜色标识不同类型的类型构造器：
  - **绿色**：Σ类型（事件类型）
  - **黄色**：Π类型（谓词类型）
  - **灰色**：基础类型（ID、TIME等）
  - **蓝色**：Prop类型

### 文本树状图格式

文本格式适合在控制台查看，显示类型之间的依存关系：

```
Type Dependency Graph
====================

FailEvt (SIGMA)
  └─> BatchID
  └─> ErrorCode
  └─> Time

ProcStep (SIGMA)
  └─> BatchID
  └─> Machine
  ...
```

### JSON格式

JSON格式提供机器可读的依存关系数据，可用于进一步处理：

```json
{
  "nodes": [
    {
      "name": "FailEvt",
      "kind": "SIGMA",
      "dependencies": ["BatchID", "ErrorCode", "Time"]
    },
    ...
  ]
}
```

## 类型依存关系说明

### 依存关系提取规则

可视化工具从类型定义（`kos_term*`）中提取类型引用：

1. **Σ类型（依赖和类型）**：提取domain和body中的类型引用
   - 例如：`FailEvt ≡ Σ(b: BatchID). Σ(err: ErrorCode). Σ(t: Time). Prop`
   - 依赖：BatchID, ErrorCode, Time

2. **Π类型（依赖积类型）**：提取domain和body中的类型引用
   - 例如：`InRoute ≡ Π(b: BatchID). Π(m: Machine). Prop`
   - 依赖：BatchID, Machine

3. **Sum类型（和类型）**：提取left和right类型引用

4. **基础类型（ID、TIME、PROP）**：不依赖其他类型定义

### 可视化中的类型分类

- **基础类型**：ID、TIME、PROP等基础Sort类型
- **Σ类型**：事件类型（如FailEvt、ProcStep、Anomaly）
- **Π类型**：谓词类型（如InRoute、Overlap）
- **Sum类型**：联合类型

## API参考

### 核心函数

```c
// 从类型本体提取依存关系图
TypeDependencyGraph* kos_visualization_extract_dependencies(TypeOntology* ontology);

// 生成GraphViz DOT格式
char* kos_visualization_generate_dot(TypeDependencyGraph* graph);
int kos_visualization_save_dot(TypeDependencyGraph* graph, const char* filename);

// 生成HTML格式
char* kos_visualization_generate_html(TypeDependencyGraph* graph, const char* title);
int kos_visualization_save_html(TypeDependencyGraph* graph, const char* filename, const char* title);

// 生成文本树状图
char* kos_visualization_generate_text_tree(TypeDependencyGraph* graph);
void kos_visualization_print_text_tree(TypeDependencyGraph* graph);

// 生成JSON格式
char* kos_visualization_generate_json(TypeDependencyGraph* graph);
int kos_visualization_save_json(TypeDependencyGraph* graph, const char* filename);

// 便捷函数：从JSON文件直接生成可视化
int kos_visualization_from_json_file(const char* input_json_file, 
                                     const char* output_file_prefix,
                                     const char* format);

// 释放依存关系图
void kos_visualization_free_graph(TypeDependencyGraph* graph);
```

## 示例

### 使用C API

```c
#include "kos_visualization.h"
#include "kos_ontology.h"

// 初始化本体
TypeOntology* ontology = kos_manufacturing_ontology_init();

// 提取依存关系
TypeDependencyGraph* graph = kos_visualization_extract_dependencies(ontology);

// 生成HTML可视化
kos_visualization_save_html(graph, "types.html", "Type Dependency Graph");

// 打印文本树
kos_visualization_print_text_tree(graph);

// 清理
kos_visualization_free_graph(graph);
kos_ontology_free(ontology);
```

## 注意事项

1. **JSON反序列化**：当前版本的 `kos_ontology_deserialize` 尚未完全实现，因此 `visualize_ontology` 工具需要从程序中初始化本体。完整的JSON加载功能将在后续版本中实现。

2. **类型引用去重**：当前实现会提取所有类型引用，包括重复的。未来版本可能会添加去重和引用计数功能。

3. **大型类型系统**：对于包含大量类型的本体，生成的图形可能会很大。建议使用GraphViz的布局算法（如`dot`、`neato`、`fdp`）来优化布局。

## 未来改进

- [ ] 实现完整的JSON反序列化支持
- [ ] 添加类型引用去重和计数
- [ ] 支持过滤和搜索功能
- [ ] 添加交互式图形编辑功能
- [ ] 支持导出为其他格式（SVG、PDF等）




























