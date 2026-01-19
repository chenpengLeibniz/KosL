# 类型依存关系可视化功能总结

## 完成的功能

✅ **类型依存关系提取**
- 从 `TypeOntology` 中提取类型定义之间的依存关系
- 支持Σ类型（依赖和类型）、Π类型（依赖积类型）、Sum类型（和类型）
- 递归遍历类型结构，提取所有类型引用

✅ **多种输出格式**
1. **GraphViz DOT格式** (`output.dot`)
   - 可用于生成PNG/SVG/PDF图像
   - 使用不同颜色标识不同类型的类型构造器
   - 绿色：Σ类型（事件类型）
   - 黄色：Π类型（谓词类型）
   - 灰色：基础类型（ID、TIME）

2. **HTML可视化** (`output.html`)
   - 使用Mermaid.js进行交互式渲染
   - 可在Web浏览器中直接查看
   - 支持缩放和交互

3. **文本树状图** (`output.txt`)
   - 控制台友好的文本输出
   - 清晰显示类型层次结构

4. **JSON格式** (`output_dependencies.json`)
   - 机器可读的依存关系数据
   - 可用于进一步处理和分析

✅ **工具程序**
- `test_visualization.exe`：测试程序，从程序初始化本体并生成可视化
- `visualize_ontology.exe`：命令行工具，从JSON文件生成可视化（需要实现JSON反序列化）

## 实现细节

### 核心文件

1. **`include/kos_visualization.h`**
   - 定义可视化API接口
   - 定义依存关系图数据结构

2. **`src/core/visualization.c`**
   - 实现依存关系提取逻辑
   - 实现各种格式的输出生成

3. **`tools/test_visualization.c`**
   - 测试程序，演示可视化功能的使用

4. **`tools/visualize_ontology.c`**
   - 命令行工具（需要JSON反序列化支持）

### 数据结构

```c
// 类型引用信息
typedef struct {
    char* type_name;           // 被引用的类型名称
    char* reference_kind;      // 引用方式
    int depth;                 // 在类型树中的深度
} TypeReference;

// 类型依存关系节点
typedef struct {
    char* type_name;              // 类型名称
    TypeReference* dependencies;  // 依赖的类型列表
    size_t dependency_count;      // 依赖数量
    char* type_kind;              // 类型种类："ID", "TIME", "PROP", "SIGMA", "PI", "SUM"
} TypeDependencyNode;

// 类型依存关系图
typedef struct {
    TypeDependencyNode* nodes;    // 节点数组
    size_t node_count;            // 节点数量
    size_t capacity;              // 数组容量
} TypeDependencyGraph;
```

## 使用示例

### 运行测试程序

```bash
cd build/bin/Release
./test_visualization.exe
```

输出示例：
```
=== Testing Type Dependency Visualization ===

1. Initializing ontology...
   ✓ Ontology initialized (11 types)

2. Extracting type dependencies...
   ✓ Dependencies extracted (11 nodes)

3. Text tree visualization:
   =========================
Type Dependency Graph
====================

FailEvt (SIGMA)
  └─> BatchID
  └─> ErrorCode
  └─> Time

ProcStep (SIGMA)
  └─> BatchID
  └─> Machine

Anomaly (SIGMA)
  └─> Machine
  └─> Param
  └─> ParamValue
  └─> Time

InRoute (PI)
  └─> BatchID
  └─> Machine

Overlap (PI)
  └─> Time

4. Generating visualization files...
   ✓ GraphViz DOT: output.dot
   ✓ HTML (Mermaid): output.html
   ✓ JSON dependencies: output_dependencies.json
```

### 生成GraphViz图像

```bash
# 需要安装GraphViz
dot -Tpng output.dot -o output.png
dot -Tsvg output.dot -o output.svg
```

### 查看HTML可视化

直接在Web浏览器中打开 `output.html` 文件即可查看交互式图形。

## 当前限制

1. **JSON反序列化未实现**：`kos_ontology_deserialize` 函数目前返回NULL，因此 `visualize_ontology` 工具暂时无法从JSON文件加载本体。测试程序使用程序初始化本体。

2. **自引用显示**：基础类型（ID、TIME、PROP）在可视化中显示为自引用（如BatchID -> BatchID）。这是当前实现的行为，因为这些基础类型确实是通过自身定义的。

3. **依赖去重**：当前实现会提取所有类型引用，包括重复的。未来可以添加去重和引用计数功能。

## 可视化效果

从生成的输出可以看到：

- **FailEvt**（Σ类型）依赖于：BatchID, ErrorCode, Time
- **ProcStep**（Σ类型）依赖于：BatchID, Machine
- **Anomaly**（Σ类型）依赖于：Machine, Param, ParamValue, Time
- **InRoute**（Π类型）依赖于：BatchID, Machine
- **Overlap**（Π类型）依赖于：Time

这些依存关系清晰地展示了类型系统的结构和类型之间的层次关系。

## 后续改进方向

1. 实现完整的JSON反序列化，使 `visualize_ontology` 工具能够从JSON文件加载本体
2. 优化基础类型的自引用显示（可以选择隐藏或标记为"基础类型"）
3. 添加依赖去重和引用计数功能
4. 支持过滤和搜索功能（如只显示特定类型的依赖）
5. 添加交互式图形编辑功能
6. 支持导出为其他格式（SVG、PDF等）

## 总结

类型依存关系可视化功能已成功实现，可以清晰地展示类型系统结构中类型之间的依存关系。工具支持多种输出格式，满足不同的使用场景。虽然JSON加载功能尚未完成，但通过测试程序已经可以验证可视化功能的正确性。




























