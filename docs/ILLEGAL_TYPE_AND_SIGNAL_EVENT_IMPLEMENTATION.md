# 非法类型不能创建 & 信号自动转化为事件 - 实现总结

## 1. 非法类型不能创建

### 设计原则
- **kos-core 为唯一合法来源**：只有经 Haskell 形式化内核校验通过的项才能转为 C 层 `kos_term`
- **Kernel/Runtime 层**：类型创建、本体注册均需经 Bridge 校验

### 实现要点

#### 1.1 kos-core 新增 `json-term` 命令
- **路径**：`kos-core/app/Main.hs`
- **用法**：`kos-core json-term "<expr>"`
- **行为**：解析并类型检查 expr，成功则输出 C `kos_term` 兼容的 JSON 到 stdout，失败则输出错误到 stderr 并退出非 0

#### 1.2 Bridge 新增 `kos_core_bridge_term_from_kos`
- **头文件**：`include/kos_core_bridge.h`
- **实现**：`src/core/kos_core_bridge.c`
- **接口**：`void* kos_core_bridge_term_from_kos(const char* term_expr, char* errmsg, size_t errmsg_size)`
- **流程**：调用 `kos-core json-term "<expr>"`，捕获 stdout JSON，`kos_term_deserialize` 得到 `kos_term*`

#### 1.3 本体从 .kos 添加类型
- **接口**：`kos_ontology_add_type_from_kos(TypeOntology*, name, kos_type_expr, ctx, errmsg, errmsg_size)`
- **实现**：`src/core/ontology_manager.c`
- **流程**：`kos_core_bridge_term_from_kos` → `kos_ontology_add_type_definition`，失败则拒绝（非法类型不能创建）

#### 1.4 既有防护
- `kos_mk_sigma`、`kos_mk_pi`、`kos_mk_sum`：子类型 `kos_type_wellformed` 检查
- `kos_ontology_add_type_definition`：`type_def` 必须 `kos_type_wellformed`
- `kos_check`：入口处检查 `kos_type_wellformed(type)`

---

## 2. 信号自动转化为事件

### 设计原则
- **信号为 .kos 表达式时**：经 kos-core 校验后直接转为 `<event, proof>` 对
- **支持两种输入**：纯 .kos 字符串、JSON `{"_kos": "<.kos expr>"}`

### 实现要点

#### 2.1 `kos_elab_from_kos`
- **头文件**：`include/kos_runtime.h`
- **实现**：`src/runtime/elab.c`
- **接口**：`kos_term* kos_elab_from_kos(bitstream raw_signal)`
- **流程**：
  1. 若信号以 `{` 开头，尝试从 JSON 提取 `_kos` 字段
  2. 否则将整个信号视为 .kos 表达式
  3. 调用 `kos_core_bridge_term_from_kos` 校验
  4. 成功则构造 `<event, Prop("KosValidated")>` 对

#### 2.2 使用示例
```c
// 纯 .kos 信号
const char* sig = "Prop Evt1";
bitstream bs = { (unsigned char*)sig, strlen(sig) };
kos_term* pair = kos_elab_from_kos(bs);

// JSON 信号
const char* json = "{\"_kos\": \"Prop Evt1\"}";
bitstream bs2 = { (unsigned char*)json, strlen(json) };
kos_term* pair2 = kos_elab_from_kos(bs2);
```

---

## 3. 文件变更清单

| 文件 | 变更 |
|------|------|
| `kos-core/app/Main.hs` | 新增 `json-term` 命令 |
| `kos-core/src/KosCore/JSON.hs` | 修复 jsonObj 尾逗号，完善 kind 等字段 |
| `kos-core/kos-core.cabal` | 暴露 `KosCore.JSON` |
| `include/kos_core_bridge.h` | 声明 `kos_core_bridge_term_from_kos` |
| `src/core/kos_core_bridge.c` | 实现 `run_json_term`、`kos_core_bridge_term_from_kos` |
| `include/kos_ontology.h` | 声明 `kos_ontology_add_type_from_kos` |
| `src/core/ontology_manager.c` | 实现 `kos_ontology_add_type_from_kos` |
| `include/kos_runtime.h` | 声明 `kos_elab_from_kos` |
| `src/runtime/elab.c` | 实现 `kos_elab_from_kos`、`extract_kos_from_json` |
| `examples/kos_core_bridge_demo.c` | 演示新接口 |
| `CMakeLists.txt` | 更新 `kos_core_bridge_demo` 链接 |

---

## 4. 构建与测试

```bash
# 构建 kos-core
cd kos-core && cabal build

# 测试 json-term
kos-core json-term "Prop P"
# 输出: {"kind":"PROP","val":"P"}

# 构建 C 项目
cmake -B build && cmake --build build --target kos_core_bridge_demo
```
