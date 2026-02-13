# kos-core 类型推理集成

应用使用 Haskell kos-core 作为形式化类型论内核，进行类型推理与类型检查。

---

## 一、kos-core 能力

| 命令 | 功能 |
|------|------|
| `kos-core infer-term <expr>` | 类型推理：解析 expr，推断类型，输出 `{term, type}` JSON |
| `kos-core check-term <term> <type>` | 类型检查：验证 term : type（空上下文） |
| `kos-core json-term <expr>` | 校验并输出 term JSON（内部使用 infer） |
| `kos-core term <expr>` | 校验并打印 expr : type |
| `kos-core check <file>` | 校验 .kos 模块 |

---

## 二、C Bridge API

### 1. 类型推理

```c
int kos_core_bridge_infer_from_kos(const char* term_expr,
                                   void** term_out, void** type_out,
                                   char* errmsg, size_t errmsg_size);
```

- **输入**：term_expr（.kos 表达式）
- **输出**：term_out、type_out（推理得到的 term 和 type，调用者负责 kos_term_free）
- **返回**：0 成功，-1 失败

### 2. 类型检查

```c
bool kos_core_bridge_check_expr(const char* term_expr, const char* type_expr,
                                char* errmsg, size_t errmsg_size);
```

- **输入**：term_expr、type_expr（.kos 表达式）
- **返回**：true 当且仅当 term : type

### 3. Core 层封装

```c
bool kos_check_via_kos(const char* term_expr, const char* type_expr,
                       char* errmsg, size_t errmsg_size);
```

- 当 kos-core 可用时，委托给 `kos_core_bridge_check_expr`
- 当 kos-core 不可用时，返回 false

---

## 三、集成点

| 场景 | 使用方式 |
|------|----------|
| **elab 信号提炼** | .kos 信号经 `kos_elab_from_kos` → `kos_core_bridge_term_from_kos`（内部使用 parseAndCheckTerm = infer） |
| **本体类型加载** | `kos_ontology_add_type_from_kos` → `kos_core_bridge_term_from_kos` |
| **类型检查（.kos 字符串）** | `kos_check_via_kos(term_kos, type_kos)` → `kos_core_bridge_check_expr` |
| **需要推理类型时** | `kos_core_bridge_infer_from_kos(expr, &term, &type, ...)` |

---

## 四、使用示例

### 类型推理

```c
void* term = NULL;
void* ty = NULL;
char err[256];
if (kos_core_bridge_infer_from_kos("Prop Evt1", &term, &ty, err, sizeof(err)) == 0) {
    printf("term inferred, type: ...\n");
    kos_term_free((kos_term*)term);
    kos_term_free((kos_term*)ty);
} else {
    printf("Error: %s\n", err);
}
```

### 类型检查

```c
char err[256];
if (kos_check_via_kos("< Prop P , Prop Q >", "Sigma(x:Prop P). Prop Q", err, sizeof(err))) {
    printf("Type check passed\n");
} else {
    printf("Type check failed: %s\n", err);
}
```

---

## 五、kos-core 构建与路径

```bash
cd kos-core
cabal build
# 可执行文件: dist-newstyle/build/.../kos-core.exe (或 kos-core)
```

C 应用需能找到 kos-core 可执行文件：

- 将 kos-core 所在目录加入 PATH，或
- 调用 `kos_core_bridge_set_path("/path/to/kos-core.exe")`

---

## 六、相关文件

| 文件 | 说明 |
|------|------|
| `kos-core/app/Main.hs` | infer-term、check-term 命令 |
| `kos-core/src/KosCore/TypeCheck.hs` | infer、check 实现 |
| `include/kos_core_bridge.h` | Bridge API 声明 |
| `src/core/kos_core_bridge.c` | Bridge 实现 |
| `src/core/type_checker.c` | kos_check_via_kos |
