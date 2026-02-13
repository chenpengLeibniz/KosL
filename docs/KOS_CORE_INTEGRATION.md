# Kernel/Runtime 调用 kos-core 形式化内核

本文档说明 C 实现的 kernel 和 runtime 层如何调用 Haskell 实现的 kos-core 形式化内核。

---

## 一、架构概览

```
┌─────────────────────────────────────────────────────────────────┐
│  C 层：Kernel (state_step.c) / Runtime (elab.c, system_init.c)   │
│  - 使用 kos_term (C struct) 表示项                                 │
│  - 使用 kos_type_check() (C 实现) 进行类型检查                     │
└─────────────────────────────────────────────────────────────────┘
                                    │
                                    │ kos_core_bridge_*()
                                    ▼
┌─────────────────────────────────────────────────────────────────┐
│  kos_core_bridge (C)                                             │
│  - 子进程调用 kos-core 可执行文件                                  │
│  - 校验 .kos 源文件 / 单 Term 表达式                              │
└─────────────────────────────────────────────────────────────────┘
                                    │
                                    │ subprocess: kos-core check/term
                                    ▼
┌─────────────────────────────────────────────────────────────────┐
│  kos-core (Haskell)                                              │
│  - Parser 即门卫，唯一合法 Term 来源                               │
│  - 形式化类型检查 (infer/check)                                   │
│  - 输出：OK / ERROR                                               │
└─────────────────────────────────────────────────────────────────┘
```

---

## 二、Bridge 接口 (kos_core_bridge.h)

| 函数 | 用途 |
|------|------|
| `kos_core_bridge_set_path(path)` | 设置 kos-core 可执行文件路径 |
| `kos_core_bridge_check_file(filepath, errmsg, size)` | 校验 .kos 文件 |
| `kos_core_bridge_check_source(kos_source, errmsg, size)` | 校验 .kos 源字符串 |
| `kos_core_bridge_check_term(term_expr, errmsg, size)` | 校验单个项表达式 |
| `kos_core_bridge_available()` | 检测 kos-core 是否可用 |

---

## 三、集成场景

### 3.1 系统初始化时校验本体

当从 .kos 文件加载本体时，可先经 kos-core 校验：

```c
#include "kos_core_bridge.h"

kos_state_t* kos_runtime_init_from_kos(const char* kos_filepath) {
    char err[512];
    if (!kos_core_bridge_check_file(kos_filepath, err, sizeof(err))) {
        fprintf(stderr, "kos-core validation failed: %s\n", err);
        return NULL;  // 拒绝加载非法本体
    }
    // 校验通过后，使用 C 层加载（或解析 .kos 构建 kos_term）
    return kos_runtime_init(/* ... */);
}
```

### 3.2 Kernel 前置条件验证（可选增强）

当前 `kos_verify_precondition` 使用 C 的 `kos_type_check`。若需形式化保障，可：

1. 将 `sigma->K` 和 `event_pair` 序列化为 .kos 源
2. 调用 `kos_core_bridge_check_source` 校验
3. 通过后才执行 `kos_kernel_step`

**注意**：C 的 `kos_term` 与 Haskell `Term` 需有序列化/反序列化约定，当前 Bridge 仅支持 .kos 文本。

### 3.3 Runtime elab 精化前校验

当 elab 从 .kos 定义的 ontology 构造事件时，可先校验 ontology 模块：

```c
// 在加载 ontology 模块后
if (kos_core_bridge_available()) {
    if (!kos_core_bridge_check_file(ontology_path, err, sizeof(err))) {
        return NULL;  // 逻辑防火墙：拒绝非法本体
    }
}
```

---

## 四、构建与部署

### 4.1 构建 kos-core

```bash
cd kos-core
cabal build
# 或
stack build
```

可执行文件位于 `kos-core/dist-newstyle/build/.../kos-core` 或 `kos-core/.stack-work/.../kos-core`。

### 4.2 配置 Bridge 路径

默认从 `PATH` 查找 `kos-core` 或 `kos-core.exe`。若 kos-core 不在 PATH：

```c
kos_core_bridge_set_path("/path/to/kos-core/dist-newstyle/build/.../kos-core");
```

### 4.3 CMake 集成

Bridge 源文件 `src/core/kos_core_bridge.c` 已加入 `CORE_SOURCES`，随主系统一起编译。演示程序 `kos_core_bridge_demo` 展示 Bridge 用法。

确保构建 kos-core 后，将 kos-core 可执行文件复制到系统 PATH 或通过 `kos_core_bridge_set_path()` 配置路径。

---

## 五、数据流与表示差异

| 层 | 表示 | 校验 |
|----|------|------|
| **kos-core (Haskell)** | `Term` (代数类型) | `parseAndCheckModule` / `parseAndCheckTerm` |
| **C Kernel/Runtime** | `kos_term` (struct + union) | `kos_type_check` (C) 或 Bridge → kos-core |

**关键点**：C 的 `kos_term` 可由 `kos_mk_*` 直接构造，可绕过 Parser。Bridge 仅在校验 **.kos 源** 时提供形式化保障。若 C 层通过 `kos_mk_*` 构造项，则仍依赖 C 的 `kos_type_check`，无形式化保证。

**建议**：将 .kos 作为本体定义的唯一来源，C 层通过 Bridge 校验后再解析构建 `kos_term`（需实现 .kos → kos_term 的 C 解析器，或由 kos-core 输出序列化格式供 C 反序列化）。
