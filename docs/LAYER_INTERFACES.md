# KOS-TL 各层接口厘清

基于代码分析，厘清 Core（kos-core）、Kernel、Runtime 各层之间的接口与调用关系。

---

## 一、架构总览

```
┌─────────────────────────────────────────────────────────────────────────┐
│  Application Layer (应用层)                                              │
│  examples/*.c, domain/*.c                                                 │
└─────────────────────────────────────────────────────────────────────────┘
                                    │
                                    ▼
┌─────────────────────────────────────────────────────────────────────────┐
│  Runtime Layer (L2) - kos_runtime.h                                       │
│  定义"如何运行"：信号源、精化、物化、溯源                                  │
│  src/runtime/elab.c, signal_process.c, material.c, replay.c              │
└─────────────────────────────────────────────────────────────────────────┘
                                    │
                    ┌───────────────┴───────────────┐
                    ▼                               ▼
┌───────────────────────────────┐   ┌───────────────────────────────────────┐
│  Kernel Layer (L1)            │   │  kos-core Bridge (C ↔ Haskell)        │
│  kos_kernel.h                 │   │  kos_core_bridge.h                    │
│  定义"如何改变"：状态演化、   │   │  src/core/kos_core_bridge.c            │
│  小步操作、知识库             │   │  子进程调用 kos-core 可执行文件        │
│  src/kernel/state_step.c,     │   └───────────────────────────────────────┘
│  knowledge_base.c, queue.c   │                       │
└───────────────────────────────┘                       │
                    │                                   │
                    ▼                                   ▼
┌───────────────────────────────┐   ┌───────────────────────────────────────┐
│  Core Layer (L0) - C 实现     │   │  kos-core (Haskell)                    │
│  kos_core.h                   │   │  形式化类型论内核                     │
│  定义"什么是合法的"：          │   │  kos-core/                            │
│  kos_term, kos_check,          │   │  check, term, json-term, infer-term,  │
│  kos_reduce, kos_type_check    │   │  check-term                           │
│  src/core/type_checker.c,      │   └───────────────────────────────────────┘
│  reduction.c                  │
└───────────────────────────────┘
```

---

## 二、Core 层（kos-core）接口

### 2.1 kos-core（Haskell）对外接口

kos-core 作为独立可执行文件，通过命令行子进程调用，**不直接链接**。Bridge 层封装为 C 接口。

| 命令 | Bridge 函数 | 功能 |
|------|-------------|------|
| `kos-core check <file>` | `kos_core_bridge_check_file` | 校验 .kos 模块（含 def 展开 δ） |
| `kos-core term "<expr>"` | `kos_core_bridge_check_term` | 校验单个项 |
| `kos-core json-term "<expr>"` | `kos_core_bridge_term_from_kos` | 校验并输出 C kos_term 兼容 JSON |
| `kos-core infer-term "<expr>"` | `kos_core_bridge_infer_from_kos` | 类型推理，返回 (term, type) |
| `kos-core check-term "<term>" "<type>"` | `kos_core_bridge_check_expr` | 检查 term : type |

**头文件**：`include/kos_core_bridge.h`  
**实现**：`src/core/kos_core_bridge.c`

### 2.2 Core 层（C）接口

| 接口 | 声明位置 | 实现 | 功能 |
|------|----------|------|------|
| `kos_term` | kos_core.h | - | 项/类型数据结构 |
| `kos_check(ctx, term, type)` | kos_core.h | type_checker.c | C 侧结构类型检查 |
| `kos_type_check(ctx, proof, prop)` | kos_core.h | type_checker.c | 委托给 kos_check |
| `kos_check_via_kos(term_expr, type_expr, err)` | kos_core.h | type_checker.c | **委托给 kos_core_bridge_check_expr** |
| `kos_reduce(term)` | kos_core.h | reduction.c | C 侧 β/ι 规约 |
| `kos_type_wellformed(type)` | kos_core.h | type_checker.c | 类型良构判定 |

**注意**：`kos_type_check` 当前使用 C 侧 `kos_check`，**不直接调用** kos-core。只有 `kos_check_via_kos` 显式委托给 kos-core。

---

## 三、Kernel 层接口

### 3.1 头文件与依赖

```c
#include "kos_core.h"   // 必须，使用 kos_term, kos_reduce, kos_type_check
// 不直接包含 kos_core_bridge.h
```

### 3.2 Kernel → Core 调用链

| Kernel 函数 | 调用的 Core 接口 | 说明 |
|-------------|------------------|------|
| `kos_verify_precondition` | `kos_type_check(sigma->K, p, e)` | 前置条件 Pre(e)：验证 p : e |
| `kos_kernel_step` | `kos_verify_precondition` → `kos_type_check` | 单步演化 |
| `kos_kernel_step` | `kos_reduce(e)` | 规约事件 e |
| `kos_kernel_step` | `kos_update_knowledge` | 更新知识集 K |
| `kos_evolution_cycle` | `kos_kernel_step` | 从队列取事件执行 |

**代码位置**：`src/kernel/state_step.c`

```c
// kos_verify_precondition 内部
return kos_type_check(sigma->K, p, e);  // 使用 C 侧 kos_check

// kos_kernel_step 内部
kos_term* reduced_e = kos_reduce(kos_term_copy(e));
```

### 3.3 Kernel 对外接口（kos_kernel.h）

| 接口 | 功能 |
|------|------|
| `kos_state_create(initial_K)` | 创建状态 σ = 〈K, TS, P〉 |
| `kos_state_free` | 释放状态 |
| `kos_queue_*` | 事件队列管理 |
| `kos_kernel_step(sigma, event_pair)` | 对给定事件对执行单步演化 |
| `kos_step(sigma)` | 从队列取事件执行单步 |
| `kos_evolution_cycle(sigma)` | Peek-Verify-Reduce-Confirm 完整周期 |
| `kos_verify_precondition` | 前置条件验证 |
| `kos_verify_postcondition` | 后置条件验证 |
| `kos_update_knowledge` | 知识集更新 |
| `kos_state_set_kb` / `kos_state_get_kb` | 知识库挂载 |

---

## 四、Runtime 层接口

### 4.1 头文件与依赖

```c
#include "kos_kernel.h"   // 必须，使用 kos_state_t, kos_kernel_step
#include "kos_core_bridge.h"  // elab.c 内部使用
```

### 4.2 Runtime → kos-core Bridge 调用链

| Runtime 函数 | 调用的 Bridge 接口 | 说明 |
|--------------|--------------------|------|
| `kos_elab` | `kos_elab_from_kos`（当信号像 .kos 时） | 信号提炼 |
| `kos_elab_from_kos` | `kos_core_bridge_term_from_kos` | .kos 字符串 → kos_term |
| `kos_elab_ex` | `kos_elab` → `kos_elab_from_kos` | 带错误输出的精化 |
| `kos_elab_ex` | `kos_core_bridge_term_from_kos`（失败时填充 errmsg） | 错误信息来自 kos-core |

**代码位置**：`src/runtime/elab.c`

```c
// kos_elab 内部：信号像 .kos 时
if (looks_like_kos) {
    kos_term* from_kos = kos_elab_from_kos(raw_signal);
    if (from_kos) return from_kos;
}

// kos_elab_from_kos 内部
kos_term* event = (kos_term*)kos_core_bridge_term_from_kos(kos_expr, err, sizeof(err));
```

### 4.3 Runtime → Kernel 调用链

| Runtime 函数 | 调用的 Kernel 接口 | 说明 |
|--------------|--------------------|------|
| `kos_runtime_process_signal` | `kos_elab_ex` | 先精化信号 |
| `kos_runtime_process_signal` | `s_traceability_handler(event, sigma)` | 溯源（若已注册） |
| `reasoning_session` 等 | `kos_elab` → `kos_kernel_step` | 精化后入队或直接演化 |

**注意**：`kos_runtime_process_signal` 只做精化+溯源，**不直接调用** `kos_kernel_step`。应用层需自行将 `event_pair` 送入 Kernel 演化。

### 4.4 Runtime 对外接口（kos_runtime.h）

| 接口 | 功能 |
|------|------|
| `kos_elab(raw_signal, ontology)` | 信号 → 事件对，失败返回 NULL |
| `kos_elab_ex` | 带错误输出的精化 |
| `kos_elab_from_kos(raw_signal)` | .kos 字符串 → 事件对（经 kos-core 校验） |
| `kos_runtime_process_signal` | 精化 + 自动溯源 |
| `kos_materialize` | 物化到存储后端 |
| `kos_signal_buffer_*` | 信号缓冲区 |
| `kos_runtime_register_traceability_handler` | 注册溯源处理器 |

---

## 五、Ontology / Domain 层对 Bridge 的调用

| 模块 | 调用的 Bridge 接口 | 说明 |
|------|--------------------|------|
| `ontology_manager.c` | `kos_core_bridge_term_from_kos` | 从 .kos 加载类型定义 |
| `runtime_update.c` | `kos_core_bridge_term_from_kos` | 运行时类型更新 |
| `ontology_setup.c` (manufacturing/finance) | `kos_core_bridge_available` | 检测 kos-core 可用性 |

---

## 六、接口调用关系汇总

```
Runtime 层
├── kos_elab / kos_elab_from_kos
│   └── kos_core_bridge_term_from_kos  ← 调用 kos-core (Haskell)
├── kos_runtime_process_signal
│   └── kos_elab_ex → kos_elab_from_kos
│   └── traceability_handler (可选)
└── 应用层负责：event_pair → kos_queue_enqueue 或 kos_kernel_step

Kernel 层
├── kos_kernel_step
│   ├── kos_verify_precondition
│   │   └── kos_type_check(K, p, e)  ← 使用 C 侧 kos_check，不调用 kos-core
│   ├── kos_reduce(e)                ← C 侧规约
│   └── kos_update_knowledge
└── kos_step / kos_evolution_cycle
    └── kos_kernel_step

Core 层 (C)
├── kos_check / kos_type_check       ← 纯 C 实现
├── kos_check_via_kos                ← 显式委托 kos_core_bridge_check_expr
└── kos_reduce                       ← 纯 C 实现
```

---

## 七、关键结论

1. **kos-core（Haskell）**：仅通过 **Bridge** 被调用，调用方为 Runtime（elab）、Ontology（类型加载）、应用层（显式 infer/check）。
2. **Kernel 层**：不直接依赖 Bridge，使用 C 侧 `kos_type_check`（即 `kos_check`）和 `kos_reduce`。
3. **Runtime 层**：在信号为 .kos 形式时，通过 `kos_elab_from_kos` → `kos_core_bridge_term_from_kos` 调用 kos-core，实现“非法类型不能创建”的防火墙。
4. **类型检查双路径**：
   - **C 路径**：`kos_type_check` → `kos_check` → `kos_check_internal`（结构检查）
   - **kos-core 路径**：`kos_check_via_kos` → `kos_core_bridge_check_expr`（形式化校验）
