# KOS-TL (Knowledge Operation System - Type Logic)

[中文](#中文) | [English](#english)

---

<a name="中文"></a>
## 中文

基于《KOS-TL (Knowledge Operation System Type Logic)》设计原则开发的"知行合一"逻辑框架系统。

**文档索引**：[docs/README_CN.md](docs/README_CN.md)（中文） | [docs/README_EN.md](docs/README_EN.md)（English）

## 系统架构

系统严格划分为三个逻辑层，确保逻辑严密性与工程效率的平衡：

| 层级 | 名称 | 核心职责 | 技术实现重点 |
| --- | --- | --- | --- |
| **L0: Core** | **静态真理层** | 定义"什么是合法的"。建立基于直觉主义受限类型理论（ITT）的类型约束 | 依赖类型检查器（Bidirectional Type Checking）、证明项构造 |
| **L1: Kernel** | **动态迁移层** | 定义"如何改变"。通过小步操作语义处理事件驱动的状态演化 | 确定性状态机、事件队列管理、因果回溯 |
| **L2: Runtime** | **环境演化层** | 定义"如何运行"。处理外部 I/O、物理存储映射及信号提炼 | 信号提炼算子（elab）、物理存储管理器（M） |

## 核心功能

- **知识对象原子化 (Σ-Types)**：所有进入系统的知识必须以 `<d, p>` 的形式存在，即将数据 `d` 与其符合业务逻辑的证明 `p` 强耦合，消除"无根数据"
- **确定性演化 (Small-step Semantics)**：系统状态迁移必须由经过验证的事件（Event）驱动，且严禁非确定性选择
- **计算反射与内生审计 (Reflexivity)**：系统在执行每一次逻辑演化时，必须自动合成等价性证明（Identity Proof），实现运行时的实时形式化审计
- **异步提炼与顺序提交**：物理信号可并行提炼，但内核必须顺序提交，以保证因果链的唯一性

## 关键算子

- **elab (提炼算子)**：将原始物理比特流映射为带有逻辑证明的事件对象 `<e, p>`
- **STEP (迁移算子)**：驱动系统从旧状态 `σ` 到新状态 `σ'` 的单调演化
- **M (具象化算子)**：将抽象的逻辑结论下沉为物理动作（如数据库 ACID 事务或硬件电压控制）

## 构建说明

### 使用 CMake 构建

```bash
# 创建构建目录
mkdir build
cd build

# 配置（使用 MinGW）
cmake -G "MinGW Makefiles" ..

# 或者使用 Visual Studio
cmake ..

# 构建
cmake --build .

# 运行
./bin/kos_system.exe
```

## 项目结构

```
KosL/
├── include/                    # 公共头文件
│   ├── kos_core.h             # L0 对接：C 侧 term 与接口（与 kos-core 交互用）
│   ├── kos_kernel.h           # L1 Kernel 层：状态 σ、队列、STEP
│   ├── kos_trace.h             # Event Log（Trace）、确定性重放
│   ├── kos_runtime.h           # L2 Runtime 层：elab、存储、重放
│   ├── kos_kernel_context.h   # Γ（类型/谓词集合）
│   ├── kos_kernel_session.h   # 会话：Γ + σ + trace
│   ├── kos_kernel_scenarios.h # 六大场景 API
│   ├── kos_knowledge_base.h   # 知识库与依赖图
│   ├── kos_core_bridge.h      # 与 kos-core（Haskell）桥接声明
│   ├── kos_ontology.h         # 本体类型与谓词
│   ├── kos_ontology_registry.h # 多本体注册
│   ├── kos_query.h            # 查询引擎
│   ├── kos_api.h / kos_*.h    # 领域与扩展 API
│   └── ...
├── src/
│   ├── core/                  # L0 与 kos-core 对接层（核心实现见 kos-core/）
│   │   ├── type_builder.c     # C 侧 term 构造（Σ/Π/Pair/Val/Prop 等），供序列化与桥接
│   │   ├── storage.c          # JSON 序列化/反序列化，与 kos-core 交换数据结构
│   │   ├── universe.c         # 双轴 Universe 的 C 侧表示
│   │   ├── core_kos_adapter.c # .kos 语法与 C 侧 term 的适配
│   │   ├── kos_core_bridge.c  # 调用 kos-core 可执行文件（check-term / prove 等）
│   │   ├── ontology_manager.c # 本体加载与管理（与 kos-core 上下文配合）
│   │   └── visualization.c   # 图谱数据生成
│   ├── kernel/                # L1 Kernel 实现（MTK 对齐）
│   │   ├── state_step.c       # σ 管理、事件队列、STEP、state_hash
│   │   ├── trace.c            # Event Log、kos_trace_replay、持久化
│   │   ├── kernel_context.c  # Γ 管理
│   │   ├── kernel_session.c  # 会话：事件入队、小步、根因查找
│   │   ├── kernel_scenarios.c # 根因/反事实/合规/审计/治理场景
│   │   ├── knowledge_base.c  # 知识库与物化项
│   │   └── event_op_registry.c# 事件操作注册
│   ├── runtime/               # L2 Runtime 实现
│   │   ├── elab.c             # 精化算子：信号 → 事件对 ⟨e,p⟩
│   │   ├── material.c         # 具象化 M、两阶段提交
│   │   ├── replay.c           # 精化轨迹/Event Log 重放、崩溃恢复
│   │   ├── storage_manager.c  # 物理存储后端（内存/文件）
│   │   ├── scheduler_relay.c  # 信号缓冲、顺序提交
│   │   ├── signal_process.c   # 信号处理与自动溯源
│   │   ├── system_init.c      # 系统初始化
│   │   └── signal_source_registry.c
│   ├── ontology/              # 本体版本与推理
│   │   ├── ontology_registry.c
│   │   ├── version_manager.c / runtime_update.c / impact_analysis.c
│   │   └── reasoning_session.c
│   ├── domain/                # 领域实现
│   │   ├── manufacturing/    # 制造业：类型、谓词、溯源、因果索引
│   │   └── finance/          # 金融：类型与精化
│   ├── query/                 # 查询引擎
│   ├── performance/           # 缓存、索引、B+ 树
│   ├── integration/           # 数据集成与融合
│   ├── stream/                # 流处理
│   ├── security/              # RBAC、审计
│   ├── api/                   # REST 服务
│   ├── utils/                 # 信号模拟等
│   └── visualization/         # 查询结果可视化
├── examples/                  # 示例与演示
│   ├── kernel_example.c       # Kernel σ、STEP、Trace
│   ├── kos_tl_end_to_end_demo.c
│   ├── quality_traceability_demo.c / manufacturing_kb_root_cause_demo.c
│   ├── core_sigma_example.c / core_pi_example.c / value_predicate_demo.c
│   ├── signal_process_demo.c / stream_demo.c / query_example.c
│   └── ...
├── tools/                     # 可视化与代码生成
│   ├── visualize_ontology.c / generate_html_visualization.c
│   └── generate_*_types.py
├── docs/                      # 设计文档
│   ├── README_CN.md / README_EN.md
│   ├── MTK_KERNEL_RUNTIME_ALIGNMENT.md  # MTK 与 Kernel/Runtime 对齐
│   ├── KERNEL_LAYER_SUMMARY.md / RUNTIME_API.md / CORE_API.md
│   └── ...
├── kos-core/                  # L0 Core 核心实现（Haskell）：类型检查、归约、证明
│   └── examples/*.kos         # 示例 .kos 源文件
├── kos-web/                   # Web 展示（FastAPI + React）
│   ├── backend/               # API、kernel_engine、信号模拟
│   └── frontend/               # 页面：Core / Runtime / Kernel / 图谱 / 轨迹
├── compiler/                  # KOS-TL 编译器（词法/语法/类型检查）
├── monograph/                 # 专著 LaTeX 与知识图谱 HTML
├── main.c                     # 主程序入口
├── CMakeLists.txt             # CMake 构建配置
├── New_Design_Plan.md         # 三年路线图与最小可信内核（MTK）设计
└── README.md                  # 本说明
```

## 核心文件功能描述

### L0: Core 层（静态真理层）

**说明**：L0 的**核心实现**在 **kos-core/**（Haskell），提供形式化的类型检查、归约与证明。**src/core/** 与 **include/kos_core.h** 的作用是与 kos-core 的**对接**：在 C 侧维护可序列化的 term 结构、调用 kos-core 可执行文件，供 Kernel/Runtime 使用。

| 文件 | 功能 |
|------|------|
| **kos-core/**（Haskell） | **L0 核心实现**：.kos 解析、类型检查、归约、证明（check-term、prove 等）；形式化语义在此实现 |
| **include/kos_core.h** | C 侧 term 类型定义（term_kind、kos_term）与对接接口声明；Kernel/Runtime 通过此与 kos-core 交互 |
| **src/core/type_builder.c** | C 侧 term 构造（Σ/Π/Pair/Val/Prop/App/Lam 等）、`kos_mk_*`、`kos_term_copy` / `kos_term_free`，供序列化与桥接 |
| **src/core/storage.c** | JSON 序列化/反序列化，与 kos-core 交换 term 结构；C 侧类型检查/归约的兼容实现（可委托 kos-core） |
| **src/core/universe.c** | 双轴 Universe（U_i / Type_i）的 C 侧表示 |
| **src/core/kos_core_bridge.c** | **对接核心**：调用 kos-core 可执行文件（check-term、prove-json 等），供 Kernel 场景、IDE、精化流程使用 |
| **src/core/core_kos_adapter.c** | .kos 语法与 C 侧 term 的适配 |
| **src/core/ontology_manager.c** | 本体加载与类型/谓词管理，与 kos-core 上下文配合 |

### L1: Kernel 层（动态迁移层，MTK）

| 文件 | 功能 |
|------|------|
| **include/kos_kernel.h** | 状态 σ（K, TS, P, trace, state_hash）、事件队列、`kos_state_create` / `kos_step` / `kos_kernel_step`、前置/后置验证、`kos_state_get_hash` |
| **include/kos_trace.h** | Event Log（Trace）只读视图；`kos_trace_append`、`kos_trace_replay`（确定性重放）、`kos_trace_fold_hash`、Trace 文件保存/加载 |
| **src/kernel/state_step.c** | σ 创建/释放；FIFO 事件队列；Verify(Pre)→Reduce→Update(K,TS)→追加 Trace、更新 state_hash；`kos_evolution_cycle` |
| **src/kernel/trace.c** | Trace 的创建/追加/步访问；`kos_trace_replay`（从 initial_K 重放得新 σ）；`kos_trace_fold_hash`；Trace 持久化 |
| **src/kernel/kernel_session.c** | 会话（Γ + σ + trace）；事件入队、`kos_kernel_session_step`、根因查找（调用 kos-core prove） |
| **src/kernel/kernel_scenarios.c** | 六大场景：根因追溯、反事实、合规、审计（`kos_trace_replay_into`）、治理、AI 建议验证 |
| **src/kernel/knowledge_base.c** | 知识库与物化项、依赖图 |

### L2: Runtime 层（环境演化层）

| 文件 | 功能 |
|------|------|
| **include/kos_runtime.h** | 信号源、bitstream、elaboration_record、storage_backend；`kos_elab`、`kos_materialize`、`kos_replay_from_trace` / `kos_replay_from_trace_file` |
| **src/runtime/elab.c** | 精化：原始信号 → 事件对 ⟨e,p⟩（含 .kos 字符串 / JSON 解析与类型校验） |
| **src/runtime/material.c** | 具象化 M：状态/事实写入存储后端，两阶段提交 |
| **src/runtime/replay.c** | 精化轨迹重放；从 Event Log（Trace）确定性重放与崩溃恢复；Trace 文件保存/加载 |
| **src/runtime/storage_manager.c** | 存储后端实现（内存、文件等），write/read/commit/rollback |
| **src/runtime/signal_process.c** | 信号处理入口、自动溯源、根因报告 |
| **src/runtime/scheduler_relay.c** | 信号缓冲区，多源信号有序入队 |
| **src/runtime/system_init.c** | 运行时与初始本体初始化 |

### 其他模块（选览）

| 目录/文件 | 功能 |
|-----------|------|
| **src/ontology/** | 本体注册、版本管理、影响分析、推理会话 |
| **src/domain/manufacturing/** | 制造业类型/谓词、因果索引、质量溯源、runtime_elab |
| **src/domain/finance/** | 金融领域类型与精化 |
| **src/query/** | 查询解析、执行、图索引、时间索引 |
| **src/performance/** | 查询缓存、B+ 树、倒排索引、分布式缓存 |
| **src/integration/** | 连接器注册、CSV/JSON、融合引擎 |
| **src/stream/** | 流管道与聚合 |
| **src/security/** | RBAC、审计、安全上下文 |
| **src/api/** | REST 服务与端点 |

## Core 层工具

L0 的形式化核心（类型检查、归约、证明）在 **kos-core** 中实现；本仓库通过 **src/core** 与 **include/kos_core.h** 提供与 kos-core 的对接，以及 C 侧 term 的构建、序列化与加载接口，供 Kernel/Runtime 使用：

### 类型构建器 (Type Builder)

- `kos_mk_atomic()` - 创建原子值类型
- `kos_mk_prop()` - 创建命题类型
- `kos_mk_val()` - 创建值类型
- `kos_mk_pair()` - 创建 Σ-Type 对 `<d, p>`
- `kos_mk_sigma()` - 创建依赖类型 `Σ(x:A).B`
- `kos_term_copy()` - 深拷贝 term
- `kos_term_free()` - 递归释放 term

### 存储和加载 (Storage & Loading)

- `kos_term_serialize()` - 序列化 term 为 JSON 格式
- `kos_term_deserialize()` - 从 JSON 反序列化 term
- `kos_term_save_to_file()` - 存储 term 到文件
- `kos_term_load_from_file()` - 从文件加载 term
- `kos_knowledge_save()` - 存储知识集 K 到文件
- `kos_knowledge_load()` - 从文件加载知识集 K

### 使用示例

```c
// 创建类型
kos_term* val = kos_mk_val("hello");
kos_term* prop = kos_mk_prop("IsVerified(alice)");
kos_term* pair = kos_mk_pair(val, prop);

// 序列化
kos_serialized* json = kos_term_serialize(pair);

// 存储到文件
kos_term_save_to_file(pair, "knowledge.json");

// 从文件加载
kos_term* loaded = kos_term_load_from_file("knowledge.json");

// 清理
kos_term_free(pair);
kos_serialized_free(json);
```

## 设计原则

该架构确保了"任何物理存储中的比特翻转，在逻辑层都有完整的本体证明链支持"。

**延伸阅读**：[New_Design_Plan.md](New_Design_Plan.md)（三年路线图与最小可信内核 MTK）、[docs/MTK_KERNEL_RUNTIME_ALIGNMENT.md](docs/MTK_KERNEL_RUNTIME_ALIGNMENT.md)（Kernel/Runtime 与 MTK 对齐说明）。


## 协作约定（Engineering Rules）

1. 没有关联 Issue 的 PR 不合并
2. 所有重要决策必须有 GitHub 记录
3. 文档以减少未来沟通成本为目标


## 许可证

[待添加]



---

<a name="english"></a>
## English

A "Unity of Knowledge and Action" logical framework system developed based on the design principles of "KOS-TL (Knowledge Operation System - Type Logic)".

**Documentation Index**: [docs/README_CN.md](docs/README_CN.md) (Chinese) | [docs/README_EN.md](docs/README_EN.md) (English)

## System Architecture

The system is strictly divided into three logical layers, ensuring a balance between logical rigor and engineering efficiency:

| Layer | Name | Core Responsibility | Technical Implementation Focus |
| --- | --- | --- | --- |
| **L0: Core** | **Static Truth Layer** | Defines "what is legal". Establishes type constraints based on Intuitionistic Restricted Type Theory (ITT) | Dependent type checker (Bidirectional Type Checking), proof term construction |
| **L1: Kernel** | **Dynamic Migration Layer** | Defines "how to change". Handles event-driven state evolution through small-step operational semantics | Deterministic state machine, event queue management, causal backtracking |
| **L2: Runtime** | **Environment Evolution Layer** | Defines "how to run". Handles external I/O, physical storage mapping, and signal refinement | Signal refinement operator (elab), physical storage manager (M) |

## Core Features

- **Knowledge Object Atomization (Σ-Types)**: All knowledge entering the system must exist in the form of `<d, p>`, i.e., strongly coupling data `d` with its business logic proof `p`, eliminating "rootless data"
- **Deterministic Evolution (Small-step Semantics)**: System state transitions must be driven by verified events, and non-deterministic choices are strictly prohibited
- **Computational Reflection and Endogenous Auditing (Reflexivity)**: The system must automatically synthesize equivalence proofs (Identity Proof) during each logical evolution, enabling real-time formal auditing at runtime
- **Asynchronous Refinement and Sequential Commit**: Physical signals can be refined in parallel, but the kernel must commit sequentially to ensure the uniqueness of the causal chain

## Key Operators

- **elab (Refinement Operator)**: Maps raw physical bitstreams to event objects with logical proofs `<e, p>`
- **STEP (Migration Operator)**: Drives monotonic evolution of the system from old state `σ` to new state `σ'`
- **M (Concretization Operator)**: Sinks abstract logical conclusions into physical actions (such as database ACID transactions or hardware voltage control)

## Build Instructions

### Building with CMake

```bash
# Create build directory
mkdir build
cd build

# Configure (using MinGW)
cmake -G "MinGW Makefiles" ..

# Or using Visual Studio
cmake ..

# Build
cmake --build .

# Run
./bin/kos_system.exe
```

## Project Structure

```
KosL/
├── include/                    # Public headers
│   ├── kos_core.h             # L0 bridge: C-side term and API (for kos-core interaction)
│   ├── kos_kernel.h           # L1 Kernel: state σ, queue, STEP
│   ├── kos_trace.h             # Event Log (Trace), deterministic replay
│   ├── kos_runtime.h           # L2 Runtime: elab, storage, replay
│   ├── kos_kernel_context.h   # Γ (types/predicates)
│   ├── kos_kernel_session.h   # Session: Γ + σ + trace
│   ├── kos_kernel_scenarios.h # Six scenario APIs
│   ├── kos_knowledge_base.h   # Knowledge base and dependency graph
│   ├── kos_core_bridge.h      # Bridge to kos-core (Haskell)
│   ├── kos_ontology.h         # Ontology types and predicates
│   └── ...
├── src/
│   ├── core/                  # L0 bridge to kos-core (core logic lives in kos-core/)
│   │   ├── type_builder.c     # C-side term construction for serialization and bridge
│   │   ├── storage.c          # JSON serialization; exchange data with kos-core
│   │   ├── universe.c        # C-side Universe representation
│   │   ├── core_kos_adapter.c # .kos syntax ↔ C-side term
│   │   ├── kos_core_bridge.c  # Invoke kos-core executable (check-term, prove, etc.)
│   │   ├── ontology_manager.c
│   │   └── visualization.c
│   ├── kernel/                # L1 Kernel (MTK-aligned)
│   │   ├── state_step.c       # σ, event queue, STEP, state_hash
│   │   ├── trace.c            # Event Log, kos_trace_replay, persist
│   │   ├── kernel_context.c   # Γ
│   │   ├── kernel_session.c   # Session: enqueue, step, root cause
│   │   ├── kernel_scenarios.c # Six scenarios
│   │   ├── knowledge_base.c
│   │   └── event_op_registry.c
│   ├── runtime/               # L2 Runtime
│   │   ├── elab.c             # Refinement: signal → ⟨e,p⟩
│   │   ├── material.c        # Concretization M, two-phase commit
│   │   ├── replay.c          # Replay from Trace, crash recovery
│   │   ├── storage_manager.c # Storage backends
│   │   ├── scheduler_relay.c  # Signal buffer, ordered commit
│   │   ├── signal_process.c  # Signal handling, traceability
│   │   ├── system_init.c
│   │   └── signal_source_registry.c
│   ├── ontology/              # Ontology versioning and reasoning
│   ├── domain/                # manufacturing, finance
│   ├── query/                 # Query engine
│   ├── performance/           # Cache, indexes, B+ tree
│   ├── integration/           # Data integration
│   ├── stream/                # Stream processing
│   ├── security/              # RBAC, audit
│   ├── api/                   # REST
│   ├── utils/
│   └── visualization/
├── examples/                  # Demos (kernel_example, quality_traceability_demo, ...)
├── tools/                     # Visualization and code generation
├── docs/                      # Design docs (MTK_KERNEL_RUNTIME_ALIGNMENT.md, ...)
├── kos-core/                  # L0 Core implementation (Haskell): type check, reduction, prove
├── kos-web/                   # Web showcase (FastAPI + React)
├── compiler/                  # KOS-TL compiler
├── monograph/                 # LaTeX and knowledge graph HTML
├── main.c
├── CMakeLists.txt
├── New_Design_Plan.md         # Roadmap and MTK design
└── README.md
```

## Core Files (Summary)

| Layer | Key files | Role |
|-------|-----------|------|
| **L0 Core** | **kos-core/** (Haskell) = core implementation (type check, reduction, prove). **src/core/** = bridge: `kos_core.h`, `type_builder.c`, `storage.c`, `kos_core_bridge.c` for C-side terms and calling kos-core | Core logic in kos-core; C side bridges and exchanges data with it |
| **L1 Kernel** | `kos_kernel.h`, `kos_trace.h`, `state_step.c`, `trace.c`, `kernel_session.c`, `kernel_scenarios.c` | State σ, Event Log (Trace), STEP, state_hash, deterministic replay, six scenarios |
| **L2 Runtime** | `kos_runtime.h`, `elab.c`, `material.c`, `replay.c`, `storage_manager.c`, `signal_process.c` | Refinement (elab), concretization (M), replay from Trace, crash recovery |
| **Other** | `ontology/`, `domain/manufacturing|finance/`, `query/`, `performance/`, `integration/`, `stream/`, `security/`, `api/` | Ontology, domains, query, cache, integration, stream, security, REST |

## Core Layer Tools

The formal Core (type checking, reduction, proof) is implemented in **kos-core**. This repo provides the **bridge** via **src/core** and **include/kos_core.h**: C-side term construction, serialization, and loading for use by Kernel/Runtime:

### Type Builder

- `kos_mk_atomic()` - Create atomic value type
- `kos_mk_prop()` - Create proposition type
- `kos_mk_val()` - Create value type
- `kos_mk_pair()` - Create Σ-Type pair `<d, p>`
- `kos_mk_sigma()` - Create dependent type `Σ(x:A).B`
- `kos_term_copy()` - Deep copy term
- `kos_term_free()` - Recursively free term

### Storage and Loading

- `kos_term_serialize()` - Serialize term to JSON format
- `kos_term_deserialize()` - Deserialize term from JSON
- `kos_term_save_to_file()` - Save term to file
- `kos_term_load_from_file()` - Load term from file
- `kos_knowledge_save()` - Save knowledge set K to file
- `kos_knowledge_load()` - Load knowledge set K from file

### Usage Example

```c
// Create types
kos_term* val = kos_mk_val("hello");
kos_term* prop = kos_mk_prop("IsVerified(alice)");
kos_term* pair = kos_mk_pair(val, prop);

// Serialize
kos_serialized* json = kos_term_serialize(pair);

// Save to file
kos_term_save_to_file(pair, "knowledge.json");

// Load from file
kos_term* loaded = kos_term_load_from_file("knowledge.json");

// Cleanup
kos_term_free(pair);
kos_serialized_free(json);
```

## Design Principles

This architecture ensures that "any bit flip in physical storage has complete ontological proof chain support at the logical layer."

**See also**: [New_Design_Plan.md](New_Design_Plan.md) (roadmap and MTK), [docs/MTK_KERNEL_RUNTIME_ALIGNMENT.md](docs/MTK_KERNEL_RUNTIME_ALIGNMENT.md) (Kernel/Runtime alignment).

## Collaboration Guidelines (Engineering Rules)

1. Pull requests without an associated issue will not be merged.
2. All significant decisions must be recorded on GitHub.
3. Documentation should aim to reduce future communication costs.


## License

[To be added]

