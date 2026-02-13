# KOS-TL 功能与 kos-web 展示对照

本文档对照 KOS-TL 系统能力与 kos-web 当前实现，标出已在 Web 体现的功能与可增强的展示/交互点。

## 一、KOS-TL 核心能力概览（来自代码库）

| 层次/模块 | 能力摘要 | 对应 C/示例 |
|-----------|----------|-------------|
| **L0 Core** | 类型/谓词/构造函数；KOS 语法解析；与 kos-core 桥接 | kos_core.h, core_loader.py, quality_traceability.kos |
| **Runtime** | 信号模拟（proc_step/anomaly/fail_evt/sensor/qc_failure）；信号入队；日志保存与重放 | kos_signal_simulate.h, signal_simulate.c, sensor_simulator.py |
| **Kernel** | Γ、σ(K)、事件队列 P；小步演化；精化（信号→事件对）；按事件类型自动根因/审计 | kos_kernel.h, state_step.c, kernel_engine.py |
| **Trace** | 轨迹 T = σ₀→^⟨e,p⟩σ₁→…；确定性重放；轨迹保存/加载 | kos_trace.h, trace.c |
| **场景** | 根因追溯、反事实、合规、审计、演化至空闲、AI 校验 | all_scenarios_demo.c, scenarios.py |
| **知识图谱** | Γ 与 σ 轨迹的可视化（类型/谓词/构造/项） | graph_data.py, KnowledgeGraph.jsx |

## 二、kos-web 当前已体现的功能

### 2.1 Core 层
- **类型 / 谓词 / 构造函数**：从 API 拉取并展示（Core 页）。
- **Core 加载**：粘贴 KOS 文本或上传 .kos 文件，解析后写入 Core 并同步到 Kernel Γ（CoreLoad 页）。
- **清空动态**：清空通过加载注入的类型与谓词。

### 2.2 Runtime 层
- **信号生成**：单条（按 kind）、批量、随机；kind 含 proc_step / anomaly / fail_evt / sensor / qc_failure。
- **生效**：将当前信号缓冲送入 Kernel 事件队列 P。
- **实时录制**：开始/停止录制；生效时自动追加到录制缓冲；停止时保存为一条日志。
- **日志**：保存当前缓冲为日志、日志列表（含条数、创建时间）、按 id 重放（重新入队到 P）。

### 2.3 Kernel 层
- **状态**：Γ（类型/谓词）、σ（K）、队列 P 预览、逻辑时钟 TS、轨迹长度。
- **执行一步**：从 P 取一条 → 精化 → 小步演化 → 按事件类型自动跑根因/审计，结果写入该步的 scenario_result。
- **重置 σ**：清空 K、P、轨迹，Γ 从 Core 重新同步。
- **轨迹**：在 Kernel 页展示最近步的 event_label 与 scenario_result（根因/审计）。
- **物化**：展示已物化到 Runtime 的项列表。

### 2.4 轨迹（Trace）页
- 从 API 拉取完整 trace，展示步号、TS、event_label、event_pair.data。
- **缺口**：未展示 scenario_result（根因/审计摘要、proof_term 等）；无刷新按钮。

### 2.5 知识图谱
- 按轨迹步滑块过滤可见节点/边；图数据来自 graph_data.py 的静态 NODES/EDGES。
- **缺口**：图谱与 Core 加载的 Γ 未联动，为固定示例拓扑。

### 2.6 场景（Scenarios）页
- 六大场景均有「执行」按钮与结果展示：根因、反事实、合规、审计、演化至空闲、AI 校验。
- 审计场景返回 audit JSON，但**未提供下载**。

### 2.7 概览（Dashboard）
- 展示 Core 类型数、Runtime 最近信号数、Kernel 的 TS/|K|/轨迹步数；链接到 Core、Runtime、Kernel、图谱、轨迹、场景。
- **缺口**：无「Core 加载」直达入口、无日志数量、无推荐操作流程说明。

## 三、可增强的展示与交互

| 项 | 说明 | 实现建议 |
|----|------|----------|
| **Trace 页展示 scenario_result** | 每步若有根因/审计结果，展示 summary、proof_term 等 | Trace.jsx 渲染 trace[].scenario_result，并增加刷新按钮 |
| **Kernel「执行到队列空」** | 连续执行多步直到 P 为空，便于一次性消化重放日志 | 后端 `POST /api/kernel/step_drain`；Kernel 页增加「执行至队列空」按钮 |
| **Dashboard 增强** | 突出 Core 加载、日志数、推荐流程 | 增加「Core 加载」链接、从 API 取日志数、简短「推荐流程」文案 |
| **知识图谱与 Γ 联动** | 图谱目前为静态示例 | 短期：在图谱页注明「示例拓扑」；后续可增加「从 Γ 生成」的 API 与简单拓扑展示 |
| **审计轨迹下载** | 审计场景结果可导出为文件 | Scenarios 页审计结果区增加「下载审计 JSON」按钮 |
| **Trace 页刷新** | 用户执行步后希望看到最新轨迹 | Trace 页增加刷新按钮，并可选定时/手动刷新 |
| **场景与当前 Kernel 状态** | 根因/审计已用当前 trace；合规可「用队列首条」 | 合规场景可增加「使用当前队列第一条」的快捷入口（可选） |

## 四、KOS-TL 中尚未在 Web 暴露的能力（可选扩展）

- **C 层 trace 文件**：`kos_trace_save_to_file` / `kos_trace_load_from_file`、确定性重放与 state hash（MTK 设计）。若后端接入 C 运行时，可增加「轨迹导出/导入」与「按 hash 校验重放」。
- **多本体 / 领域**：ontology_registry、manufacturing/finance 等；Web 当前为单 Core 动态库，可后续增加「领域/本体选择」。
- **查询引擎**：kos_query.h；可在 Web 增加「查询」入口与简单查询表单。
- **因果追溯索引**：causal_index、causal_trace；若后端有对应 API，可在 Trace/场景结果中展示因果链。

## 五、小结

- **已较好覆盖**：Core 加载与展示、Runtime 信号生成与日志/重放、Kernel 状态与小步/轨迹/物化、六大场景的触发与结果展示、知识图谱按步浏览。
- **建议优先补齐**：Trace 页展示 scenario_result 与刷新、Kernel「执行至队列空」、Dashboard 的 Core 加载与推荐流程、审计结果下载。
- **后续可做**：图谱从 Γ 派生、轨迹导出/重放与 state hash、查询与多本体入口。

上述优先项已在实现中或列入实现清单（见本仓库 kos-web 与 backend 对应提交）。
