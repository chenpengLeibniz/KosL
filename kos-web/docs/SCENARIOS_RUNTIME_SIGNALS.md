# 六大核心场景与 Runtime 层信号对应关系

本文档说明在 kos-web 中，**六大核心场景**各自在 **Runtime 层**应构建或依赖哪些信号（signal kind），以便正确驱动演示与推理。并说明 **宪法约束（Γ）**、**自动演化** 与 **回放** 的用法。

---

## 宪法约束与 Γ（必读）

- **对象创建必须通过 Core 层（Γ）**：仅当事件类型在 **Γ** 中定义时，信号才可入队、小步才可写入知识集 K。Γ 由 **Core 加载** 注入（粘贴 KOS、导入 .kos、单条类型定义），加载后自动同步到 Kernel。
- **Γ 为空时**：Runtime「生效」无法入队任何信号；Kernel 无法创建任何对象。使用前请先在 **Core 加载** 中加载包含事件类型（如 ProcStep、Anomaly、FailEvt、Sensor、QcFailure）的 KOS 定义。
- 详见 [CORE_CONSTITUTION.md](CORE_CONSTITUTION.md)。

---

## 自动演化与回放

- **自动演化**（Runtime 页「自动演化」区块）：用户指定 **序列长度**（如 5～50），系统自动生成该长度的信号序列，将能精化为 Γ 中事件类型的信号入队，并自动执行小步直至队列空或达到步数上限。结果可 **保存为日志**，便于回放与合规审计；可跳转 **Kernel** 或 **场景（审计/合规）**。
- **回放**：在「日志与重放」中可选择 **重放**（仅将日志中的信号重新入队）或 **重放并演化**（入队后自动执行小步直至队列空）。重放后可在 Kernel 查看队列与轨迹，或在 **场景 → 审计与问责 / 合规性决策** 做合规审计。

---

## 信号类型速查（Runtime → Kernel）

| signal kind   | 精化后事件类型 | 说明 |
|---------------|----------------|------|
| `proc_step`   | ProcStep       | 生产步骤：批次在机器上加工（batch_id, machine, start_time, end_time） |
| `anomaly`     | Anomaly        | 设备异常：温控、电压等（machine, param, value, timestamp） |
| `fail_evt`    | FailEvt        | 质检失效事件（batch_id, error_code, timestamp） |
| `sensor`      | Sensor         | 传感器读数（name, value, unit, ts） |
| `qc_failure`  | QcFailure      | 质检不合格（batch, defect, severity, ts） |

---

## 场景 1：根因追溯（Root Cause）

- **含义**：给定失效事件，由 proof 构造 RootCauseReport（FailEvt + Anomaly + CausalProof）。
- **Runtime 应构建**：
  - **至少一条 `fail_evt`**：在 Kernel 执行一步消费该条时，会自动触发 `scenario_root_cause()`。
  - **完整因果链演示**：按时间顺序生成并生效：
    1. **proc_step**（如 08:00–09:30 某批次在 M_03 加工）
    2. **anomaly**（如 08:15 M_03 电压波动）
    3. **fail_evt**（如 10:00 该批次 HARD_ERR）
  - 这样轨迹中既有 ProcStep、Anomaly，又有 FailEvt，根因证明与前端时间线一致。
- **操作**：Runtime 生成上述信号 → 生效入队 → Kernel 执行一步（消费 fail_evt 时自动跑根因）或执行至队列空。

---

## 场景 2：反事实推理（Counterfactual）

- **含义**：对比「事实 ctx（含 anomaly）」与「反事实 ctx（无 anomaly）」下 RootCauseReport 是否可证，判断 anomaly 对根因是否因果必要。
- **应用环境**：前端提供**反事实推理应用环境**入口（导航「反事实推理」或 `/counterfactual`），可查看事实/反事实两个上下文的说明、建议信号链，并一键执行对比（prove 两个 .kos 上下文）。
- **Runtime 应构建**：
  - **不直接消费 Runtime 信号**：场景仅对两个预置 .kos 文件做 prove 对比。
  - 若需「构造反事实情境」做扩展演示：可生成两条轨迹——一条含 **anomaly**，一条不含（仅 **proc_step** + **fail_evt**），分别入队演化后对比审计或根因结果。

---

## 场景 3：合规性决策（Compliance Check）

- **含义**：在摄入前检查单条信号是否合规（当前实现：仅检查 kind 是否在允许列表中）。
- **应用环境**：前端提供**合规性决策应用环境**入口（导航「合规性决策」或 `/compliance`），可查看当前合规规则、合规/不合规类型，并用 Runtime 生成的信号一键执行检查（合规：proc_step / anomaly / fail_evt；不合规：sensor / qc_failure 等）。
- **Runtime 应构建**：
  - **合规示例**：任意一条 **proc_step**、**anomaly** 或 **fail_evt**（当前合规逻辑仅允许此三种）。
  - **不合规示例**：一条 `kind` 不在上述三种的 signal（如 **sensor**、**qc_failure** 或自定义 kind），用于演示“拒绝”。
  - 场景页当前实现：点击运行会先 `generateSignal('proc_step')` 再调用合规检查，故默认构建的是 **proc_step**。

---

## 场景 4：审计与问责（Audit Trail）

- **含义**：导出当前 Kernel 轨迹为审计 JSON（trace 中每步的 step_index、ts_after、event_label、event_pair）。
- **应用环境**：导航「场景」→「审计与问责」（`/audit`），可查看说明并执行审计导出、下载 JSON。
- **Runtime 应构建**：
  - **任意可入队的信号组合**：**proc_step**、**anomaly**、**fail_evt**、**sensor**、**qc_failure** 均可。
  - 先由 Runtime 生成若干条并「生效」入队，再在 Kernel 执行若干步形成轨迹，最后在场景页运行「审计」即可得到非空的 audit.trace。
  - 建议：至少 2～3 条不同类型（如 1 proc_step + 1 anomaly + 1 fail_evt）以便审计内容有代表性。

---

## 场景 5：复杂系统治理 / 演化直至空闲（Evolve Idle）

- **含义**：从 Runtime 模拟器取最近 50 条信号，逐条 ingest 直至队列清空，统计步数与消费数。
- **应用环境**：导航「场景」→「复杂系统治理」（`/governance`），可查看说明并执行演化直至空闲。
- **Runtime 应构建**：
  - **在「生成」或「从历史生成」中准备多类信号**：**proc_step**、**anomaly**、**fail_evt**、**sensor**、**qc_failure**（可批量生成或从轨迹/日志加载）。
  - 点击「生效」将信号入队到 Kernel 的 P，再在场景页点击「演化直至空闲」；场景内部会调用 `get_simulator().get_latest(50)` 并逐条 `ingest_signal`（注意：当前实现是消费模拟器 buffer，与「已入队到 P」可能不是同一批，取决于实现细节；若需严格一致，可先「生效」再在 Kernel 侧执行 step_drain，或由场景改为从 P 消费）。
  - 为充分演示，建议至少 5～10 条混合类型信号。

---

## 场景 6：AI 治理 / 类型校验（Verify AI）

- **含义**：校验给定项（term_expr）是否满足给定类型（type_expr），调用 kos-core check-term。
- **应用环境**：导航「场景」→「AI 治理」（`/ai-governance`），可输入 term/type 并执行类型校验。
- **Runtime 应构建**：
  - **不需要 Runtime 信号**：输入仅为 term_expr 与 type_expr 字符串，与事件队列、轨迹无关。
  - 场景页通常写死示例（如 `Prop P` / `Prop P`）用于演示。

---

## 汇总表

| 场景           | Runtime 需构建的信号 | 说明 |
|----------------|----------------------|------|
| 1 根因追溯     | **fail_evt**（必）；完整链：**proc_step** → **anomaly** → **fail_evt** | 消费 fail_evt 时自动跑根因；因果链需先有步骤与异常 |
| 2 反事实推理   | 无（或两条轨迹：含/不含 **anomaly**） | 主要用预置 .kos 做 prove 对比 |
| 3 合规性决策   | 单条 **proc_step** / **anomaly** / **fail_evt**（合规）或非法 kind（不合规） | 当前场景页默认生成 proc_step |
| 4 审计与问责   | 任意 **proc_step**、**anomaly**、**fail_evt**、**sensor**、**qc_failure** 若干条 | 先入队并执行步，再跑审计导出轨迹 |
| 5 演化直至空闲 | **proc_step**、**anomaly**、**fail_evt**、**sensor**、**qc_failure** 混合多条 | 由场景消费模拟器 buffer 或先入队再 drain |
| 6 AI 类型校验  | 无 | 仅 term/type 表达式，不依赖信号 |

上述信号均在 Runtime 层通过「生成」「从历史生成」或「自动演化」得到；**仅当精化后的事件类型在 Γ 中**时才会被「生效」入队，入队后在 Kernel 层参与小步演化与各场景引擎。自动演化与回放支持将生成的序列保存为日志并用于合规审计。
