# 最小可信内核（MTK）与 Kernel/Runtime 层对齐设计

本文档根据 `New_Design_Plan.md` 中的 **最小可信 KOS 内核（Minimal Trusted KOS Kernel, MTK）** 设计，说明当前 Kernel 层与 Runtime 层的对应关系、已满足的性质，以及为实现 MTK 所做的改进。

---

## 1. MTK 核心原则回顾

1. 所有状态变化必须由 **Event** 驱动  
2. **Event** 不可修改  
3. **Replay** 必须 **deterministic**（同样 Trace + 同样初始 σ ⇒ 同样 σ'）  
4. **类型系统** 在 Event 提交前校验  
5. **Kernel** 极小、可审计  

---

## 2. MTK 最小模块与当前实现对应

| MTK 模块        | 当前实现位置 | 说明 |
|-----------------|--------------|------|
| **Type Checker** | `kos_verify_precondition()`（`state_step.c`）| 在 `kos_kernel_step` 内，先 Verify(Pre) 再执行；Pre 通过 Core 层 `kos_type_check(K, p, e)` 校验 |
| **Kernel Reducer** | `kos_reduce(e)`（`state_step.c`）| 单步内对事件项 e 做 β/ι 归约，再写回 K |
| **State Store** | `kos_state_t`（K, TS, P）+ 可选 schema | K 为知识集（Σ 链），TS 逻辑时钟，P 为待处理事件队列；新增 `state_hash` 满足 hash(σ) = fold(trace) |
| **Event Log**   | `kos_trace_t`（`trace.c`）| **不可变** 追加序列：每步成功后仅 `kos_trace_append`，不修改已有步；Trace = List<Event>，即事件序列 |

---

## 3. 数据模型对齐

### 3.1 State（MTK）

```
State σ := { store : Map<Key, Value>, schema : Schema, version : Hash }
```

### 3.2 当前 State（Kernel）

- **store** → 知识集 `K`（`kos_term*`，Σ 链形式，可视为逻辑 store）  
- **schema** → 由类型系统与本体约束体现（可选：后续可加显式 schema 指针）  
- **version** → 新增 **state_hash**（uint64_t）：由 Trace 按序折叠计算，满足 **State Hash Stability**  

### 3.3 Event（MTK）

```
Event := { id, precond, transform, meta }
```

### 3.4 当前 Event（Kernel）

- 事件对 `⟨e, p⟩`：**e** 为事件项，**p** 为证明；**precond** = 在当前 K 下 `kos_type_check(K, p, e)`；**transform** = 归约后更新 K 与 TS；**id** 可由步序或逻辑时钟体现；**meta** 可为扩展字段。  
- Event 一旦追加到 Trace，**不可修改**（Trace 只追加不改写）。  

### 3.5 Trace（MTK）

```
Trace := List<Event>
```

### 3.6 当前 Trace（Kernel）

- `kos_trace_t`：`steps[]` 为事件步序列，每步包含 `event_pair`（及可选 K 快照）。  
- **不可变**：仅通过 `kos_trace_append` 追加，不提供对历史步的修改接口。  
- 执行语义：σ₀ →^⟨e₁,p₁⟩ σ₁ →^⟨e₂,p₂⟩ σ₂ → …  

---

## 4. 执行模型与内核可信性

### 4.1 执行流程（MTK）

```
σ₀ ──e₁──▶ σ₁ ──e₂──▶ σ₂ ──e₃──▶ σ₃
Replay: σ₀ + [e₁,e₂,e₃] → σ₃
```

### 4.2 当前实现

- **单步**：`kos_kernel_step(σ, event_pair)`：  
  `if precond(σ)` → `σ' = transform(σ)`（Reduce + Update K/TS）→ 追加 Event Log（trace）→ return true；  
  `else` → reject，不修改 σ，不追加 log。  
- **无隐藏副作用**：状态变更仅通过上述路径，且类型检查在提交前完成。  

### 4.3 Replay 确定性

- 新增 **kos_trace_replay(trace, initial_K)**：  
  从 `σ₀ = (initial_K, TS=0, 空队列)` 开始，按 Trace 顺序依次对每步的 `event_pair` 调用 `kos_kernel_step(σ, event_pair)`，得到最终 σ。  
- **定理（Determinism）**：同一 Trace + 同一初始 K ⇒ 同一最终 σ（由单步确定性与顺序执行保证）。  

---

## 5. 必须满足的核心性质

| 性质 | 实现方式 |
|------|----------|
| **Determinism** | 同一 Trace + 同一 σ₀，仅通过 `kos_trace_replay` 或等价顺序 step 得到唯一 σ'；单步内无随机与外部副作用。 |
| **Replay Completeness** | 任意当前 σ 均可由某 Trace 从 σ₀ 重放得到（Trace 在每步成功时追加，故可复现）。 |
| **State Hash Stability** | `state_hash(σ) = fold(hash, trace)`：在每次 `kos_trace_append` 后更新 `sigma->state_hash`；重放后可验证 `state_hash(replayed_σ) == expected_hash`。 |

---

## 6. Runtime 层对齐

- **Event Log 持久化**：Trace 可序列化/反序列化（见 `kos_save_elaboration_trajectory` / `kos_load_elaboration_trajectory` 及基于 Trace 的保存/加载），便于崩溃后恢复。  
- **Crash 恢复**：从持久化的 **Event Log（Trace）** 加载，用 **kos_trace_replay** 从 σ₀ 重放，得到一致状态；可选再验证 state_hash。  
- **精化轨迹**：`elaboration_record_t` 保留“信号 → 事件对”的映射，用于审计与信号级重放；**确定性重放** 以 **Trace（Event Log）** 为准。  

---

## 7. 接口与文件变更摘要

- **Kernel**  
  - `kos_state_t`：新增 `state_hash`（uint64_t），可选 `schema` 预留。  
  - `kos_state_get_hash` / `kos_state_update_hash`：读取与按 Trace 更新 state_hash。  
  - Trace 作为 **Event Log**：仅追加、不修改；文档明确“不可变”。  
  - **kos_trace_replay**：从初始 K 和 Trace 确定性重放得到最终 σ。  
  - **kos_trace_hash_step**：用于折叠计算 state_hash（内部或公开）。  

- **Runtime**  
  - **kos_replay_from_trace**：基于 `kos_trace_replay`，从 Trace（含从文件加载）重放，得到 σ；可选 state_hash 校验。  
  - 崩溃恢复流程：加载 Event Log → kos_trace_replay(σ₀) → 可选验证 state_hash。  

---

## 8. 与 New_Design_Plan 的对应关系

- **第一年目标**：Deterministic Event-Sourced Knowledge Kernel → 通过 **Event Log（Trace）+ 类型检查前置 + 确定性 Replay + State Hash** 实现。  
- **工程交付物**：事件日志不可变（Trace 只追加）、Replay 机制（kos_trace_replay）、State hash 可验证、Crash 后可恢复一致状态（从 Trace 重放）。  
- **理论沉淀**：Operational semantics 对应小步规则 σ ⊢ e ⇓ σ'；Determinism / Invariant Preservation / Replay Soundness 与当前实现及上述性质一致，可在文档中明确对应到定理 1–3。  

以上为 MTK 与 Kernel/Runtime 层的对齐设计与实现要点。
