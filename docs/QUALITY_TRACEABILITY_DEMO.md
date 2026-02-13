# KOS-TL 质量追溯应用示例

本文档说明 `examples/quality_traceability_demo.c` 应用示例，展示 KOS-TL 在制造业质量异常追溯中的完整应用流程。

## 应用背景

参考 Kos.pdf Section 7: Application of KOS-TL

**核心挑战**：
> 当某批次产品出现严重质量缺陷时，系统能否自动追溯其生产过程，识别与设备、人员或原材料相关的潜在异常，并产生可执行和可解释的因果链？

**问题特征**：
- 异构数据源（工单、设备日志、人员排班、质检记录）
- 强时间顺序和因果约束
- 推理结果必须直接支持生产决策和责任归属

## 应用场景：轴承生产质量追溯

### 场景描述

**批次**：`Batch_202310-01`

**事件序列**：
1. **09:50** - 设备 `HeatTreatment_03` 发生电压异常（220.5V，正常范围 218-222V）
2. **10:00** - 批次 `Batch_202310-01` 发生硬度错误（`HARD_ERR`）

**问题**：系统需要自动识别电压异常是否为硬度错误的根本原因。

## 三层架构协作流程

### Phase 1: Core Layer - 类型定义

**目标**：定义"什么是合法的"（静态逻辑域）

**操作**：
1. 定义基础原子类型：`BatchID`, `Machine`, `Time`, `ErrorCode`, `ParamName`, `Value`
2. 定义失败事件类型：`FailEvt : Σ(b:BatchID). Σ(err:ErrorCode). Σ(t:Time). Prop`
3. 定义异常事件类型：`AnomalyEvt : Σ(m:Machine). Σ(p:ParamName). Σ(v:Value). Σ(t:Time). Prop`

**代码**：
```c
kos_term* fail_evt_type = create_failure_event_type();
kos_term* anomaly_evt_type = create_anomaly_event_type();
```

**说明**：
- Core 层定义类型构造和约束，不涉及时间变化
- 使用依赖类型（Σ）强制数据与证明的耦合

---

### Phase 2: Runtime Layer - 信号精化

**目标**：将物理比特流映射为带逻辑证明的事件对象（双向映射）

**操作**：
1. 捕获原始信号：
   - 失败信号：`"Batch_202310-01|HARD_ERR|1697004000"`
   - 异常信号：`"HeatTreatment_03|voltage|220.5|1697001000"`

2. 使用 `elab` 算子精化信号：
   - `kos_elab(raw_signal, ontology)` → `<e, p>`
   - 如果无法构造证明，返回 `NULL`（逻辑防火墙）

**代码**：
```c
bitstream failure_signal = create_raw_failure_signal();
kos_term* failure_event_pair = kos_elab(failure_signal, initial_ontology);

if (!failure_event_pair) {
    // 信号被逻辑防火墙拒绝
    return 1;
}
```

**说明**：
- Runtime 层建立逻辑语义与物理资源的双向映射
- 所有输入必须通过 `elab` 算子，确保合法性

---

### Phase 3: Kernel Layer - 状态演化

**目标**：确定性状态演化（Peek-Verify-Reduce-Confirm）

**操作**：
1. 创建初始状态：`σ = 〈K, TS=0, P=∅〉`
2. 将事件加入队列（严格顺序）：
   - 先加入异常事件（09:50）
   - 后加入失败事件（10:00）
3. 执行状态演化循环：
   - **Peek**：查看队列头部事件
   - **Verify**：验证前置条件 `Pre(e)`
   - **Reduce**：执行归约操作
   - **Confirm**：验证后置条件 `Post(e)` 并更新状态

**代码**：
```c
kos_state_t* sigma = kos_runtime_init(initial_ontology);

kos_queue_enqueue(sigma->P, anomaly_event_pair);
kos_queue_enqueue(sigma->P, failure_event_pair);

while (!kos_queue_is_empty(sigma->P)) {
    bool success = kos_step(sigma);
    // 处理演化结果
}
```

**说明**：
- Kernel 层确保因果链的唯一性（严格顺序提交）
- 逻辑时钟 `TS` 单调递增
- 知识集 `K` 单调增长：`K' = Σ(new_fact, K)`

---

### Phase 4: 因果推理和根因分析

**目标**：构造因果链并验证有效性

**操作**：
1. 提取事件的时间、空间、批次信息
2. 验证因果有效性：
   - **时间约束**：异常时间 < 失败时间（`09:50 < 10:00`）
   - **空间约束**：异常设备与失败批次相关
   - **批次约束**：异常和失败属于同一批次

3. 生成根因报告：
   - 包含完整的因果链
   - 包含逻辑证明（可追溯的证明项）

**代码**：
```c
bool causal_valid = verify_causal_validity(anomaly_instance, failure_instance);

if (causal_valid) {
    // 生成根因报告
    printf("Root Cause: Voltage anomaly on HeatTreatment_03\n");
    printf("Evidence: Temporal and spatial correlation verified\n");
}
```

**说明**：
- 因果推理不是简单的数据查询，而是逻辑证明的构造
- 生成的报告包含可追溯的证明链，支持端到端的可解释性

---

### Phase 5: 物化到物理存储

**目标**：将逻辑结论下沉为物理动作（原子提交栅栏）

**操作**：
1. 创建存储后端（内存/文件/数据库）
2. 执行物化操作：
   - **Phase 1: Prepare** - 准备写入
   - **Phase 2: Commit** - 确认提交
3. 只有物理写入成功后，逻辑演化才算完成

**代码**：
```c
storage_backend_t* backend = kos_storage_create(STORAGE_BACKEND_MEMORY, NULL);
int result = kos_materialize(sigma, backend);
kos_storage_free(backend);
```

**说明**：
- 实现原子提交栅栏，确保逻辑与物理的一致性
- 只有物理层返回 ACK 后，逻辑时钟才正式前进

---

## 关键设计原则体现

### 1. 逻辑防火墙（Input Filtering）

**体现**：
- Runtime 层的 `kos_elab` 算子
- 无法构造证明的信号被拒绝（返回 `NULL`）

**效果**：
- 确保 Kernel 层不被意外信号污染
- 所有进入系统的数据都有形式化的合法性基础

### 2. 确定性归约（Deterministic Reduction）

**体现**：
- Kernel 层的 `kos_step` 算子
- 相同初始状态和事件序列产生唯一的知识视图

**效果**：
- 系统行为可预测、可重现
- 支持轨迹重放和灾后自愈

### 3. 闭环演化（Closed-Loop Evolution）

**体现**：
- 执行前验证 `Pre(e)`
- 执行后验证 `Post(e)`

**效果**：
- 系统始终在"被证明为真"的状态之间转换
- 消除逻辑真空期

### 4. 因果追溯（Causal Traceability）

**体现**：
- 知识集使用 Σ 链结构：`K = Σ(fact_n, Σ(fact_{n-1}, ...))`
- 每个事实都携带证明和时间戳

**效果**：
- 支持完整的因果链追溯
- 每个决策都有可追溯的逻辑证明

---

## 运行示例

### 编译

```bash
cmake -B build
cmake --build build
```

### 运行

```bash
./build/bin/quality_traceability_demo
```

### 预期输出

```
========================================
KOS-TL Quality Traceability Demo
Manufacturing: Bearing Production
========================================

=== Phase 1: Core Layer - Type Definitions ===
[Core] Creating base types...
  - BatchID, Machine, Time, ErrorCode, ParamName, Value
[Core] Creating FailureEvent type...
  - FailEvt : Σ(b:BatchID). Σ(err:ErrorCode). Σ(t:Time). Prop
[Core] Creating AnomalyEvent type...
  - AnomalyEvt : Σ(m:Machine). Σ(p:ParamName). Σ(v:Value). Σ(t:Time). Prop

=== Phase 2: Runtime Layer - Signal Elaboration ===
[Runtime] Creating raw failure signal...
  - Raw signal: Batch_202310-01|HARD_ERR|1697004000
  ✓ Failure signal elaborated successfully
[Runtime] Creating raw anomaly signal...
  - Raw signal: HeatTreatment_03|voltage|220.5|1697001000
  ✓ Anomaly signal elaborated successfully

=== Phase 3: Kernel Layer - State Evolution ===
  Initial state created (TS=0)
  ✓ Anomaly event enqueued
  ✓ Failure event enqueued
  Queue size: 2

  Executing state evolution...
  Step 1: SUCCESS (TS=1, Queue=1)
  Step 2: SUCCESS (TS=2, Queue=0)

=== Phase 4: Causal Reasoning and Root Cause Analysis ===
[Kernel] Creating failure event instance...
  - Instance: Batch=Batch_202310-01, Error=HARD_ERR, Time=1697004000
[Kernel] Creating anomaly event instance...
  - Instance: Machine=HeatTreatment_03, Param=voltage, Value=220.5, Time=1697001000
[Kernel] Verifying causal validity...
  - Anomaly event valid: Yes
  - Failure event valid: Yes

  Causal Chain Analysis:
  - Anomaly: HeatTreatment_03 voltage=220.5V at 09:50
  - Failure: Batch_202310-01 HARD_ERR at 10:00
  - Temporal: 09:50 < 10:00 ✓
  - Causal validity: VALID

  Root Cause Report:
  ========================================
  Batch: Batch_202310-01
  Failure: HARD_ERR at 10:00
  Root Cause: Voltage anomaly on HeatTreatment_03
  Evidence:
    - Anomaly occurred at 09:50 (before failure)
    - Machine: HeatTreatment_03
    - Parameter: voltage = 220.5V (out of normal range 218-222V)
  Causal Proof: ✓ Valid temporal and spatial correlation
  ========================================

=== Phase 5: Materialization to Physical Storage ===
  ✓ Knowledge materialized successfully

=== Cleanup ===
  Resources freed

========================================
Demo completed successfully!
========================================
```

---

## 与 Kos.pdf 的对应关系

| Kos.pdf 章节 | 示例代码对应 |
|-------------|------------|
| Section 7.1: Application Background | `quality_traceability_demo.c` 应用场景 |
| Section 7.2: The Application Workflow | Phase 1-5 完整流程 |
| Section 7.3: Kernel Layer Rule Definitions | `create_failure_event_type()` 等 |
| Section 7.4: Runtime Layer Elaboration | `kos_elab()` 信号精化 |
| Section 7.5: Causal Reasoning | `verify_causal_validity()` 因果验证 |
| Section 7.6: Logic as the System Kernel | 三层架构协作 |

---

## 扩展方向

1. **跨域审计**：
   - 结合金融域，当质量损失超过发票金额 20% 时触发审计锁定
   - 实现 `analyzeAudit()` 函数

2. **反事实推理**：
   - 实现 `Contrib(a, f)` 验证：移除异常 `a` 后，失败 `f` 是否仍可推导
   - 支持多因分析

3. **轨迹重放**：
   - 使用 `kos_replay_elaboration_trajectory()` 实现灾后自愈
   - 保存和加载精化轨迹

4. **更复杂的因果链**：
   - 支持多级因果链（异常 → 中间事件 → 失败）
   - 支持多因一果、一因多果等复杂场景

---

## 相关文档

- `Kos.pdf` Section 7 - Application of KOS-TL
- `CORE_API.md` - Core 层 API 参考
- `KERNEL_API.md` - Kernel 层 API 参考
- `RUNTIME_API.md` - Runtime 层 API 参考
- `examples/kernel_example.c` - Kernel 层使用示例
- `examples/core_sigma_example.c` - Core 层 Σ 类型示例
