# 不合格产品信号运行过程演示

以信号 **“检测到一个不合格的产品，其批次Batch_26_01-00001，时间为2026-01-30”** 为例，展示 KOS-TL 系统从物理信号到逻辑事件、溯源、内核演化的完整运行过程。

---

## 一、系统运行流程概览

```
物理信号 → [Runtime: elab] → <event, proof> → [Traceability] → 根因报告
                                    ↓
                              [Kernel: 入队] → [STEP 演化] → K 更新、TS 递增
```

---

## 二、Phase 1：自然语言信号处理

### 输入信号

```
检测到一个不合格的产品，其批次Batch_26_01-00001，时间为2026-01-30
```

### 运行过程

| Step | 层级 | 操作 | 结果 |
|------|------|------|------|
| 1 | Runtime | 信号到达 | 原始比特流进入 elab |
| 2 | Runtime | elab 提炼 | 非 .kos 格式 → `parse_signal_to_event` 将整段文本转为 KOS_PROP 事件 |
| 3 | Runtime | 构造证明 | `construct_proof_from_ontology` 生成 BasicProof(event=...) |
| 4 | Runtime | 输出 | 成功返回 `<event, proof>` |
| 5 | Traceability | 提取 FailEvt | 自然语言格式不匹配 `FailEvt(batch=..., error=..., time=...)` → 无法提取 |
| 6 | Traceability | 根因报告 | `root_cause_report = NULL` |

### 结论

- **elab 成功**：信号能构成合法事件，逻辑防火墙放行
- **溯源失败**：事件格式非 FailEvt，无法触发制造业溯源逻辑

---

## 三、Phase 2：结构化 FailEvt 信号处理

### 输入信号（结构化格式）

```
FailEvt(batch=Batch_26_01-00001, error=UNQUALIFIED, time=1738195200)
```

其中 `1738195200` 为 2026-01-30 00:00:00 UTC 的 Unix 时间戳。

### 运行过程

| Step | 层级 | 操作 | 结果 |
|------|------|------|------|
| 1 | Runtime | 信号到达 | 原始比特流进入 elab |
| 2 | Runtime | elab 提炼 | 非 .kos 格式 → `parse_signal_to_event` 将字符串转为 KOS_PROP 事件 |
| 3 | Runtime | 构造证明 | 生成 OntologyProof(event=...) |
| 4 | Runtime | 输出 | 成功返回 `<event, proof>` |
| 5 | Traceability | 提取 FailEvt | `strncmp(s, "FailEvt(", 8)==0` → `sscanf` 解析 batch、error、time |
| 6 | Traceability | 因果搜索 | `kos_analyze_quality_traceability` 从 sigma->K 构建因果索引，查找 (Anomaly, ProcStep, FailEvt) 链 |
| 7 | Traceability | 根因报告 | 若 K 中有因果数据（异常、工艺步骤、失败事件），返回 RootCauseReport |
| 8 | Kernel | 入队 | `kos_queue_enqueue(sigma->P, event_pair)` |
| 9 | Kernel | STEP 演化 | `kos_step(sigma)` → Peek-Verify-Reduce-Confirm → K 更新、TS++ |

### 预置因果数据（K 中）

为展示溯源成功，需在 K 中预置因果链：

- **ProcStep**：Batch_26_01-00001 在 HeatTreatment_05 上，时间区间 [T-3600, T]
- **Anomaly**：HeatTreatment_05 温度参数 185.2°C 异常，时间 T-1800
- **FailEvt**：Batch_26_01-00001 UNQUALIFIED，时间 T

满足因果约束：异常在工艺步骤内、步骤结束早于失败时间、批次一致。

### 结论

- **elab 成功**：信号构成合法事件
- **溯源成功**：提取 FailEvt，从 K 中找到因果链，生成根因报告
- **Kernel 演化**：事件入队并执行 STEP，K 单调增长

---

## 四、运行演示

```bash
cd build
cmake ..
cmake --build .
./bin/unqualified_product_demo
```

### 预期输出示例

```
========================================
KOS-TL 不合格产品信号运行过程演示
信号: 检测到一个不合格的产品，其批次Batch_26_01-00001，时间为2026-01-30
========================================

========================================
  Phase 1: 自然语言信号处理
========================================

[Step 1] 信号到达 Runtime 层
  原始信号: 检测到一个不合格的产品，其批次Batch_26_01-00001，时间为2026-01-30

[Step 2] elab 算子提炼信号
  ✓ 提炼成功：信号构成合法事件 <event, proof>
  - event: 检测到一个不合格的产品，其批次Batch_26_01-00001，时间为2026-01-30
  - proof: BasicProof(event=...)
  - 溯源: 无（自然语言格式无法解析为 FailEvt）

========================================
  Phase 2: 结构化 FailEvt 信号处理
========================================

[Step 1] 结构化信号到达
  信号: FailEvt(batch=Batch_26_01-00001, error=UNQUALIFIED, time=1738195200)

[Step 2] elab 算子提炼
  ✓ 提炼成功
  ✓ 自动溯源成功:
    - 失败: batch=Batch_26_01-00001, error=UNQUALIFIED, time=1738195200
    - 根因: temperature 异常 (设备=HeatTreatment_05, 参数=temperature, 值=185.20)

[Step 3] 事件入队并执行 Kernel STEP
  ✓ 事件入队
  - STEP 演化: 成功 (TS=4)

========================================
  Phase 3: 运行过程总结
========================================

完整流程:
  1. 信号 → elab: 物理比特流映射为 <event, proof>
  2. 若 event 为 FailEvt 且 sigma->K 非空 → 自动溯源找根因
  3. kos_queue_enqueue → kos_step: 事件入队、Peek-Verify-Reduce-Confirm 演化
  4. K 单调增长，TS 递增

=== 演示完成 ===
```

---

## 五、相关文件

| 文件 | 说明 |
|------|------|
| `examples/unqualified_product_demo.c` | 演示程序 |
| `src/runtime/elab.c` | elab 算子实现 |
| `src/runtime/signal_process.c` | kos_runtime_process_signal |
| `src/domain/manufacturing/traceability.c` | kos_try_extract_fail_evt_from_event、kos_analyze_quality_traceability |
| `docs/RUNTIME_SIGNAL_PROCESS_AND_TRACEABILITY.md` | 信号处理与溯源 API 说明 |
