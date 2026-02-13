# 因果关系追溯实现说明

本文档说明如何实现**因果关系追溯**：给定失败事件 FailEvt，从知识集 K 中自动查找满足因果约束的 (Anomaly, ProcStep) 链，并生成根因报告。

---

## 1. 因果有效性约束

因果链 (Anomaly a, ProcStep e, FailEvt f) 有效需满足：

| 约束 | 含义 |
|------|------|
| **时间逻辑** | a.t ∈ e.dur 且 e.dur.end < f.t（异常发生在工艺步骤内，且步骤结束早于失败） |
| **空间逻辑** | a.m = e.m（异常与步骤在同一机器） |
| **批次一致性** | e.b = f.b（步骤与失败为同一批次） |

---

## 2. 实现架构

### 2.1 因果事件索引（Causal Index）

- **头文件**：`include/kos_causal_trace.h`
- **实现**：`src/domain/manufacturing/causal_index.c`

**能力**：

- 存储 FailEvt、ProcStep、Anomaly 数组
- **从 K 构建**：`kos_causal_index_build_from_K(idx, K)` 遍历 K，解析字符串表示（`FailEvt(...)`、`ProcStep(...)`、`Anomaly(...)`）并填充索引
- **显式注册**：`kos_causal_index_add_fail_evt`、`add_proc_step`、`add_anomaly` 用于外部数据源或增量更新
- **搜索**：`kos_causal_search_evidence(idx, failure, &n)` 返回满足因果约束的 `kos_causal_chain_t*` 数组

### 2.2 追溯分析（Traceability）

- **实现**：`src/domain/manufacturing/traceability.c`

**API**：

- **`kos_search_causal_evidence(FailEvt, sigma)`**：从 sigma->K 构建索引，搜索因果链，返回首个有效链的 kos_term 表示（CausalProof）
- **`kos_analyze_quality_traceability(FailEvt, sigma)`**：同上，但返回 `RootCauseReport*`（含 failure、anomaly、causal_proof）
- **`kos_root_cause_report_free(report)`**：释放根因报告及其内部 kos_term

---

## 3. 使用流程

### 3.1 事件格式要求

K 中的事件需为制造业领域类型构造器生成的字符串表示：

- `FailEvt(batch=..., error=..., time=...)`
- `ProcStep(batch=..., machine=..., start=..., end=...)`
- `Anomaly(machine=..., param=..., value=..., time=...)`

可通过 `kos_mk_fail_event`、`kos_mk_proc_step`、`kos_mk_anomaly` 构造，或由制造业 runtime_elab 产出。

### 3.2 示例

```c
// 1. 构造事件并加入 K
BatchID batch = {.batch_id = "Batch_202310-01"};
ErrorCode err = {.code = "HARD_ERR"};
Machine machine = {.machine_id = "HeatTreatment_03", .line_id = "Line1"};
Param param = {.param_name = "voltage"};
ParamValue pv = {.param = param, .value = 220.5};
TimeRange step_dur = {.start = 1697000000ULL, .end = 1697002000ULL};

kos_term* proc_step = kos_mk_proc_step(batch, machine, step_dur);
kos_term* anomaly = kos_mk_anomaly(machine, param, pv, 1697001000ULL);
kos_term* fail = kos_mk_fail_event(batch, err, 1697004000ULL);

sigma->K = kos_update_knowledge(sigma->K, proc_step);
sigma->K = kos_update_knowledge(sigma->K, anomaly);
sigma->K = kos_update_knowledge(sigma->K, fail);

// 2. 因果证据搜索
FailEvt failure = {.batch = batch, .error = err, .time = 1697004000ULL, .qa_shift_proof = NULL};
kos_term* evidence = kos_search_causal_evidence(failure, sigma);

// 3. 根因分析
RootCauseReport* report = kos_analyze_quality_traceability(failure, sigma);
if (report) {
    printf("Root cause: %s anomaly on %s\n", report->anomaly.param.param_name, report->anomaly.machine.machine_id);
    kos_root_cause_report_free(report);
}
```

---

## 4. 高级用法：显式索引

当 K 中事件格式与上述不同，或需从外部数据源（DB、流）构建索引时，可显式注册事件：

```c
kos_causal_index_t* idx = kos_causal_index_create();
kos_causal_index_add_fail_evt(idx, fail_evt);
kos_causal_index_add_proc_step(idx, proc_step);
kos_causal_index_add_anomaly(idx, anomaly);

size_t n = 0;
kos_causal_chain_t* chains = kos_causal_search_evidence(idx, failure, &n);
// ... 使用 chains ...
kos_causal_chains_free(chains, n);
kos_causal_index_free(idx);
```

---

## 5. 与现有模块的关系

| 模块 | 关系 |
|------|------|
| **types.c** | `kos_mk_fail_event`、`kos_mk_proc_step`、`kos_mk_anomaly` 生成因果索引可解析的字符串 |
| **predicates.c** | `kos_check_causal_validity` 验证 CausalProof 的因果有效性 |
| **traceability.c** | 使用 causal_index 实现 `kos_search_causal_evidence` 和 `kos_analyze_quality_traceability` |
| **quality_traceability_demo.c** | 演示完整因果追溯流程 |
