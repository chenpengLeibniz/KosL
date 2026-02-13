# Core 与 Runtime 实现对照 Kos.pdf 的严肃分析

本文档基于 Kos.tex 与当前 C 实现，对三个问题做逐项对照分析，不做浮夸表述。

---

## （1）Core 层能否起到“宪法”角色？不合法的类型能否完全不能创建？

### Kos.tex 的设计意图

- **299 行**：Core 是系统的 "formal constitution"，其任务是定义 "what constitutes a legal construction"。
- **270 行**：所有输入必须经过 elab；若无法构造有效证明 p，则视为无效输入并立即丢弃，建立 "logical firewall"。
- **3469 行**：通过 "evidence-carrying types" 使 "illegal states are unrepresentable at the design level"。
- **1238 行**：Core 作为 "formal constitution" 在逻辑上无矛盾（无 $\bot$ 的构造子）。

### 当前实现的真实情况

| 设计意图 | 实现状态 | 说明 |
|----------|----------|------|
| 非法类型不能创建 | **部分达成** | `kos_mk_sigma`、`kos_mk_pi`、`kos_mk_sum` 对子类型做 `kos_type_wellformed` 检查，非良构则返回 NULL；`kos_ontology_add_type_definition` 对 `type_def` 做良构检查。 |
| 类型良构判定 | **已实现** | `kos_type_wellformed` 仅接受 PROP/SIGMA/PI/SUM/U/TYPE，拒绝 PAIR/VAL/TIME/ID 作为类型。 |
| 类型检查拒绝非良构类型 | **已实现** | `kos_check` 在入口处检查 `kos_type_wellformed(type)`，非良构则直接返回 false。 |
| 非法类型完全不能创建 | **未完全达成** | `kos_mk_prop`、`kos_mk_val`、`kos_mk_time`、`kos_mk_id`、`kos_mk_pair` 等**不做**良构检查，可构造任意 KOS_PROP/KOS_VAL/... 项；C 代码可直接 `calloc` 并填充 `kos_term` 绕过所有 API。 |

**结论**：Core 层**不能**在实现层面保证“不合法的类型完全不能创建”。原因：

1. **类型构造器未全覆盖**：`kos_mk_prop`、`kos_mk_val`、`kos_mk_time`、`kos_mk_id` 等不校验其“类型角色”，可被当作类型使用；`kos_mk_pair` 构造项，不校验 data/proof 的类型。
2. **C 语言无强制边界**：任何模块都可绕过 kos_mk_*，直接分配 `kos_term` 并填字段，Core 无法拦截。
3. **反序列化未校验**：`kos_term_deserialize` 从 JSON 恢复 term 时，未调用 `kos_type_wellformed` 或 `kos_check`，可引入非法结构。

若要更接近“宪法”角色，需要：对所有构造入口做良构/类型检查、反序列化时校验、以及（在 C 中难以实现的）禁止绕过 API 的机制。

---

## （2）基于 C 的实现能否达成 Core 作为类型系统的功能？

### Kos.tex 的类型论基础

- **299 行**：基于 "Intuitive Dependent Type Theory"（直觉主义依赖类型论）。
- **437 行**：遵循 derivation rules 保证 "logically well-formed"。
- **1238、1308 行**：强正规化（SN）、Subject Reduction、Confluence、唯一正规形。
- **1292 行**：类型检查与等价判定可判定（decidable）。

### C 实现与类型论要求的对照

| 类型论要求 | C 实现 | 差距 |
|------------|--------|------|
| **项由构造规则生成** | 项由 `calloc` + 填字段生成，无语法树约束 | C 无“只能通过构造规则生成项”的保证 |
| **强正规化（SN）** | `kos_reduce` 有最大步数或递归深度限制，但无形式化 SN 证明 | 实现上可终止，但非由类型论保证 |
| **Subject Reduction** | `kos_check` 做结构匹配，归约后类型不变性未形式化验证 | 依赖实现正确性，无元理论证明 |
| **Confluence** | 归约策略固定，无形式化 Confluence 证明 | 实际单一路径归约，无分支收敛证明 |
| **依赖类型** | `kos_substitute` 实现替换，Π/Σ 检查中有依赖 | 依赖绑定使用固定变量名 "x"，未实现完整依赖上下文 |
| **可判定性** | 类型检查与归约在有限步内完成 | 无形式化可判定性证明，仅工程实现 |
| **Id 类型** | Kos.tex 有 $\textsf{Id}_A(a,b)$ | 当前实现**无** Id 类型，因果追溯中的等价判断无法在 Core 层表达 |

### 根本性张力

直觉类型论/CoC 是**函数演算**：项由归纳定义的语法与推导规则唯一生成，归约与类型保持由元理论保证。C 是**命令式语言**：无归纳语法、无推导规则、无类型论元理论，只能通过**手工实现的子程序**模拟这些规则。

因此：

- **能达成**：在工程层面模拟 Π/Σ/Sum、类型检查、归约、良构判定等**部分**功能。
- **不能达成**：由类型论元理论保证的 SN、Subject Reduction、Confluence、可判定性等**形式化性质**；这些在 C 中只能作为“实现假设”，无法在系统内证明。

---

## （3）Runtime 层：异常信号 → 自动精化 → 溯因，如何实现为无需手写代码的自动流程？

### Kos.tex 的流程设计

- **1937–1945 行**：$\textsf{elab} : \textsf{RawSignal} \to \textsf{Env} \to \textsf{Option}(\Sigma_{e:\textsf{Ev}} \textsf{Pre}(e,\Sigma))$；若可构造 $\pi : \textsf{Pre}(e,\Sigma)$ 则返回 $\textsf{Some}(\langle e,\pi\rangle)$，否则 $\textsf{None}$。
- **1925–1934 行**：主循环 `while Q_raw not empty: s=pop; match elab(s,Env): Some -> kernel_step; None -> Log_Refinement_Failure`。
- **1948–1951 行**：精化包括 Signal Parsing、Proof Construction、Time Anchoring。
- **2039 行**：若信号超出范围，精化映射到预定义错误类型项，仍保持良构。

### 当前实现的真实情况

| 设计 | 实现 | 差距 |
|------|------|------|
| **elab 引用本体模板构造证明** | `construct_proof_from_ontology` 不查 TypeOntology，不按类型定义构造证明，对任意信号都返回 "OntologyProof(...)" | 未实现“依据本体自动构造证明” |
| **无法构造证明则返回 None** | 当前实现**从不**返回 NULL（除解析失败），所有信号都被接受 | 逻辑防火墙形同虚设 |
| **Proof Construction 基于 Env** | 未使用 Env（传感器自检、寄存器等），证明为硬编码字符串 | 无物理证据驱动的证明构造 |
| **自动溯因** | 溯因需显式调用 `kos_search_causal_evidence`、`kos_analyze_quality_traceability`，且 K 中事件须为制造业格式 | 非自动触发，且依赖领域特定格式 |
| **主循环** | 无统一的 `while Q_raw not empty` 调度循环，各 demo 手写 enqueue + step | 无内置自动调度 |

### 实现“自动流程”所需机制

要实现**无需手写代码**的“异常信号 → 精化 → 溯因”自动流程，需要：

1. **声明式 elab 规则**  
   在本体中定义：对何种信号模式（如正则、JSON schema、键值约束）可精化为何种事件类型，以及如何从 Env 构造证明。当前本体只有类型定义，无此类规则。

2. **elab 引擎**  
   根据 RawSignal + 本体规则 + Env 自动尝试匹配并构造 $\langle e, \pi \rangle$；无法匹配则返回 NULL，实现真正的逻辑防火墙。

3. **统一调度循环**  
   实现 `kos_runtime_main_loop(sigma, signal_source, ontology_registry)`：从 signal_source 取信号 → elab → 成功则 enqueue → kernel_step → 可选：对失败事件触发溯因。当前无此统一入口。

4. **溯因触发策略**  
   当 elab 产生 FailEvt 或 kernel 检测到失败时，自动调用 `kos_search_causal_evidence` / `kos_analyze_quality_traceability`，并将结果写入日志或报告。当前需应用层显式调用。

5. **领域无关的因果索引**  
   因果索引当前依赖制造业的 FailEvt/ProcStep/Anomaly 字符串格式。要实现通用溯因，需在本体或元数据中声明事件类型之间的因果/时间关系，供索引与搜索使用。

### 建议的实现路径

1. **扩展本体**：增加 `elab_rule` 或类似结构，描述 (signal_pattern, event_type, proof_constructor)。
2. **实现通用 elab 引擎**：根据 `elab_rule` 解析信号、查找类型、构造证明，失败则返回 NULL。
3. **实现 `kos_runtime_main_loop`**：集成 signal_source、elab、kernel_step、可选溯因与持久化。
4. **溯因钩子**：在 kernel_step 或主循环中，对特定事件类型（如 FailEvt）自动调用溯因并写回结果。

### 骨架设计（可落地的接口）

```c
// 1. elab 规则：信号模式 -> 事件类型 + 证明构造
typedef struct {
    const char* pattern;      // 如 "BatchID|ErrorCode|Time" 或 JSON path
    const char* event_type;   // 如 "FailEvt"
    int (*try_elab)(bitstream s, void* env, kos_term** out_event, kos_term** out_proof);
} kos_elab_rule_t;

// 2. 主循环：自动 取信号 -> elab -> enqueue -> step -> 可选溯因
typedef struct {
    kos_ontology_registry_t* ontology_registry;
    kos_reasoning_session_t* reasoning_session;
    kos_elab_rule_t* elab_rules;
    size_t elab_rule_count;
    int (*signal_source_read)(void* ctx, bitstream* out);  // 阻塞或非阻塞
    void* signal_source_ctx;
    bool auto_trace_on_fail;   // 遇 FailEvt 是否自动溯因
} kos_runtime_config_t;

int kos_runtime_main_loop(kos_runtime_config_t* config);
```

主循环逻辑（伪代码）：

```
while (signal_source_read(&s) == 0) {
    event_pair = NULL;
    for (r in elab_rules)
        if (r.try_elab(s, env, &e, &p) == 0) { event_pair = <e,p>; break; }
    if (!event_pair) { Log_Refinement_Failure(s); continue; }
    kos_reasoning_feed_event(session, ontology_id, s);  // 或直接 enqueue
    kos_reasoning_tick(session, ontology_id);
    if (auto_trace_on_fail && is_fail_evt(event_pair))
        report = kos_analyze_quality_traceability(...);  // 自动溯因
}
```

---

## 总结

| 问题 | 结论 |
|------|------|
| （1）Core 能否作为“宪法”保证非法类型不能创建？ | **部分达成**。类型构造器与本体注册处有良构检查，但 `kos_mk_prop`/`kos_mk_pair` 等未检查，且 C 可绕过 API，反序列化未校验。 |
| （2）C 实现能否达成 Core 作为类型系统的功能？ | **工程上可模拟部分功能**，但无法获得类型论的形式化保证（SN、Subject Reduction、Confluence 等）；Id 类型缺失。 |
| （3）如何实现自动的“信号→精化→溯因”流程？ | 需：声明式 elab 规则、通用 elab 引擎、统一主循环、溯因触发策略、领域无关的因果元数据。当前实现均为手写流程，未达到“自动生成、无需额外编码”的设计目标。 |
