# KOS-TL Kernel 层完善总结

## 1. 核心数据结构（Γ、σ、trace）

| 符号 | 实现 | 说明 |
|------|------|------|
| **Γ** | `kos_kernel_context_t` | 类型与谓词集合，保存所有类型、谓词、构造子 |
| **σ** | `kos_state_t` | 知识状态 σ = ⟨K, TS, P⟩：K 为类型实例（叶子项）Σ 链，TS 逻辑时钟，P 事件队列 |
| **trace** | `kos_trace_t` | σ 的演化轨迹 T = σ₀ →^⟨e₁,p₁⟩ σ₁ → … |

**统一会话** `kos_kernel_session_t` 整合三者，提供一站式知识操作。

## 2. 知识操作

### 2.1 事件喂入与小步演化

```c
// 喂入事件对 ⟨e, p⟩
kos_kernel_session_enqueue_event(session, event_pair);

// 执行一步：Verify(Pre) → Reduce → Update(K, TS) → 追记 trace
kos_kernel_session_step(session);
```

### 2.2 根因查找（调用 kos-core prove）

```c
// 在 ctx_kos_path 上下文中对 RootCauseReport 做证明搜索
kos_term* proof = NULL;
bool found = kos_kernel_session_find_root_cause(session, ctx_path, "RootCauseReport", &proof, err, sizeof(err));
if (found && proof) {
    // proof 为 Pair failEvt (Pair anomaly causalProof) 形式
    kos_term_free(proof);
}
```

### 2.3 轨迹跟踪

- 每次 `kos_kernel_session_step` 成功后会调用 `kos_trace_append` 记录一步
- 每步包含：step_index、ts_after、event_pair、可选的 K_snapshot

## 3. Bridge 扩展（kos-core prove）

- `kos_core_bridge_prove(ctx_file, goal, errmsg, size)`：仅返回是否找到证明
- `kos_core_bridge_prove_json(ctx_file, goal, errmsg, size)`：返回证明项 `kos_term*`（需调用者 free）

kos-core 新增命令：
```bash
kos-core prove-json --ctx <file.kos> <goal>
```
输出证明项的 JSON，供 C 层反序列化为 `kos_term`。

## 4. 新增/修改文件

| 文件 | 变更 |
|------|------|
| `include/kos_kernel_session.h` | 新增：统一会话 API |
| `src/kernel/kernel_session.c` | 新增：会话实现 |
| `include/kos_core_bridge.h` | 新增：prove / prove_json API |
| `src/core/kos_core_bridge.c` | 新增：prove 桥接实现 |
| `include/kos_core.h` | 新增：KOS_APP、KOS_LAM、kos_mk_lam |
| `src/core/type_builder.c` | 修正 kos_mk_app 使用 KOS_APP，新增 kos_mk_lam |
| `src/core/storage.c` | 新增：APP/LAM 序列化与反序列化 |
| `src/core/substitution.c` | 新增：APP/LAM 替换逻辑 |
| `src/core/reduction.c` | 修正：β 归约使用 KOS_APP，新增 APP/LAM 递归归约 |
| `kos-core/app/Main.hs` | 新增：prove-json 命令 |
| `kos-core/src/KosCore/JSON.hs` | 新增：App、Lam、Triv 的 JSON 输出 |

## 5. 核心应用场景 API（kos_kernel_scenarios.h）

| 场景 | API | 说明 |
|------|-----|------|
| **1. 根因追溯** | `kos_kernel_session_find_root_cause` | 回溯轨迹，形式化定位根因 |
| **2. 反事实推理** | `kos_kernel_counterfactual_test` | 对比事实/反事实上下文，判断排除变量是否因果必要 |
| **3. 合规性决策** | `kos_kernel_verify_decision_compliance` | 验证事件 Pre 是否成立 |
| | `kos_kernel_session_enqueue_if_compliant` | 仅合规事件才入队 |
| **4. 审计与问责** | `kos_trace_replay_into` / `kos_trace_replay` | 轨迹重放到已有 σ / MTK 从 initial_K 确定性重放得新 σ |
| | `kos_trace_export_audit_json` | 导出审计轨迹 JSON |
| | `kos_kernel_session_export_audit_trail` | 导出到文件 |
| **5. 复杂系统治理** | `kos_kernel_session_evolve_until_idle` | 演化直到队列空，每步合法 |
| | `kos_kernel_state_is_legal` | 检查状态合法性 |
| **6. AI 治理** | `kos_kernel_verify_ai_suggestion` | 验证 AI 建议是否在规范范围内 |

## 6. 使用示例

```c
kos_kernel_session_t* session = kos_kernel_session_create("examples/quality_traceability_prove.kos");

// 场景 1：根因查找
kos_term* proof = NULL;
if (kos_kernel_session_find_root_cause(session, NULL, "RootCauseReport", &proof, err, 256)) {
    kos_term_free(proof);
}

// 场景 2：反事实推理（需准备 fact.kos 与 counterfact.kos）
kos_counterfactual_result_t cf;
kos_kernel_counterfactual_test("fact.kos", "counterfact.kos", "RootCauseReport", &cf);
// cf.excluded_necessary == true 表示排除的变量是因果必要的

// 场景 3：合规性入队
kos_kernel_session_enqueue_if_compliant(session, event_pair);

// 场景 4：审计导出
kos_kernel_session_export_audit_trail(session, "audit.json");

// 场景 6：AI 建议验证
bool ok = kos_kernel_verify_ai_suggestion(NULL, "mkFailure ...", "FailEvt");

kos_kernel_session_free(session);
```
