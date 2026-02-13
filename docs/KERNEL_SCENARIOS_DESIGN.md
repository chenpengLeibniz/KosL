# KOS-TL Kernel 层核心场景设计与实现

## 1. 场景总览

| 场景 | 核心诉求 | Kernel 支持 |
|------|----------|-------------|
| 根因追溯 | 回溯轨迹，形式化定位根因 | `find_root_cause` + trace |
| 反事实推理 | 合法假设，可构造性推导 | `counterfactual_test`（双上下文 prove 对比） |
| 合规性决策 | 每个决策可被 trace 证明 | `verify_decision_compliance`、`enqueue_if_compliant` |
| 审计与问责 | 可重放的证明，责任归属 | `trace_replay`、`export_audit_json` |
| 复杂系统治理 | 每步合法性定义与追溯 | `evolve_until_idle`、`state_is_legal` |
| AI 治理 | AI 建议在规范内可验证 | `verify_ai_suggestion` |

## 2. 设计要点

### 2.1 根因追溯
- 依赖 trace 记录 σ 演化
- 通过 kos-core prove 在上下文（含实例）中对 RootCauseReport 做证明搜索
- 证明项即根因报告（Pair failEvt (Pair anomaly causalProof)）

### 2.2 反事实推理
- “若 X 未发生，Y 是否仍会发生？”
- 实现：准备两个 .kos 上下文
  - `ctx_factual`：完整上下文
  - `ctx_counterfactual`：排除假设未发生的变量（如某异常）
- 分别对 goal 做 prove，对比结果
- `excluded_necessary = factual_provable && !counterfactual_provable`

### 2.3 合规性决策
- 决策 = 事件 ⟨e, p⟩ 入队并演化
- 合规 = Pre(e) 在当前 K 上成立
- `kos_verify_precondition` 实现 Pre 检查
- `enqueue_if_compliant` 仅合规事件入队

### 2.4 审计与问责
- trace 即审计日志
- `trace_replay`：按序重放，可验证演化可复现
- `export_audit_json`：导出 JSON，含每步 index、ts_after、event_pair
- 责任归属：event_pair 标识驱动事件，可扩展 step 增加 actor_id

### 2.5 复杂系统治理
- `evolve_until_idle`：持续 step 直至队列空
- 每步均经 Verify(Pre) → Reduce → Update → Confirm(Post)
- `state_is_legal`：检查 σ 是否处于可接受状态

### 2.6 AI 治理
- `verify_ai_suggestion(suggestion_expr, expected_type)`：检查 suggestion : expected_type
- 当前使用 kos-core check-term 空上下文
- 扩展：可增加 `--ctx` 支持，在规范上下文下校验

## 3. 扩展方向

1. **trace 增强**：step 增加 `actor_id`、`decision_id` 用于问责
2. **反事实**：支持从单 .kos 自动生成“排除 X”的临时上下文
3. **AI 治理**：bridge 增加 check-term --ctx，支持规范上下文校验
