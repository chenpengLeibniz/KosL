# RootCauseReport 证明搜索完整过程

本文档 traced 在 `quality_traceability_prove.kos` 上下文下，对目标 `RootCauseReport` 执行 `prove` 的完整搜索过程。

## 目标类型

```
RootCauseReport ≡ Σ(f : FailEvt). Σ(a : Anomaly). CausalProof
```

## 策略顺序 (proveDepth)

```
1. tryValuePred     -- 值依赖谓词 (Gt/Ge/Lt/Le/Eq)
2. tryRefl          -- Id 类型的 Refl
3. tryAssumption    -- 上下文假设匹配
4. tryRewriteAny    -- 用 Id 假设 rewrite
5. tryCaseHypAny    -- Sum 假设的 case 分析
6. tryIntroSigmaD   -- Σ 引入
7. tryIntroPiD      -- Π 引入
8. tryIntroSumLeftD -- Sum 左引入
9. tryIntroSumRightD-- Sum 右引入
10. tryConstructorD -- 构造函数应用
```

采用「第一个成功即返回」：任一策略成功则停止，不再尝试后续策略。

---

## 完整 Trace（深度 d=100）

### 顶层：goal = RootCauseReport

| 步骤 | 策略 | 结果 | 说明 |
|------|------|------|------|
| 1 | tryValuePred | ❌ Nothing | 目标为 Sigma，非 Gt/Ge/Lt/Le/Eq |
| 2 | tryRefl | ❌ Nothing | 目标非 Id |
| 3 | tryAssumption | ❌ Nothing | 上下文中无类型 ≡ RootCauseReport 的项 |
| 4 | tryRewriteAny | ❌ Nothing | 无 Id 类型假设 |
| 5 | tryCaseHypAny | ❌ Nothing | 无 Sum 类型假设 |
| 6 | tryIntroSigmaD | ⚠️ 可选 | 目标为 Σ(f:FailEvt). Σ(a:Anomaly). CausalProof，可引入；但会先尝试 tryConstructor |
| 7–9 | tryIntroPi/Sum | ❌ Nothing | 目标非 Pi/Sum |
| 10 | **tryConstructorD** | ✅ **成功** | 见下文 |

---

### tryConstructorD：遍历上下文定义

对每个 `name ∈ ctxNames`，检查其类型是否形如 `Π...→ RootCauseReport`。

**mkRootCauseReport** 类型：

```
Π(f : FailEvt). Π(a : Anomaly). (CausalProof → RootCauseReport)
```

- `piChainToGoal` 分解为：`[(f, FailEvt), (a, Anomaly), (c, CausalProof)]`，余域 `RootCauseReport` ✓
- 调用 `proveArgsD(d-1) ctx [(f,FailEvt), (a,Anomaly), (c,CausalProof)]`

---

### proveArgsD：参数 1 — FailEvt

| 步骤 | 策略 | 结果 |
|------|------|------|
| 1–2 | tryValuePred, tryRefl | ❌ |
| 3 | **tryAssumption** | ✅ **Var "failEvtUnderInvestigation"** |

→ 得到 `d₁ = failEvtUnderInvestigation`

---

### proveArgsD：参数 2 — Anomaly

（在 `restSubst` 中已把 `f` 替换为 `failEvtUnderInvestigation`，此处 Anomaly 不依赖 f，保持不变）

| 步骤 | 策略 | 结果 |
|------|------|------|
| 3 | **tryAssumption** | ✅ **Var "anomalyEx"** |

→ 得到 `d₂ = anomalyEx`

---

### proveArgsD：参数 3 — CausalProof

| 步骤 | 策略 | 结果 | 说明 |
|------|------|------|------|
| 1–5 | tryValuePred .. tryCaseHypAny | ❌ | |
| 6 | tryIntroSigmaD | ⚠️ 可行 | CausalProof = Σ(e:ProcStep). Prop PCausal，可引入 |
| 10 | **tryConstructorD** | ✅ **成功** | 见下文 |

#### tryConstructorD (子目标 CausalProof)

**mkCausalProof** 类型：

```
Π(a:Anomaly). Π(f:FailEvt). Π(e:ProcStep).
  (Prop PTimeOK → Prop PSpaceOK → Prop PBatchOK → CausalProof)
```

- `piChainToGoal` 分解为 6 个参数类型
- 调用 `proveArgsD` 依次证明：

| 参数 | 类型 | 策略 | 结果 |
|------|------|------|------|
| 1 | Anomaly | tryAssumption | Var "anomalyEx" |
| 2 | FailEvt | tryAssumption | Var "failEvtUnderInvestigation" |
| 3 | ProcStep | tryAssumption | Var "procStepEx" |
| 4 | Prop PTimeOK | tryAssumption | Var "pTimeOK" |
| 5 | Prop PSpaceOK | tryAssumption | Var "pSpaceOK" |
| 6 | Prop PBatchOK | tryAssumption | Var "pBatchOK" |

→ 得到  
`mkCausalProof anomalyEx failEvtUnderInvestigation procStepEx pTimeOK pSpaceOK pBatchOK`

---

### 顶层 tryConstructorD 的最终结果

```
args = [failEvtUnderInvestigation, anomalyEx, mkCausalProof(...)]
proof = foldl App (Var "mkRootCauseReport") args
      = mkRootCauseReport failEvtUnderInvestigation anomalyEx (mkCausalProof ...)
```

但实际输出是 `Pair (Var "failEvtUnderInvestigation") (Pair (Var "anomalyEx") (App (...)))`，说明最终用的是 **tryIntroSigmaD** 而不是 tryConstructorD。

---

## 修正 Trace：tryIntroSigmaD 路径

目标 `RootCauseReport = Σ(f:FailEvt). Σ(a:Anomaly). CausalProof`。

### tryIntroSigmaD 行为

1. 识别目标为 `Sigma f FailEvt (Sigma a Anomaly CausalProof)`
2. 先证 `FailEvt` → `d'`
3. 再证 `Sigma(a:Anomaly). CausalProof`（在 `f:=d'` 的替换下，此处无依赖，仍为 `Σ(a:Anomaly). CausalProof`）

### 第一层 Sigma

- 证 `FailEvt`：tryAssumption → `failEvtUnderInvestigation`
- 证 `Sigma(a:Anomaly). CausalProof`：递归 tryIntroSigmaD

### 第二层 Sigma

- 证 `Anomaly`：tryAssumption → `anomalyEx`
- 证 `CausalProof`：tryConstructor mkCausalProof →  
  `mkCausalProof anomalyEx failEvtUnderInvestigation procStepEx pTimeOK pSpaceOK pBatchOK`

### 合成的 Pair

```
Pair failEvtUnderInvestigation
  (Pair anomalyEx
    (mkCausalProof anomalyEx failEvtUnderInvestigation procStepEx pTimeOK pSpaceOK pBatchOK))
```

这与实际输出一致。策略顺序上，**tryIntroSigmaD 在 tryConstructorD 之前**，因此会先被采用。

---

## 总结：实际成功的搜索路径

```
goal = RootCauseReport
  = Σ(f:FailEvt). Σ(a:Anomaly). CausalProof

【tryIntroSigmaD 成功】
├─ 证 dom₁ = FailEvt
│   └─ tryAssumption → failEvtUnderInvestigation
│
├─ 证 body₁[f:=failEvtUnderInvestigation] = Σ(a:Anomaly). CausalProof
│   【tryIntroSigmaD 成功】
│   ├─ 证 dom₂ = Anomaly
│   │   └─ tryAssumption → anomalyEx
│   │
│   └─ 证 body₂[a:=anomalyEx] = CausalProof
│       【tryConstructorD mkCausalProof 成功】
│       ├─ 证 Anomaly      → tryAssumption → anomalyEx
│       ├─ 证 FailEvt      → tryAssumption → failEvtUnderInvestigation
│       ├─ 证 ProcStep     → tryAssumption → procStepEx
│       ├─ 证 Prop PTimeOK → tryAssumption → pTimeOK
│       ├─ 证 Prop PSpaceOK→ tryAssumption → pSpaceOK
│       └─ 证 Prop PBatchOK→ tryAssumption → pBatchOK
│
└─ 返回 Pair failEvtUnderInvestigation
         (Pair anomalyEx (mkCausalProof anomalyEx failEvtUnderInvestigation procStepEx pTimeOK pSpaceOK pBatchOK))
```

## 与 monograph 算法 8.1 的对应

| monograph | proof 实现 |
|-----------|------------|
| analyze(f) 搜索 Σ(a:Anomaly). CausalProof(a,f) | tryIntroSigma 分解 RootCauseReport，递归证 CausalProof |
| tryWithProc(a,f,e) | tryConstructor mkCausalProof，参数 a,f,e 由 tryAssumption 提供 |
| checkTime → checkSpace → checkBatch | pTimeOK, pSpaceOK, pBatchOK 作为公理化证明由 tryAssumption 匹配 |
| mkCausalProof(a,f,e,p_t,p_s,p_b) | tryConstructor 构造 `App (Var "mkCausalProof") [a,f,e,p_t,p_s,p_b]` |
