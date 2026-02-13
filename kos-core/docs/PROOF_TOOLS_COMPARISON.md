# kos-core 证明模块与类型论证明工具对标

本文档将 kos-core 的 Proof 模块与主流基于类型论的证明助手（Coq、Lean、Agda 等）的常见能力做对照，并说明已实现项与可扩展方向。

---

## 一、对标总表

| 能力 | Coq / Lean | kos-core | 说明 |
|------|------------|----------|------|
| **intro** | 将 Π/→ 的绑定移入上下文，新目标为体 | `tryIntroPi` | 目标 Π(x:A).B 时扩展 Γ,x:A 证 B，返回 λx.body |
| **apply** | 用项与目标结论合一，产生子目标 | `tryConstructor` | 在 Γ 中找类型 Π…→ goal 的定义，对参数递归证明后构造 App |
| **exact** | 用给定项精确填满当前目标 | `exactTerm ctx goal t` | 若 check ctx t goal 通过则返回 t |
| **assumption** | 在上下文中找与目标匹配的假设 | `tryAssumption` | 目标与某变量类型定义等价时返回 Var name |
| **split / constructor** | 分解合取或应用归纳构造子 | `tryIntroSigma`、`tryIntroSumLeft/Right` | Σ 时证两分量得 Pair；Sum 时证左或右得 InL/InR |
| **rfl / reflexivity** | 用定义等价闭合等式目标 | `tryRefl` | 目标 Id A a b 且 a≡b 时返回 Refl a |
| **深度限制** | auto n、solve [n] 等 | `proveDepth depth`、`defaultProveDepth` | depth≤0 不再递归，避免发散 |
| **first \| t1 \| t2** | 依次尝试直至一个成功 | `firstSuccess`、`orElse` | 组合多策略为“第一个成功” |
| **repeat t** | 反复执行 t 直至失败 | `deepen maxDepth` | 用深度 1…maxDepth 递增尝试，作为有限“加深”版 repeat |
| **try t** | t 失败则等价于 skip | `tryTactic`、orElse | 单策略包装或通过 prove 顺序体现 |
| **exact h** | 用指定假设 h 填满目标 | `tryExactVar name` | 仅当该假设类型与目标定义等价时成功 |
| **cases / destruct** | 对和类型/归纳类型分情况 | `tryCaseHyp name`、`tryCaseHypAny` | 目标为 Sum 时 tryIntroSum*；对假设 s:A+B 分情况由 tryCaseHyp 产生 Case s x pL y pR，已并入 prove |
| **rewrite / rw** | 用等式重写目标或假设 | `tryRewriteL2R`、`tryRewriteR2L`、`tryRewriteWith`、`tryRewriteAny` | 用 Id 假设将目标中 a→b 或 b→a 替换，证新目标后用 Transport（+ Sym）得原目标；已并入 prove |
| **simp / simplify** | 用规则化简目标 | `goalNorm` | 仅做 normalize + expandTypeSynonym |

---

## 二、已实现与用法

### 2.1 深度限制（对标 Coq auto 深度）

- **proveDepth :: Int -> Context -> Term -> Maybe Term**  
  带深度上限的证明搜索；递归子目标时传入 depth-1，depth≤0 立即返回 Nothing。
- **defaultProveDepth**  
  默认 100，可由上层改为更小（更快、更可预测）或更大（更复杂目标）。
- **用法**：`prove ctx goal` 内部等价于 `proveDepth defaultProveDepth ctx goal`；需要更紧资源时用 `proveDepth 20 ctx goal` 等。

### 2.2 Sum 引入（对标 Lean Or.inl / Or.inr、split 对析取）

- **tryIntroSumLeft / tryIntroSumRight**  
  目标为 Sum A B 时，分别尝试证 A 或 B，返回 InL A B proof 或 InR A B proof。
- 已并入主策略顺序（在 tryIntroPi 之后、tryConstructor 之前），无需单独调用。

### 2.3 exact（对标 Lean exact / Coq exact）

- **exactTerm :: Context -> Term -> Term -> Maybe Term**  
  `exactTerm ctx goal t`：若 `check ctx t goal` 成立则返回 Just t，否则 Nothing。
- 用于“给定候选证明项，检查是否就是当前目标的解”；不参与默认 prove 顺序，由调用方在需要时使用。

### 2.4 策略组合

- **firstSuccess [t1, t2, ...]**  
  依次尝试 t1、t2、…，返回第一个 Just。
- **orElse**  
  `m1 `orElse` m2`：m1 为 Just 则取 m1，否则 m2。
- **tryTactic t**  
  只运行策略 t，失败即返回 Nothing（与直接调用 t 等价，用于组合时语义清晰）。
- **deepen maxDepth**  
  用深度 1, 2, …, maxDepth 依次调用 proveDepth，直到某一深度成功；可替代“repeat 直到成功”的有限版本。
- 主 prove 顺序即一条“firstSuccess”链：值谓词 → Refl → assumption → **rewrite（tryRewriteAny）** → cases 假设（tryCaseHypAny）→ Sigma → Pi → Sum左 → Sum右 → constructor。

### 2.5 指定假设：exact / cases

- **tryExactVar name**  
  若上下文中变量 `name` 的类型与当前目标定义等价，则返回 `Var name`（对标 `exact h`）。
- **tryCaseHyp name**  
  若 `name` 的类型为 Sum A B，则在左分支扩展 `__caseL : A`、右分支扩展 `__caseR : B`，将 goal 中的 `name` 分别替换为 inl/inr 后证明两分支，成功则返回 `Case (Var name) __caseL pL __caseR pR`（对标 `cases H` / `destruct H`）。
- **tryCaseHypAny**  
  对上下文中任一 Sum 类型假设尝试 tryCaseHyp，已并入 proveDepth 策略顺序。

### 2.6 Rewrite（用等式重写目标）

- **replaceSubterm ctx a b goal**  
  将 goal 中所有与 a 定义等价的子项替换为 b，用于构造新目标与类型族 P。
- **tryRewriteL2R eqName**  
  假设 `eqName : Id A a b`，将目标中 a 重写为 b，证新目标后用 `Transport P (Sym eq) proofNew` 得原目标（P a）。
- **tryRewriteR2L eqName**  
  将目标中 b 重写为 a，证新目标后用 `Transport P eq proofNew` 得原目标（P b）。
- **tryRewriteWith eqName**  
  先尝试 L2R，再尝试 R2L（对标 `rewrite H` / `rw [H]`）。
- **tryRewriteAny**  
  对上下文中任一 Id 类型假设尝试 tryRewriteWith，已并入 proveDepth。  
- AST 新增 **Sym**（等式对称）、**Transport**（Id 消除）；归约规则：`Transport P (Refl a) p ≡ p`，`Sym (Refl a) ≡ Refl a`。

---

## 三、与 KOS-TL 的衔接

- **Core 层**：Proof 提供“给定 Γ 与目标 G 的证明搜索”，与 monograph 中“证明构造与小步演化”一致；依赖型构造子（proveArgs 顺序代入）、候选列表（tryWithCandidates）已支持。
- **Kernel 层**：σ、getProcSteps/getAnomalies 由上层维护；将候选与目标传入 prove / tryWithCandidates 即可。
- **领域谓词**：TimeOK/SpaceOK/BatchOK 等在 .kos 中定义为 Gt/Le/Eq 或 Σ 等形式后，可由 tryValuePred、tryConstructor 等处理。

---

## 四、可扩展方向（对标进阶能力）

| 方向 | 说明 |
|------|------|
| **repeat / 多子目标** | 若引入“子目标栈”或多目标配置，可增加 repeat t、t \<;\> t2（对多子目标分别应用 t2）等组合子。 |
| **rewrite 在假设中** | 当前仅支持用等式重写目标；若需“在假设 H 中用等式重写”需扩展为在上下文中替换后重新证明。 |
| **hint db** | 类似 Coq hint 的“按目标形状选用引理/构造子”的数据库，可扩展 tryConstructor 的候选来源。 |
| **induction** | 对归纳类型做归纳策略需 AST 支持归纳定义与递归子，属较大扩展。 |

---

*文档与当前 Proof 模块实现对应；若增加新策略或组合子，可在此表中补充。*
