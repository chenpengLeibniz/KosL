# 三项改进：元理论、Π/Pair 推断、归约时 Id 合成

本文档说明 KOS 类型能力对照中列出的三项“未完全覆盖”改进的范围、设计与实现要点，便于后续按优先级落地。

---

## 一、元理论的形式化证明

### 1.1 范围

- **强正规化 (SN)**：所有良类型项可归约到正规形式且归约序列终止。
- **Subject Reduction**：若 Γ ⊢ t : A 且 t → t′，则 Γ ⊢ t′ : A。
- **Confluence**：若 t →* t₁ 且 t →* t₂，则存在 t′ 使得 t₁ →* t′ 且 t₂ →* t′。
- **可判定性**：类型检查与定义等价判定可终止。

### 1.2 设计要点

- **不改变 kos-core 运行时行为**：元理论是对“现有实现所满足的性质”的形式化，而非新功能。
- **两种路线**：
  - **路线 A（推荐）**：在 Agda/Coq/Lean 中形式化 KOS Core 的语法、归约与类型规则，并证明 SN、Subject Reduction、Confluence；再（可选）证明 Haskell 实现与该形式化规范一致。
  - **路线 B**：在仓库内维护一份“元理论陈述”文档（仅定理陈述与证明草图），不提供机器可验证证明。
- **与 Kos.tex 的对应**：Kos.tex 已陈述这些定理；形式化时可直接对应其规则与归纳结构。

### 1.3 实现状态与建议

- **当前**：无机器可验证证明；类型检查与 `normalize` 在工程上可终止。
- **建议**：单独开仓或子目录（如 `kos-core-metatheory`），用证明助手形式化语法、归约、类型规则与上述三条定理；Haskell 代码可加注释引用形式化中的规则编号。

---

## 二、Π / Pair 的层级与依赖推断细节

### 2.1 Π 类型推断的层级 (Kos.tex predicative 规则)

**规范**：当 A : Type_i、B : Type_j 时，Π(x:A).B : Type_{\max(i,j)}。

**当前实现**：`infer (Pi x dom body)` 对 body 的 sort 做分支，但**对 dom 的 sort 未推断**，且返回值统一为 `Universe Logical 1`。

**改进**：

1. 在推断 Π 类型时，先推断 **dom 的 sort**：`domSort <- infer ctx dom`。
2. 已有 `bodySort <- infer (extend x dom ctx) body`。
3. 从 `domSort`、`bodySort` 中提取层级：
   - 若为 `Universe Logical i` / `Universe Logical j`，则返回 `Universe Logical (max i j)`。
   - 若一方为 `Prop _`，按 Type₁ 处理（与现有 Prop 规则一致）。
   - 其余情况保持向后兼容，退回 `Universe Logical 1`。

**涉及文件**：`TypeCheck.hs` 中 `infer` 的 `Pi` 分支。

### 2.2 Pair 推断的依赖 (Σ 第二分量依赖第一分量)

**规范**：对 Σ(x:A).B，Pair d p 中 p 的类型应为 B[d/x]；推断 Pair 时，第二分量的类型应在“扩展了 x : A”的上下文中推断，以便 B 中出现 x。

**当前实现**：`infer (Pair d p)` 在**同一上下文**中分别推断 d 和 p，得到 `Sigma "x" dTy pTy`，其中 pTy 无法依赖 x。

**改进**：

1. 先推断第一分量：`dTy <- infer ctx d`。
2. 扩展上下文：`ctx' = extend "x" dTy ctx`（或使用 Sigma 的绑定名）。
3. 在 `ctx'` 中推断第二分量：`pTy <- infer ctx' p`。
4. 返回 `Sigma x dTy pTy`（pTy 中可含 x，即依赖对）。

**涉及文件**：`TypeCheck.hs` 中 `infer` 的 `Pair` 分支。

### 2.3 实现状态

- 见下文代码改动：**Π 层级**与 **Pair 依赖推断**已在 `TypeCheck.hs` 中实现。

---

## 三、归约时自动合成 Id 证明

### 3.1 两种含义

1. **目标为 Id 时的自动填洞**  
   当证明目标为 Id A t t′ 且 t ≡ t′（定义等价）时，自动给出证明项（如 Refl）。  
   **当前**：`Proof.hs` 中 **tryRefl** 已实现：若目标为 `Id aTy a b` 且 `definitionallyEqual ctx a b`，则返回 `Refl a`；且已纳入 **proveDepth** 的策略链。  
   **结论**：此含义已覆盖，无需再改。

2. **归约一步时附带“等式证明”**  
   当执行单步归约 t → t′ 时，除得到 t′ 外，还得到类型为 Id A t t′ 的证明项（便于后续在证明或 DSL 中使用“带证明的归约”）。  
   **当前**：`Reduction.hs` 仅有 `reduceStep`，无证明产出。  
   **改进**：增加 **reduceStepWithProof**：若 `reduceStep ctx t = Just t'`，则返回 `(t', pf)`，其中 `pf` 在定义等价下可被接受为 Id A t t′ 的证明（例如 `Refl (normalize ctx t)`，类型为 Id (…) (normalize t) (normalize t)，与 Id A t t′ 通过 conversion 一致）。

### 3.2 设计要点

- **reduceStepWithProof** 需要知道 t 的类型 A（用于构造 Id A …）。可从 `infer ctx t` 得到；若推断失败则返回 Nothing。
- 返回的证明项建议为 **Refl (normalize ctx t)**，类型检查器会通过 definitionallyEqual 接受其为 Id A t t′ 的证明。
- **可选**：在 Proof 策略中，对目标 Id A t t′ 在 tryRefl 中优先使用 **normalize** 后的项构造 Refl，以更好利用 conversion（当前 tryRefl 已用 definitionallyEqual，语义正确）。

### 3.3 实现状态

- **目标 Id 自动填洞**：已由 tryRefl + proveDepth 覆盖。  
- **归约一步带证明**：在 `Reduction.hs` 中实现并导出 **reduceStepWithProof**（见下文），供需要“带证明小步”的调用方使用。

---

## 四、小结表

| 改进项 | 范围 | 实现方式 | 状态 |
|--------|------|----------|------|
| 元理论形式化 | SN, Subject Reduction, Confluence, 可判定性 | 证明助手中形式化（建议独立仓/目录） | 未做，已写设计 |
| Π 推断层级 | infer(Pi) 返回 Type_{\max(i,j)} | TypeCheck.hs：推断 dom/body sort，取 max | 已实现 |
| Pair 依赖推断 | infer(Pair d p) 中 p 在 x:dTy 下推断 | TypeCheck.hs：extend ctx 再 infer p | 已实现 |
| Id 目标自动证明 | 目标 Id A t t′ 且 t≡t′ ⇒ Refl | Proof.hs tryRefl + proveDepth | 已有 |
| 归约一步带证明 | t → t′ 时产出 (t′, pf : Id A t t′) | Reduction.hs + 可选 API | 已实现 reduceStepWithProof |

---

更细的与 Kos.tex 的对照见 **KOS_PDF_TYPE_CAPABILITY_REVIEW.md** 与仓库根目录 **docs/KOS_CORE_KOS_TEX_COMPARISON.md**。
