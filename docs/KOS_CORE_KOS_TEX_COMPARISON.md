# KOS-Core 实现与 Kos.tex Core 层严格对照

本文档对当前 Haskell 实现的 `kos-core/` 与 Kos.tex（Kos.pdf）中关于 Core 层的阐述进行逐项对照，标注**已实现**、**部分实现**、**未实现**及**形式化差距**。

**参考**：Kos.tex 第 297–1310 行（Core Layer 语法、推导规则、归约、元理论）。

---

## 一、Core 层角色与定位

| Kos.tex 阐述 | 实现状态 | 说明 |
|--------------|----------|------|
| Core 是系统的 "formal constitution"（形式化宪法） | **已实现** | Parser 即门卫，唯一合法 Term 来源 |
| 定义 "what constitutes a legal construction" | **已实现** | `typeWellFormed` 仅接受合法类型构造子 |
| 基于直觉主义依赖类型论（ITT） | **已实现** | Π/Σ/Sum/Prop/Universe 对应 ITT |
| 逻辑上无矛盾（无 ⊥ 构造子） | **已实现** | AST 无空类型构造子 |
| 强可判定（Strongly decidable） | **部分实现** | 类型检查可终止，但无形式化 SN 证明支撑可判定性 |

---

## 二、语法与类型构造

### 2.1 类型构造（Types）

| Kos.tex 定义 | KOS-Core 实现 | 状态 |
|--------------|---------------|------|
| $\textsf{Prop}$ | `Prop Text` | **已实现** |
| $\textsf{Type}_i$ | `Universe Logical i` | **已实现** |
| $\mathcal{U}_i$ | `Universe Computational i` | **已实现** |
| $\textsf{Val}, \textsf{Time}, \textsf{ID}$ | `Val`, `Time`, `Ident` | **已实现** |
| $\Pi(x:A).B$ | `Pi Text Term Term` | **已实现** |
| $\Sigma(x:A).B$ | `Sigma Text Term Term` | **已实现** |
| $A + B$ | `Sum Term Term` | **已实现** |
| $\textsf{Id}_A(a, b)$ | `Id Term Term Term` | **已实现** |

### 2.2 项构造（Terms）

| Kos.tex 定义 | KOS-Core 实现 | 状态 |
|--------------|---------------|------|
| $x$（变量） | `Var Text` | **已实现** |
| $\lambda x.t$, $t\,u$ | `Lam`, `App` | **已实现** |
| $\langle t, u \rangle$ | `Pair Term Term` | **已实现** |
| $\textsf{split}(t, x.y.u)$ | `Split Term Text Text Term` | **已实现** |
| $\textsf{inl}(t)$, $\textsf{inr}(t)$ | `InL`, `InR`；单参数 `InLS`, `InRS` | **已实现** |
| $\textsf{case}(t, x.u, y.v)$ | `Case Term Text Term Text Term` | **已实现** |
| $\textsf{refl}$（Id 引入） | `Refl Term` | **已实现** |
| $\textsf{let } x=u \textsf{ in } t$ | `Let Text Term Term Term` | **已实现** |

---

## 三、双轴 Universe 系统

| Kos.tex 规则 | KOS-Core 实现 | 状态 |
|--------------|---------------|------|
| $\textsf{Prop} : \textsf{Type}_1$ | `Prop n -> Universe Logical 1` | **已实现** |
| $\textsf{Type}_i : \textsf{Type}_{i+1}$ | `Universe ax lev -> Universe ax (lev+1)` | **已实现** |
| $\mathcal{U}_i : \mathcal{U}_{i+1}$ | 同上 | **已实现** |
| $\mathcal{U}_i : \textsf{Type}_{i+1}$（计算轴可提升到逻辑轴） | `universeLeq` 中 `(Computational, Logical) -> l1+1 <= l2` | **已实现** |
| $\textsf{Prop} \hookrightarrow \mathcal{U}_1$（命题嵌入数据轴） | `propEmbedToData` | **已实现** |
| $\textsf{Prop}$ 的 Impredicativity | `infer` 中 bodySort 为 Prop 时返回 Type₁ | **已实现** |

---

## 四、推导规则（Judgmental Rules）

### 4.1 Π 类型

| Kos.tex 规则 | KOS-Core 实现 | 状态 |
|--------------|---------------|------|
| 引入：$\frac{\Gamma,x:A \vdash t:B}{\Gamma \vdash \lambda x:A.t : \Pi(x:A).B}$ | `Lam` 分支：`check ctx' bodyTerm bodyTy` | **已实现** |
| 消除：$\frac{\Gamma \vdash f:\Pi(x:A).B \quad \Gamma \vdash a:A}{\Gamma \vdash f\,a : B[a/x]}$ | `App` 分支：`Right (substitute x a body)` | **已实现** |

### 4.2 Σ 类型

| Kos.tex 规则 | KOS-Core 实现 | 状态 |
|--------------|---------------|------|
| 引入：$\frac{\Gamma \vdash a:A \quad \Gamma \vdash b:B[a/x]}{\Gamma \vdash \langle a,b \rangle : \Sigma(x:A).B}$ | `Pair`：`check ctx d dom`，`check ctx p (substitute x d body)` | **已实现** |
| 消除：$\textsf{split}(p, x.y.t)$ | `Split (Pair d p) x1 x2 body -> substitute x2 p (substitute x1 d body)` | **已实现** |

### 4.3 Sum 类型

| Kos.tex 规则 | KOS-Core 实现 | 状态 |
|--------------|---------------|------|
| 引入：$\textsf{inl}(a):A+B$，$\textsf{inr}(b):A+B$ | `InL`/`InR` 三参数；`InLS`/`InRS` 单参数（check 模式需期望 Sum A B） | **已实现** |
| 消除：$\textsf{case}(s, x.t, y.u)$ | `Case` | **已实现** |

### 4.4 Conversion 规则

| Kos.tex 规则 | KOS-Core 实现 | 状态 |
|--------------|---------------|------|
| $\frac{\Gamma \vdash t:A \quad \Gamma \vdash A \equiv B \quad \Gamma \vdash B:\mathcal{S}}{\Gamma \vdash t:B}$ | `check` 结构失败时 fallback：`infer` + `definitionallyEqual` | **已实现** |

---

## 五、归约规则（Reduction Rules）

| Kos.tex 规则 | KOS-Core 实现 | 状态 |
|--------------|---------------|------|
| β：$(\lambda x:A.t)\,u \to t[u/x]$ | `App (Lam x _ body) arg -> substitute x arg body` | **已实现** |
| ι（Σ）：$\textsf{split}(\langle u,v \rangle, x.y.t) \to t[u/x, v/y]$ | `Split (Pair d p) x1 x2 body -> ...` | **已实现** |
| ι（Sum）：$\textsf{case}(\textsf{inl}(u), x.t, y.v) \to t[u/x]$ | `Case (InL ...) x1 t1 ... -> substitute x1 v t1` | **已实现** |
| ι（Id）：$\textsf{refl}$ 时恒等判断自动消解 | Id 类型由 definitionallyEqual 判定 | **已实现** |
| δ（定义展开）：$c \equiv_\delta t$ | `ctxLookupDef` + `reduceStep` 对 Var 展开 | **已实现** |
| ζ（let 展开）：$\textsf{let } x=u \textsf{ in } t \equiv_\zeta t[u/x]$ | `Let x _ val body -> substitute x val body` | **已实现** |
| η：$\lambda x.(f\,x) \equiv_\eta f$（$x \notin \text{FV}(f)$） | `Lam x ty (App f (Var y))` 当 y==x 且 x∉FV(f) | **已实现** |

---

## 六、元理论性质（Kos.tex 定理）

| Kos.tex 定理/定义 | KOS-Core 实现 | 状态 |
|-------------------|---------------|------|
| **强正规化（SN）**：所有 well-formed term 无无穷归约链 | **无证明** | **未实现**：`normalize` 可终止，但无形式化 SN 证明 |
| **Subject Reduction**：$t \to t'$ 则 $\Gamma \vdash t' : A$ | **无证明** | **未实现**：归约后类型保持性未验证 |
| **Confluence**：$M \twoheadrightarrow N$ 且 $M \twoheadrightarrow P$ 则存在 $Q$ | **无证明** | **未实现**：无形式化 Confluence 证明 |
| **唯一正规形** | **无证明** | **未实现**：依赖 SN + Confluence |
| **可判定性**：类型检查与等价判定可判定 | **工程实现** | **部分实现**：实现上可终止，但无元理论支撑 |
| **逻辑一致性**：无 $\vdash t:\bot$ | **结构保证** | **已实现**：AST 无 ⊥ 构造子 |

---

## 七、Core 三大功能模块（Kos.tex 164 行）

| Kos.tex 模块 | KOS-Core 实现 | 状态 |
|--------------|---------------|------|
| **类型构造器与本体管理器**：将业务本体转为 Sorts 与依赖类型 | **部分实现** | 有类型构造，无本体管理器（无 TypeOntology、无业务本体注册） |
| **归约引擎**：函数应用、结构分解，计算逻辑稳态 | **已实现** | `Reduction.hs` 实现 β、ι（Σ/Sum） |
| **类型检查器**：双向类型检查，Correct-by-construction | **已实现** | `TypeCheck.hs` 实现 infer/check |

---

## 八、计算自反性（Computational Reflexivity）

| Kos.tex 阐述 | KOS-Core 实现 | 状态 |
|--------------|---------------|------|
| Core 描述自身归约规则 | 归约规则硬编码于 `Reduction.hs` | **部分实现**：有归约，非声明式 |
| 每次小步演化自动合成 $\textsf{Id}(t,t')$ 证明 | **无** | **未实现**：有 Id/Refl，但归约时无自动证明合成 |
| 全路径自动审计 | **无** | **未实现** |

---

## 八点五、Kos.pdf 未定义的 kos-core 扩展

以下为 kos-core 实现但 **Kos.pdf 未在 Core 层语法/规则中定义** 的构造，属工程扩展：

| 扩展 | 实现位置 | 说明 |
|------|----------|------|
| **Gt, Ge, Lt, Le, Eq**（值依赖谓词） | AST.hs, TypeCheck.hs, Parser.hs | 类型构造依赖项数值比较，如 `gt(val "200", val "180") : Prop`；Kos.pdf 无此语法 |
| **InLS, InRS**（单参数 inl/inr） | AST.hs, Parser.hs | `inl v` 需 check 模式期望 `Sum A B`；Kos.pdf 仅定义 `inl(a):A+B` 显式双参数形式 |
| **Prop Text**（命题名参数） | AST.hs | Kos.pdf 中 Prop 为单一 sort；实现允许 `Prop "P"` 等，用于区分命题名 |
| **Sym, Transport**（Id 对称与迁移） | AST.hs, TypeCheck.hs, Reduction.hs, Proof.hs | Kos.tex 仅写 refl 与“恒等判断自动消解”；实现增加 Sym（Id A a b → Id A b a）、Transport（沿等式迁移证明），用于 rewrite 策略 |

---

## 九、总结对照表

| 维度 | Kos.tex 要求 | KOS-Core 实现 | 差距 |
|------|--------------|---------------|------|
| **类型构造** | Prop, Type_i, U_i, Val, Time, ID, Π, Σ, +, Id | 已实现 | 无 |
| **项构造** | λ, app, pair, split, inl, inr, case, refl, let | 已实现 | 无 |
| **Universe** | 双轴、层级、Prop↪U₁、U_i↪Type_{i+1}、Impredicativity | 已实现 | 无 |
| **归约** | β, ι(Σ/Sum/Id), δ, ζ, η | 已实现 | 无 |
| **定义等价** | $\equiv$ 由 β,ι,δ,ζ,η 生成 | `definitionallyEqual` = normalize + α | 无 |
| **Conversion** | Γ⊢t:A, A≡B ⇒ Γ⊢t:B | check fallback: infer + definitionallyEqual | 无 |
| **元理论** | SN, Subject Reduction, Confluence, 可判定性 | 无形式化证明 | 仅工程实现 |
| **本体管理** | 业务本体 → Sorts/类型 | 无 | 无 TypeOntology |
| **计算自反性** | 自动合成 Id 证明 | Id + definitionallyEqual 支持等价判定 | 部分：无自动证明合成 |

---

## 十、补全项实施状态（2025-01 更新）

| 补全项 | 状态 | 实现说明 |
|--------|------|----------|
| **1. Id 类型** | ✅ 已实现 | `Id Term Term Term`、`Refl Term`；TypeCheck 中 Id 引入/消除；Parser: `Id(A,a,b)`、`refl w`；`typeWellFormed` 支持 Id |
| **2. 定义等价** | ✅ 已实现 | `definitionallyEqual ctx t1 t2` = normalize + α 等价；用于 Id/Refl 检查与 Conversion 规则 |
| **3. δ/ζ/η** | ✅ 已实现 | Context `addDef`/`ctxLookupDef`；Let 构造；ζ: Let 展开；δ: Var 定义展开；η: λx.f x → f（x∉FV(f)） |
| **4. Impredicativity** | ✅ 已实现 | Pi infer 中 bodySort 为 Prop 时返回 Type₁ |
| **5. inl/inr 单参数** | ✅ 已实现 | `InLS Term`、`InRS Term`；Parser: `inl v`、`inr v`；check 模式需期望 Sum A B |
| **6. Conversion 规则** | ✅ 已实现 | `check` 结构失败时 fallback：`infer` 推断 A，`definitionallyEqual A ty` 则接受 |
| **7. 类型别名展开** | ✅ 已实现 | `expandTypeSynonym` 递归展开 Var（如 `def p : T` 中 T→Type1）；模块 `type T : Type1` 支持 |

---

## 十一、已知简化与进一步改进方向

**已知简化**（与 Kos.tex 的差异）：
- **Π 类型推断**：`infer` 对 Π 构造统一返回 `Type₁`；Kos.tex 规定 predicative 情形为 $\Pi(x:A).B : \textsf{Type}_{\max(i,j)}$，实现未按层级精确计算
- **Pair 类型推断**：`infer (Pair d p)` 返回 `Sigma "x" dTy pTy`，对依赖 B(x) 的 Σ 类型做了简化

| 方向 | 说明 |
|------|------|
| **Id 消除** | Kos.tex 仅写 refl 时恒等消解；kos-core 已扩展 **Sym**（等式对称）、**Transport**（沿等式的类型迁移），支持 rewrite 策略与“沿 Id 迁移证明”，相当于部分 J/transport 能力 |
| **元理论形式化** | SN、Subject Reduction、Confluence 的机器可验证证明 |
| **本体管理器** | 业务本体 → Sorts/类型 的 TypeOntology 模块 |
| **计算自反性自动合成** | 归约时自动合成 Id(t,t') 证明 |

---

## 十二、Kos.tex 行号索引（便于追溯）

| 内容 | Kos.tex 行号 |
|------|--------------|
| Core 层角色、三大模块 | 157–165 |
| 双轴 Universe、Base Sorts | 305–323 |
| 类型构造语法 | 328–333 |
| 项构造语法 | 335–341 |
| Π/Σ/Sum/Id 构造规则 | 378–411 |
| Universe Lifting、Prop↪U₁ | 324–335 |
| 推导规则（Π/Σ/Sum 引入消除） | 341–481 |
| 归约规则（β/ι/δ/ζ/η） | 488–546 |
| 定义等价 | 544–546 |
| SN、Subject Reduction、Confluence | 559–944 |
| 可判定性定理 | 1292–1310 |
