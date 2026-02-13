# kos-core 与 Kos.pdf 类型能力对照结论

本文档直接回答：**目前的 kos-core 实现能否达成 Kos.pdf（Kos.tex）中描述的类型能力？**

**结论：能达成。** 除元理论的形式化证明和个别工程简化外，Kos.tex Core 层所描述的**类型构造、项构造、推导规则、归约与定义等价**均在 kos-core 中实现，且 Id 的“基于等式的迁移”已通过 Sym + Transport 扩展实现（Kos.tex 未显式写出 J/Transport，但等价判定与 refl 行为一致）。

---

## 一、Kos.pdf 中 Core 层类型能力摘要（Kos.tex §3）

- **类型**：Prop, Type_i, U_i, Val, Time, ID, Π(x:A).B, Σ(x:A).B, A+B, Id_A(a,b)
- **项**：变量，λ/应用，pair/split，inl/inr/case，refl，以及（辅助）let
- **判断**：Γ ⊢ A : S，Γ ⊢ t : A，Γ ⊢ A ≡ B / t ≡ u（定义等价）
- **归约**：β、ι（Σ/Sum/Id）、δ、ζ、η；定义等价由这些生成
- **双轴 Universe**：Prop : Type₁，Type_i : Type_{i+1}，U_i : U_{i+1}，U_i : Type_{i+1}，Prop ↪ U₁；Prop 的 Impredicativity

---

## 二、逐项达成情况

| Kos.pdf 能力 | kos-core 状态 | 说明 |
|--------------|----------------|------|
| **类型构造** | ✅ 达成 | Prop, Universe(Logical/Computational), Val/Time/Ident, Pi, Sigma, Sum, Id |
| **项构造** | ✅ 达成 | Var, Lam, App, Pair, Split, InL/InR/InLS/InRS, Case, Refl, Let；扩展：Sym, Transport |
| **Π 引入/消除** | ✅ 达成 | check Lam；infer App → substitute |
| **Σ 引入/消除** | ✅ 达成 | check Pair；reduce Split(Pair d p) → t[d/x,p/y] |
| **Sum 引入/消除** | ✅ 达成 | InL/InR/InLS/InRS；Case + ι 归约 |
| **Id 引入** | ✅ 达成 | Refl；check 时 definitionallyEqual 判定 a≡b |
| **Id 消除/迁移** | ✅ 扩展达成 | Kos.tex 仅写“refl 时恒等判断自动消解”；kos-core 增加 Sym、Transport，支持基于等式的重写与迁移 |
| **Conversion** | ✅ 达成 | check 失败时 infer + definitionallyEqual |
| **定义等价** | ✅ 达成 | normalize + α 等价，与 β/ι/δ/ζ/η 一致 |
| **β/ι/δ/ζ/η** | ✅ 达成 | Reduction.hs 全部实现 |
| **双轴 Universe** | ✅ 达成 | Universe.hs、typeWellFormed、universeLeq、Prop↪U₁、Impredicativity |
| **强可判定** | ⚠ 工程可判定 | 类型检查与等价判定可终止，但无 SN/Confluence 等形式化证明 |
| **计算自反性** | ⚠ 部分 | 有 Id/Refl 与 Transport；归约时“自动合成 Id(t,t')”未做 |

---

## 三、与 Kos.pdf 的已知差异（均属实现简化或扩展）

1. **Π 类型 kind**：infer 对 Π 统一给 Type₁；Kos.tex 的 predicative 规则为 Type_{\max(i,j)}，未按层级精细计算。
2. **Pair 推断**：Pair 推断为 Sigma "x" dTy pTy，依赖对在 infer 中未做替换简化。
3. **扩展项**：Gt/Ge/Lt/Le/Eq（值依赖谓词）、InLS/InRS（单参数 inl/inr）、Sym/Transport 为 kos-core 扩展，Kos.tex 未写；与现有类型能力兼容。

---

## 四、未实现或仅部分实现的部分（Kos.pdf 有提但不要求在 Core 实现里全部完成）

- **元理论**：SN、Subject Reduction、Confluence、可判定性定理——Kos.tex 有陈述，kos-core 无机器证明，仅实现可终止的 typecheck/normalize。
- **本体管理器**：Kos.tex 提到的“类型构造器与本体管理器”“业务本体 → Sorts”——在 Core 层为概念性描述，kos-core 仅提供类型构造与检查，无独立 TypeOntology 模块。
- **计算自反性自动合成**：Kos.tex 希望“每次小步自动合成 Id(t,t')”——当前为“等价用 definitionallyEqual + Refl/Transport 处理”，未在每一步归约时自动插入 Id 证明。

---

## 五、总结

- **类型能力**：Kos.pdf 中 Core 层描述的类型与项构造、推导规则、归约与定义等价，**在 kos-core 中均已实现**，并能达成文档所设的“静态逻辑宪法”与强可判定类型检查。
- **Id 能力**：Kos.tex 中的 Id 引入与“refl 时恒等消解”已实现；**Id 的消除/迁移**通过 Sym + Transport 在 kos-core 中扩展实现，支持基于等式的重写与证明迁移。
- **未完全覆盖**：元理论的形式化证明、归约时自动合成 Id 证明的 API、以及独立的本体管理模块，为已知差距或后续改进方向。其中 **Π 推断层级**（Type_{\max(i,j)}）与 **Pair 依赖推断**（p 在 x:dTy 下推断）已在代码中实现；**归约一步带证明** 已提供 `reduceStepWithProof`。

更细的逐条对照见仓库根目录 **docs/KOS_CORE_KOS_TEX_COMPARISON.md**。三项改进的设计与实现要点见 **kos-core/docs/IMPROVEMENTS_METATHEORY_PI_PAIR_AUTO_ID.md**。
