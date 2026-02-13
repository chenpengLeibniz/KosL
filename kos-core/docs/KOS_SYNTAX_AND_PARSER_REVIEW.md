# KOS 语法与解析/编译 Review

本文档对 kos-core 的 KOS 语法设定、解析器与类型检查/归约做了一轮审阅，并记录了结论与已做修改。

---

## 一、语法设定是否合理

### 1.1 总体评价

- **与 Kos.tex / 直觉主义依赖类型论一致**：AST 覆盖 Π、Σ、Sum、Id、Let、Universe 双轴（U_i / Type_i）、Prop、val/time/id 等，与 monograph 内核对应良好。
- **优先级与结合性**：`->` 右结合、应用左结合、`+` 左结合，与文档 BNF 一致，符合常见习惯。
- **值依赖谓词**：Gt/Ge/Lt/Le/Eq 作为 Prop 层构造，类型检查时求值比较，适合制造业务约束（如 TimeOK、数值范围），设计合理；文档已补全（见下）。

### 1.2 建议与注意点

- **lam/Pi/Sigma 括号**：规范要求 `lam (x : A) . t`、`Pi(x:A). B` 带括号；此前解析器对开括号用了 `optional`，导致可解析 `lam x : A ) .` 这种不对称形式。已改为**必须**写 `(x : A)`，与文档一致。
- **保留字**：`gt`、`ge`、`lt`、`le`、`eq` 已在 Parser 中保留，但 KOS_SYNTAX.md 未列；已在文档 1.2 与 BNF 中补充。
- **Pair 推断**：`infer` 对 `Pair d p` 得到 `Sigma "x" dTy pTy`，未对 `pTy` 做 `d` 的替换，即依赖对在推断时按非依赖对处理；若只通过 `check` 对给定 Sigma 类型检查 Pair，则无问题。若未来允许只写 Pair 不写类型，需在 infer 中考虑依赖替换。

---

## 二、解析是否正确

### 2.1 与 BNF / 文档一致性

| 项目 | 状态 |
|------|------|
| 注释 `--` / `/* */` | ✓ |
| 标识符与保留字 | ✓（已补 gt/ge/lt/le/eq 文档） |
| `module M where` + 声明 | ✓ |
| `type Name : Term` / `def name : Term := Term` | ✓ |
| Universe：U0/U1/U, Type0/Type1/Type | ✓ |
| Prop P, val/time/id 字面量 | ✓ |
| Pi/Σ 与箭头、lam、应用 | ✓；lam/Pi/Sigma 已改为必须括号 |
| `<a,b>`、split、case、inl/inr（双/单参数）、Id/refl、Let | ✓ |
| 值依赖谓词 gt/ge/lt/le/eq | ✓ 解析；文档已补 |

### 2.2 解析顺序与歧义

- **inl/inr**：先尝试三参数 `inl(A,B,v)`，再尝试单参数 `inl v`，顺序正确，无歧义。
- **Let**：在 parseAtomic 中置于 Var 之前，避免 `let` 被当成变量。
- **parseArrow 中 Pi 的复用**：`A -> B` 时若左侧已是 `Pi x dom _` 则复用绑定名，否则用 `_`，行为符合「非依赖简写」的语义。

### 2.3 已做解析修改

- **lam**：由 `optional (symbol "(")` 改为必须 `symbol "("`，与 `")"` 成对，只接受 `lam (x : A) . body`。
- **Pi / Sigma**：同样改为必须 `(x : A)`，只接受 `Pi(x:A). B`、`Sigma(x:A). B`（及 Unicode 形式）。

---

## 三、类型检查与归约

### 3.1 Sum 类型检查（已修复）

- **问题**：当期望类型为 `Sum leftTy rightTy`、项为类型表达式 `Sum a b` 时，原逻辑用 `check ctx a leftTy` 与 `check ctx b rightTy`。对「类型 vs 类型」应使用**定义等价**，否则对非 Prop 的类型（如 `U0 + U0`）会错误失败。
- **修改**：对 `term = Sum a b` 分支改为使用 `definitionallyEqual ctx a leftTy` 与 `definitionallyEqual ctx b rightTy`，二者皆成立才接受。

### 3.2 其他检查与归约

- **Universe 层级**：UniverseInfo 与 universeLeq 使用正常；双轴与层级在 TypeCheck/Universe 中一致。
- **值依赖谓词**：Gt/Ge/Lt/Le/Eq 在 check 时用 evalValuePred / evalValuePredEq，Triv 作为证明项；infer 时这些谓词给出 Type1（Prop 层），逻辑正确。
- **β/ζ/ι**：App-Lam、Let、Split-Pair、Case-InL/InR/InLS/InRS 的归约与 AST 一致；η 在 reduceStep 中对 Lam 有处理。
- **δ**：Var 通过 ctxLookupDef 展开定义，与「def 可 δ 展开」一致。

---

## 四、文档与测试

- **KOS_SYNTAX.md**：已补充值依赖谓词一节（3.8）、保留字列表与 BNF 中的 gt/ge/lt/le/eq。
- **测试**：`test/Spec.hs` 覆盖解析、typeWellFormed、parseAndCheck、Let/β 归约、模块解析等；建议在可联网/代理正确时运行 `stack test` 做回归。

---

## 五、小结

| 类别 | 结论 |
|------|------|
| 语法设定 | 合理，与依赖类型论及现有文档一致；括号与保留字已收紧/补全。 |
| 解析 | 正确；lam/Pi/Sigma 括号要求已与文档统一。 |
| 类型检查 | Sum 类型表达式分支已改为定义等价；其余与 AST/Universe/值谓词设计一致。 |
| 归约 | β/ζ/ι/δ（及 η）实现与规范一致。 |

建议后续：若支持「仅 Pair 不写类型」的推断，再在 `infer` 中为 Pair 考虑依赖替换；并保持 KOS_SYNTAX.md 与 Parser 的变更同步。
