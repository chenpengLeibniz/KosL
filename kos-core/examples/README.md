# .kos 示例文件

本目录包含覆盖所有 .kos 语言特性的示例模块。语法规范见 [docs/KOS_SYNTAX.md](../docs/KOS_SYNTAX.md)。

## 示例索引

| 文件 | 覆盖特性 |
|------|----------|
| `01_universe.kos` | Universe: U0, U1, U, Type0, Type1, Type |
| `02_prop_base.kos` | Prop, val, time, id (Base Sorts) |
| `03_pi_lam.kos` | Pi, lam, App, arrow (->) |
| `04_sigma_pair.kos` | Sigma, Pair (<,>), split |
| `05_sum_case.kos` | Sum (+), inl, inr, case |
| `06_id_refl.kos` | Id, refl |
| `07_let.kos` | Let 绑定 |
| `08_full.kos` | 综合示例，覆盖所有特性 |
| `simple.kos` | 最简模块 |
| `core_features.kos` | Id, Let, def, 类型别名 |
| `fail_evt.kos` | 类型别名（与 C core 对应） |
| `quality_traceability.kos` | KOS-TL 应用示例：质量追溯类型与谓词（monograph 第 8 章） |
| `quality_traceability_prove.kos` | 根因证明演示：给定失效事件，prove 自动构造 RootCauseReport |

## 运行

```bash
# 类型检查
cabal run kos-core -- check examples/01_universe.kos

# 解析并打印 AST
cabal run kos-core -- parse examples/01_universe.kos

# 根因证明：给定失效事件，自动构造 RootCauseReport
cabal run kos-core -- prove --ctx examples/quality_traceability_prove.kos RootCauseReport
```
