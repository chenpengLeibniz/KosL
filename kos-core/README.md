# KOS-Core

**形式化类型论内核** — 基于 Haskell 实现的 KOS Core 层，作为 KOS-TL 系统的“宪法”。实现直觉主义依赖类型论，带双轴 Universe（U_i / Type_i），Parser 为唯一合法 Term 来源。

## 设计原则

- **Parser 即门卫**：所有 Term 仅能通过解析 Core DSL 产生，非法语法无法生成 AST
- **归纳构造**：代数数据类型（`KosCore.AST`）保证每个 Term 变体都有明确定义
- **类型检查即合法性**：`parseAndCheckTerm` / `parseAndCheckModule` 通过后才产出合法 Term，失败则无输出
- **双轴 Universe**：U_i（计算轴）、Type_i（逻辑轴），对应 Kos.tex / monograph 设计

## 构建

```bash
cd kos-core
cabal build
# 或
stack build
```

## 库 API（Library）

对外入口模块为 `KosCore`，主要导出：

| API | 说明 |
|-----|------|
| `parseAndCheckTerm :: Text -> Either String (Term, Term)` | 解析并类型检查单个项，成功返回 `(term, type)` |
| `parseAndCheckModule :: Text -> Either String Module` | 解析并类型检查整个模块（含 `type`/`def` 声明） |
| `contextFromModule :: Module -> Context` | 从已类型检查的模块构建证明用上下文（供 `prove` 在带公理/定义下搜索） |

同时重导出：`KosCore.AST`、`KosCore.Universe`、`KosCore.Context`、`KosCore.TypeCheck`、`KosCore.Parser`、`KosCore.Reduction`、`KosCore.Proof`。类型良构判定见 `TypeCheck.typeWellFormed`。

## 命令行（CLI）

可执行文件：`kos-core`（或 `cabal run kos-core --` / `stack run kos-core --`）。

| 命令 | 说明 |
|------|------|
| `check <file.kos>` | 对模块文件做解析 + 类型检查 |
| `parse <file.kos>` | 仅解析，打印 AST |
| `term <expr>` | 单行项解析并类型检查，输出类型 |
| `json-term <expr>` | 校验项并输出 **C kos_term 兼容的 JSON**（供 Bridge 捕获） |
| `infer-term <expr>` | 类型推理，输出 `{"term":..., "type":...}` JSON，供 C Bridge 使用 |
| `check-term <term> <type>` | 在空上下文中检查 `term : type` |
| `prove <goal>` | 自动证明：在空上下文中对目标类型做证明搜索，成功则输出证明项 |
| `auto <goal>` | 同上（`prove` 的别名） |
| `prove --ctx <file.kos> <goal>` | 先加载 `file.kos` 中的类型与定义，再在该上下文中对 `goal` 做证明搜索 |

示例：

```bash
# 类型检查单个文件
cabal run kos-core -- check examples/fail_evt.kos

# 解析并打印 AST
cabal run kos-core -- parse examples/fail_evt.kos

# 单行项解析与类型检查
cabal run kos-core -- term "Π(x:U₀). x"

# 输出校验后的 JSON（供 C 端反序列化）
cabal run kos-core -- json-term "Prop P"
cabal run kos-core -- infer-term "lam (x:Type1). x"

# 自动证明：空上下文
cabal run kos-core -- prove "Prop P"
cabal run kos-core -- auto "Id(Type1, Prop P, Prop P)"

# 在模块上下文中自动证明
cabal run kos-core -- prove --ctx examples/08_full.kos "Ty"
```

## 语法（Core DSL）

完整语法规范见 [docs/KOS_SYNTAX.md](../docs/KOS_SYNTAX.md)。

```
-- 原子类型
Prop P                  -- 命题 (Prop : Type1)
U0, U1, U               -- 计算轴 Universe
Type0, Type1, Type      -- 逻辑轴 Universe
val "x"                 -- 值
time "2025-01-01"       -- 时间
id "BATCH1"             -- 标识符

-- 依赖积 Π
Pi(x:A). B              -- 依赖函数类型
A -> B                  -- 箭头简写
lam (x:A) . t           -- λ 抽象
f a                     -- 应用

-- 依赖和 Σ
Sigma(x:A). B           -- 依赖对类型
<d, p>                  -- 对构造
split (p) as x y in body -- Σ 消除

-- 和类型
A + B                   -- 不交并
inl(A,B,v), inr(A,B,v)  -- 左/右注入
case s of inl x -> t1; inr y -> t2

-- Id 类型
Id(A,a,b)               -- 恒等类型
refl w                  -- 自反证明
sym e                   -- 对称 (Id A b a 当 e : Id A a b)
transport P e p         -- 沿等式 P 的迁移

-- Let
let x : A := v in body

-- 值依赖谓词（可自动证明时产生 Triv）
Gt/Ge/Lt/Le/Eq(a,b)     -- 比较谓词
```

模块内可写 `type Name : A`（类型同义词）与 `def name : A := t`（定义）。

## 自动化证明（Proof）

`KosCore.Proof` 提供目标类型驱动的证明搜索（对标 Coq/Lean/Agda 的 intro/apply/assumption/cases 等）：

- **策略**：intro/split、apply、assumption、cases、rewrite（Id + Transport + Sym）、值依赖谓词（Gt/Ge/Lt/Le/Eq → Triv）
- **深度限制**：`proveDepth` / `deepen`，防止无限递归
- **入口**：`prove ctx goal` 返回 `Maybe Term`（证明项），CLI 的 `prove`/`auto` 即基于此

详见 [docs/IMPROVEMENTS_METATHEORY_PI_PAIR_AUTO_ID.md](docs/IMPROVEMENTS_METATHEORY_PI_PAIR_AUTO_ID.md)、[docs/PROOF_TOOLS_COMPARISON.md](docs/PROOF_TOOLS_COMPARISON.md)。

## 与 C Runtime 集成

Haskell Core 产出已校验的 Term，可序列化为 JSON。C Runtime 通过：

1. **子进程 + JSON**：调用 `kos-core json-term <expr>` 或 `kos-core infer-term <expr>`，读取 stdout 的 JSON
2. **Bridge**：C 层使用 `kos_core_bridge` 等接口调用上述命令并解析 JSON 为 `kos_term`（见主仓库 `src/core/kos_core_bridge.c`）
3. 未来可考虑 Haskell FFI 直接调用

JSON 格式与 C 端 `kos_term` 结构对应，由 `KosCore.JSON.termToJSON` 生成（先正规化再序列化）。

## 目录结构

```
kos-core/
├── src/
│   ├── KosCore.hs           -- 主模块，对外 API（parseAndCheckTerm/Module, contextFromModule）
│   ├── KosCore/
│   │   ├── AST.hs           -- Term / Declaration / Module 代数数据类型
│   │   ├── Universe.hs      -- 双轴 Universe 判定与层级
│   │   ├── Context.hs       -- 类型上下文、扩展、查找
│   │   ├── Substitution.hs  -- 变量替换
│   │   ├── Alpha.hs         -- α 等价（绑定变量重命名）
│   │   ├── Reduction.hs     -- β/ζ 归约、正规化
│   │   ├── TypeCheck.hs     -- 双向类型检查、definitionallyEqual、typeWellFormed、expandTypeSynonym
│   │   ├── Parser.hs        -- 词法+语法分析（parseTermFromSource, parseModuleFromSource）
│   │   ├── Proof.hs         -- 自动证明策略与 prove
│   │   └── JSON.hs          -- Term → C kos_term 兼容 JSON
├── app/
│   └── Main.hs              -- CLI 入口
├── examples/                -- .kos 示例（见 examples/README.md）
├── test/
│   └── Spec.hs              -- 测试（解析、良构、parseAndCheck、Id/Refl、归约、模块）
├── docs/                    -- 本仓库文档（语法、证明、类型能力等）
├── kos-core.cabal
└── stack.yaml
```

## 示例文件

| 文件 | 覆盖特性 |
|------|----------|
| `01_universe.kos` | U0, U1, U, Type0, Type1, Type |
| `02_prop_base.kos` | Prop, val, time, id |
| `03_pi_lam.kos` | Pi, lam, App, (->) |
| `04_sigma_pair.kos` | Sigma, Pair, split |
| `05_sum_case.kos` | Sum, inl, inr, case |
| `06_id_refl.kos` | Id, refl |
| `07_let.kos` | Let |
| `08_full.kos` | 综合示例 |
| `09_type_inference_reduction.kos` | 类型推理与归约 |
| `core_features.kos` / `fail_evt.kos` | 类型别名、def |

更多见 [examples/README.md](examples/README.md)。

## 测试

```bash
cabal test
# 或
stack test
```

测试覆盖：解析、类型良构、parseAndCheck、Id/Refl、转换与归约、带定义的模块。

## 依赖

- GHC 4.16+（cabal 或 stack）
- `base`, `containers`, `parsec`, `text`

详见 [kos-core.cabal](kos-core.cabal)。

---

# KOS-Core (English)

**Formal type-theoretic kernel** — Haskell implementation of the KOS Core layer, serving as the "constitution" of the KOS-TL system. It implements intuitionistic dependent type theory with a dual-axis universe system (U_i / Type_i); the parser is the sole source of well-formed terms.

## Design principles

- **Parser as gatekeeper**: All terms can only be produced by parsing the Core DSL; ill-formed syntax cannot produce an AST.
- **Inductive construction**: Algebraic data types (`KosCore.AST`) ensure every term variant has a well-defined meaning.
- **Type-checking as legitimacy**: Only after `parseAndCheckTerm` / `parseAndCheckModule` succeed do we obtain valid terms; failure yields no output.
- **Dual-axis universes**: U_i (computational axis) and Type_i (logical axis), aligned with Kos.tex / monograph design.

## Build

```bash
cd kos-core
cabal build
# or
stack build
```

## Library API

The main entry module is `KosCore`. Key exports:

| API | Description |
|-----|-------------|
| `parseAndCheckTerm :: Text -> Either String (Term, Term)` | Parse and type-check a single term; on success returns `(term, type)`. |
| `parseAndCheckModule :: Text -> Either String Module` | Parse and type-check a whole module (including `type`/`def` declarations). |
| `contextFromModule :: Module -> Context` | Build a proof context from a type-checked module (for `prove` to search under axioms/definitions). |

Also re-exported: `KosCore.AST`, `KosCore.Universe`, `KosCore.Context`, `KosCore.TypeCheck`, `KosCore.Parser`, `KosCore.Reduction`, `KosCore.Proof`. Type well-formedness: `TypeCheck.typeWellFormed`.

## Command-line (CLI)

Executable: `kos-core` (or `cabal run kos-core --` / `stack run kos-core --`).

| Command | Description |
|---------|-------------|
| `check <file.kos>` | Parse and type-check a module file. |
| `parse <file.kos>` | Parse only; print AST. |
| `term <expr>` | Parse and type-check a one-line term; print its type. |
| `json-term <expr>` | Validate term and output **C kos_term–compatible JSON** (for Bridge to consume). |
| `infer-term <expr>` | Type inference; output `{"term":..., "type":...}` JSON for C Bridge. |
| `check-term <term> <type>` | Check `term : type` in the empty context. |
| `prove <goal>` | Auto-prove: search for a proof of the goal type in the empty context; on success print the proof term. |
| `auto <goal>` | Same as `prove`. |
| `prove --ctx <file.kos> <goal>` | Load types and definitions from `file.kos`, then search for a proof of `goal` in that context. |

Examples:

```bash
# Type-check a file
cabal run kos-core -- check examples/fail_evt.kos

# Parse and print AST
cabal run kos-core -- parse examples/fail_evt.kos

# One-line term parse and type-check
cabal run kos-core -- term "Π(x:U₀). x"

# Output validated JSON (for C deserialization)
cabal run kos-core -- json-term "Prop P"
cabal run kos-core -- infer-term "lam (x:Type1). x"

# Auto-prove in empty context
cabal run kos-core -- prove "Prop P"
cabal run kos-core -- auto "Id(Type1, Prop P, Prop P)"

# Auto-prove in module context
cabal run kos-core -- prove --ctx examples/08_full.kos "Ty"
```

## Syntax (Core DSL)

Full syntax specification: [docs/KOS_SYNTAX.md](../docs/KOS_SYNTAX.md).

```
-- Atomic types
Prop P                  -- Proposition (Prop : Type1)
U0, U1, U               -- Computational universes
Type0, Type1, Type      -- Logical universes
val "x"                 -- Value
time "2025-01-01"       -- Time
id "BATCH1"             -- Identifier

-- Dependent product Π
Pi(x:A). B              -- Dependent function type
A -> B                  -- Arrow shorthand
lam (x:A) . t           -- λ abstraction
f a                     -- Application

-- Dependent sum Σ
Sigma(x:A). B           -- Dependent pair type
<d, p>                  -- Pair constructor
split (p) as x y in body -- Σ elimination

-- Sum type
A + B                   -- Disjoint sum
inl(A,B,v), inr(A,B,v)  -- Left/right injection
case s of inl x -> t1; inr y -> t2

-- Id type
Id(A,a,b)               -- Identity type
refl w                  -- Reflexivity proof
sym e                   -- Symmetry (Id A b a when e : Id A a b)
transport P e p         -- Transport along equality P

-- Let
let x : A := v in body

-- Value-dependent predicates (yield Triv when auto-proved)
Gt/Ge/Lt/Le/Eq(a,b)     -- Comparison predicates
```

In modules you can write `type Name : A` (type synonym) and `def name : A := t` (definition).

## Automated proof (Proof)

`KosCore.Proof` provides goal-type–driven proof search (analogous to Coq/Lean/Agda intro/apply/assumption/cases):

- **Tactics**: intro/split, apply, assumption, cases, rewrite (Id + Transport + Sym), value-dependent predicates (Gt/Ge/Lt/Le/Eq → Triv).
- **Depth limit**: `proveDepth` / `deepen` to avoid infinite recursion.
- **Entry point**: `prove ctx goal` returns `Maybe Term` (proof term); CLI `prove`/`auto` use this.

See [docs/IMPROVEMENTS_METATHEORY_PI_PAIR_AUTO_ID.md](docs/IMPROVEMENTS_METATHEORY_PI_PAIR_AUTO_ID.md) and [docs/PROOF_TOOLS_COMPARISON.md](docs/PROOF_TOOLS_COMPARISON.md).

## Integration with C Runtime

The Haskell Core produces validated terms that can be serialized to JSON. The C runtime can:

1. **Subprocess + JSON**: Invoke `kos-core json-term <expr>` or `kos-core infer-term <expr>` and read JSON from stdout.
2. **Bridge**: Use `kos_core_bridge` (or similar) to call these commands and parse JSON into `kos_term` (see main repo `src/core/kos_core_bridge.c`).
3. Future option: call Haskell directly via FFI.

The JSON format matches the C `kos_term` structure; it is produced by `KosCore.JSON.termToJSON` (after normalization).

## Directory layout

```
kos-core/
├── src/
│   ├── KosCore.hs           -- Main module, public API (parseAndCheckTerm/Module, contextFromModule)
│   ├── KosCore/
│   │   ├── AST.hs           -- Term / Declaration / Module algebraic types
│   │   ├── Universe.hs     -- Dual-axis universe checks and levels
│   │   ├── Context.hs      -- Type context, extension, lookup
│   │   ├── Substitution.hs -- Variable substitution
│   │   ├── Alpha.hs        -- α-equivalence (binding renaming)
│   │   ├── Reduction.hs    -- β/ζ reduction, normalization
│   │   ├── TypeCheck.hs    -- Bidirectional type checking, definitionallyEqual, typeWellFormed, expandTypeSynonym
│   │   ├── Parser.hs       -- Lexer + parser (parseTermFromSource, parseModuleFromSource)
│   │   ├── Proof.hs        -- Auto-proof tactics and prove
│   │   └── JSON.hs         -- Term → C kos_term–compatible JSON
├── app/
│   └── Main.hs              -- CLI entry
├── examples/                -- .kos examples (see examples/README.md)
├── test/
│   └── Spec.hs              -- Tests (parsing, well-formedness, parseAndCheck, Id/Refl, reduction, modules)
├── docs/                    -- In-repo docs (syntax, proof, type capabilities)
├── kos-core.cabal
└── stack.yaml
```

## Example files

| File | Features covered |
|------|------------------|
| `01_universe.kos` | U0, U1, U, Type0, Type1, Type |
| `02_prop_base.kos` | Prop, val, time, id |
| `03_pi_lam.kos` | Pi, lam, App, (->) |
| `04_sigma_pair.kos` | Sigma, Pair, split |
| `05_sum_case.kos` | Sum, inl, inr, case |
| `06_id_refl.kos` | Id, refl |
| `07_let.kos` | Let |
| `08_full.kos` | Full example |
| `09_type_inference_reduction.kos` | Type inference and reduction |
| `core_features.kos` / `fail_evt.kos` | Type synonyms, def |

More in [examples/README.md](examples/README.md).

## Tests

```bash
cabal test
# or
stack test
```

Coverage: parsing, type well-formedness, parseAndCheck, Id/Refl, conversion and reduction, modules with definitions.

## Dependencies

- GHC 4.16+ (cabal or stack)
- `base`, `containers`, `parsec`, `text`

See [kos-core.cabal](kos-core.cabal).
