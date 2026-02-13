# Core 层独立化与类型论形式化保障建议

本文档基于对 `coc/`（Calculus of Constructions）实现与当前 C Core 层的对照分析，提出将 Core 独立出来、保证类型论形式化保障的架构建议。

---

## 一、问题背景

根据 `CORE_RUNTIME_RIGOROUS_ANALYSIS.md` 的结论：

1. **C 实现只能部分模拟 Core 作为类型系统的功能**：工程上可模拟 Π/Σ/Sum、类型检查、归约等，但无法获得 SN、Subject Reduction、Confluence 等**形式化保证**。
2. **根本张力**：直觉类型论/CoC 是**函数演算**——项由归纳定义的语法与推导规则**唯一**生成；C 是**命令式语言**——无归纳语法、无推导规则，只能通过手工子程序模拟，且可被 `calloc` + 直接填字段绕过。

---

## 二、CoC 实现为何能保证形式化性质

### 2.1 架构特点（参考 `coc-implementation.md`）

| 特性 | CoC 实现 | C Core 实现 |
|------|----------|-------------|
| **项的唯一入口** | 词法→语法→AST，**仅通过 Parser 创建 Term** | `kos_mk_*` API + 任意 `calloc` 可绕过 |
| **AST 表示** | 代数数据类型 `enum Term { Var, App, Abs, Pi, ... }`，**穷尽匹配** | `struct kos_term` + `union`，可任意填充 |
| **类型检查时机** | 解析后立即 `check_module`，**未通过则无合法 Term** | `kos_check` 事后检查，非法 term 已存在 |
| **上下文管理** | `Context` 独立结构，`extend`/`lookup` 不可变扩展 | `kos_term` 链表模拟 ctx，易混入非法绑定 |
| **定义等价** | `normalize` + `alpha_equivalent`，有明确语义 | `kos_reduce` 有步数限制，无形式化等价定义 |
| **错误处理** | `CocError`/`TypeError` 分层，解析失败即无输出 | 返回 NULL/false，调用方可能忽略 |

### 2.2 CoC 的“形式化保障”来源

1. **Parser 作为唯一门卫**：所有 Term 必须经 `parse_term`/`parse_module` 产生，非法语法无法生成 AST。
2. **归纳构造**：Rust 的 `enum` 保证每个 Term 变体都有明确定义，无“未定义状态”。
3. **类型检查即合法性判定**：`check_module` 通过后才将声明加入 Context；未通过则整个流程失败，无“半合法”状态。
4. **不可变与克隆**：Context 扩展返回新 Context，避免共享可变状态导致的非法修改。
5. **可测试性**：`run_golden_test` 等对解析+类型检查做回归测试，行为可复现。

---

## 三、建议：将 Core 独立为“形式化内核”

### 3.1 总体思路

**将 Core 从 C Runtime/Kernel 中剥离，作为独立的、具有单一合法入口的类型论内核**，参考 `coc/` 的架构：

```
                    ┌─────────────────────────────────────┐
                    │         Core 形式化内核               │
                    │  (独立工程，唯一合法 Term 来源)        │
                    ├─────────────────────────────────────┤
                    │  词法 → 语法 → AST → 类型检查 → 合法Term │
                    │  失败则无输出，无旁路                  │
                    └─────────────────────────────────────┘
                                        │
                                        │ 仅输出：已类型检查的 Term / 序列化格式
                                        ▼
┌───────────────────────────────────────────────────────────────────────────┐
│  C Runtime / Kernel / Ontology                                             │
│  - 仅通过 Core 的“验证接口”接收 Term                                       │
│  - 不直接构造 kos_term，不 calloc kos_term                                 │
│  - 反序列化时必须经 Core 校验                                              │
└───────────────────────────────────────────────────────────────────────────┘
```

### 3.2 方案 A：Core 用 Rust 实现（推荐）

**将 Core 重写为 Rust 库**，与 `coc/` 类似，作为独立 crate：

- **目录结构**：`kos-core/`（与 `coc/` 平级）
  - `src/ast.rs`：KOS 的 Term 代数数据类型（对应 Kos.tex 的 Π/Σ/Sum/Prop/U/Type 等）
  - `src/lexer.rs`、`src/parse.rs`、`parser.lalrpop`：Core DSL 词法/语法
  - `src/typecheck.rs`：双向类型检查
  - `src/context.rs`：类型上下文
  - `src/reduction.rs`：归约、定义等价
- **对外接口**：
  - `parse_and_check(source: &str) -> Result<Module, CocError>`：唯一合法入口
  - `serialize_checked(module: &Module) -> Vec<u8>`：输出已校验的序列化格式
  - C 侧通过 **FFI** 调用：`kos_core_parse_and_check(char* src, size_t len)` 返回 opaque handle 或序列化 blob

**优势**：

- 与 CoC 相同的“Parser 即门卫”保障
- 代数数据类型天然防止非法构造
- 可选：用 Creusot 或导出到 Coq/Agda 做元理论形式化
- C Runtime 无法绕过，只能通过 FFI 获取已校验 Term

**代价**：需维护 Rust 工具链与 FFI 绑定。

### 3.3 方案 B：Core DSL + C 解析器（折中）

若必须保留纯 C 实现，可引入 **Core DSL** 作为唯一合法来源：

1. **定义 Core 语法**（如 `.kos` 或 `.core` 文件）：
   - 对应 Kos.tex 的项：Prop, Π, Σ, Sum, U_i, Type_i, pair, inl, inr 等
   - 语法规则与 CoC 类似，但适配 KOS 双轴 Universe

2. **实现 C 解析器**（可手写或使用 lemon/lemonplus 等）：
   - 解析器**唯一**产出 `kos_term`
   - 解析失败则返回 NULL，**不**产生任何 term
   - 解析成功后**立即**调用 `kos_type_wellformed` + `kos_check`，未通过则释放并返回 NULL

3. **禁用直接构造**：
   - 将 `kos_mk_*` 移入 `core_internal.h`，仅 `core/` 内使用
   - 对外只提供 `kos_core_parse_file(path)`、`kos_core_parse_string(src)`
   - Runtime/Kernel 只能通过这两个接口获取 term，或通过 `kos_term_deserialize`（见下）

4. **反序列化必须校验**：
   - `kos_term_deserialize` 在恢复 `kos_term` 后，**必须**调用 `kos_type_wellformed` 和（在已知类型时）`kos_check`
   - 未通过则返回 NULL，不暴露非法 term

**优势**：无需 Rust，纯 C 可达成“Parser 即门卫”。

**局限**：C 无法防止恶意代码直接 `calloc` 伪造 `kos_term`；只能通过工程规范（如静态分析、代码审查）约束。

### 3.4 方案 C：混合——Rust Core + C 薄封装

- **Rust** 实现 Core 的解析、类型检查、归约、序列化
- **C** 仅提供薄封装：
  - `kos_core_load(path)` → 调用 Rust，返回不透明句柄
  - `kos_core_get_term(handle, id)` → 返回只读视图或拷贝
  - C 侧**不**持有 `kos_term` 的构造能力，仅能“读取”Rust 输出的结果

这样 C Runtime 完全无法构造非法 term，形式化保障由 Rust 侧保证。

---

## 四、具体实施建议（按优先级）

### 4.1 短期（可立即执行）

1. **反序列化校验**：在 `kos_term_deserialize` 末尾增加 `kos_type_wellformed` 检查，失败则 `kos_term_free` 并返回 NULL。
2. **构造器全覆盖**：对 `kos_mk_prop`、`kos_mk_val`、`kos_mk_time`、`kos_mk_id`、`kos_mk_pair` 增加一致性检查（如 pair 的 data/proof 类型与 sigma 的 domain/body 兼容）；不通过则返回 NULL。
3. **内部头文件**：将 `kos_term` 结构定义与 `kos_mk_*` 声明移入 `core/kos_core_internal.h`，仅 `src/core/` 链接；对外只暴露不透明指针和 `kos_check`、`kos_term_serialize` 等只读/校验接口。

### 4.2 中期（架构调整）

4. **Core DSL 设计**：定义 `.kos` 或 `.core` 语法，覆盖当前 `kos_mk_*` 能表达的所有构造。
5. **Parser 实现**：实现词法+语法分析器，**唯一**通过解析产生 `kos_term`。
6. **API 收缩**：废弃或隐藏直接构造 API，对外仅提供 `kos_core_parse_*`。

### 4.3 长期（形式化保障）

7. **Rust Core 选型**：若项目接受 Rust 依赖，采用方案 A 或 C，将 Core 迁至 Rust。
8. **Id 类型补全**：在 Core 中实现 `Id_A(a,b)`（Kos.tex 有定义，当前 C 实现缺失），用于因果追溯中的等价判断。
9. **元理论形式化**：对 Rust Core 的归约、类型保持等做形式化证明（如导出到 Coq），或至少建立 golden 测试覆盖关键性质。

---

## 五、与 CoC 的对照表

| 维度 | CoC | 建议的 KOS Core |
|------|-----|-----------------|
| 实现语言 | Rust | Rust（方案 A/C）或 C+Parser（方案 B） |
| 项的唯一来源 | Parser | Parser 或 FFI 调用 Rust |
| 类型检查时机 | 解析后立即 | 解析后立即，未通过则无输出 |
| 上下文 | `Context` 独立结构 | 独立 `Context`，不混入 term 链表 |
| 定义等价 | normalize + alpha | 明确实现并测试 |
| 反序列化 | 不适用（无持久化） | 必须校验 wellformed + check |
| 对外接口 | `typecheck_module` 等 | `parse_and_check` / `kos_core_parse_*` |

---

## 六、小结

- **C 实现无法在语言层面保证“非法类型不能创建”**，因 C 允许任意内存操作。
- **参考 CoC 的路径**：将 Core 独立为“形式化内核”，**Parser 作为唯一门卫**，类型检查通过才产出合法 Term。
- **推荐路线**：短期加强校验与封装；中期引入 Core DSL + Parser；长期若可行则用 Rust 重写 Core，通过 FFI 供 C Runtime 使用，从而在实现层面逼近类型论的形式化保障。
