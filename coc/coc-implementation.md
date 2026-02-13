# CoC 工程实现与架构说明

本文档基于独立的 `coc` 工程（`d:\post\KOS\typechecker\coc`），从**架构、编译流程与工程文件体系**三个角度，对 Calculus of Constructions（CoC）类型检查器的实现进行说明。

---

## 一、工程整体结构

### 1.1 目录结构概览

工程根目录下的主要内容：

- `Cargo.toml`：Rust 包配置，声明 crate 名称、依赖和构建脚本
- `build.rs`：构建脚本，用于驱动 LALRPOP 生成解析器代码
- `src/`：核心实现代码
  - `main.rs`：命令行入口，可执行程序
  - `lib.rs`：库入口，对外暴露高层 API
  - `ast.rs`：抽象语法树定义（Term、Declaration、Module 等）
  - `lexer.rs`：基于 `logos` 的词法分析器
  - `parse.rs` + `parser.lalrpop`：基于 LALRPOP 的语法分析器封装
  - `typecheck.rs`：双向类型检查器核心逻辑
  - `context.rs`：类型环境与上下文管理
  - `unification.rs`：基础统一算法（Unifier）
  - `solver.rs`：高级约束求解器（支持隐式参数、Miller pattern 等）
  - `universe_solver.rs`：宇宙层级约束求解
  - `alpha.rs`：alpha 等价与 De Bruijn 归约
  - `term_utils.rs`：各种 term 操作与工具函数
  - `diagnostics.rs`：错误诊断与美化输出（基于 `ariadne`）
  - `errors.rs`：统一错误类型定义（CocError / TypeError / ParseError）
- `examples/`：`.coc` 示例源码（语法展示、特性演示）
  - `syntax_showcase.coc`：覆盖完整 CoC 语法特性的综合示例
  - 其它若干 debug / pattern / implicit / universe 等专题示例
- `tests/`：集成测试与 golden 测试
  - `*_tests.rs`：基于 `cargo test` 的 Rust 测试代码
  - `*.coc` / `*.out`：测试输入与期望输出（golden files）
- `target/`：构建产物（由 `cargo build` 生成，通常无需关心）

整体上，`src/` 是编译与类型检查链路的核心；`examples/` 对应用户视角下的 CoC 程序；`tests/` 则是验证实现正确性的回归测试集合。

---

## 二、编译与类型检查流程

这一部分描述**从 `.coc` 源文件到类型检查完成**的完整流水线。

### 2.1 自顶向下的视角

以命令行为入口，完整路径如下：

1. 命令行解析（`src/main.rs`）
2. 读取 `.coc` 文件内容
3. 词法分析（`lexer.rs`，基于 `logos`）
4. 语法分析（`parser.lalrpop` 生成的 LALRPOP 解析器，经 `parse.rs` 封装）
5. 构建 AST（`ast.rs` 中的 `Module` / `Term` 等）
6. 类型检查（`typecheck.rs`，依赖 `context`/`unifier`/`solver` 等）
7. 诊断输出（`diagnostics.rs` + `errors.rs`）

### 2.2 main.rs：命令行入口与模式

`main.rs` 将 coc 同时暴露为**可执行工具**和**库**：

- 支持的命令：
  - `check <file>`：类型检查一个 `.coc` 源文件
  - `parse <file>`：仅解析并打印 AST
  - `term <expr>`：解析并类型检查单个表达式
  - `repl`：交互式 REPL，支持增量声明与表达式交互
- 典型的 `check` 流程：
  1. `fs::read_to_string` 读取文件内容
  2. 构建 `DiagnosticReporter`，用于错误展示
  3. 通过 `Parser::new().parse_module(&content)` 获得 `Module`
  4. `TypeChecker::new().check_module(&module)` 完成类型检查
  5. 打印“Parsed module with N declarations / ✓ type checks successfully!”或错误

`main.rs` 主要负责**参数解析和用户交互**，业务逻辑集中在 `lib.rs` 及各子模块中。

### 2.3 lib.rs：库 API 与高层封装

`lib.rs` 将内部模块组合成对外友好的 API，主要包括：

- `typecheck_module(source: &str, filename: &str)`：从源代码字符串构建 `Parser` 和 `TypeChecker` 完成模块级检查
- `check_term(source: &str)`：解析单个 `Term` 并在空上下文中进行类型推断，返回 `"term : type"` 字符串
- `check_module_file(filename: &str)`：读取文件、解析模块、检查、拼装易读的结果字符串
- `run_golden_test(...)` & `run_module_golden_test(...)`：用于测试框架，将实际输出与 `.out` golden 文件对比

这使得 coc 既可以作为 CLI 工具使用，也可以作为一个 library 嵌入到其他 Rust 项目中。

### 2.4 词法分析：lexer.rs

`lexer.rs` 使用 `logos` 派生出的 `Token` 枚举来描述所有词法单元，包括：

- 关键字：`def`, `axiom`, `inductive`, `structure`, `match`, `with`, `if`, `then`, `else` 等
- 类型关键字：`Type`, `Prop`, `Sort`, `max`, `imax`
- 操作符：`->`, `=>`, `:=`, `:`, `=`, `|`, `*`, `+`, `-` 等
- 括号与分隔符：`()`, `{}`, `⟨⟩`, `,`, `.`
- 标识符与字面量：
  - `Identifier(String)`：变量、常量、构造子名称
  - `Number(Option<i64>)` / `String` / `Char`
- 空白和注释会被 `logos::skip` 丢弃：
  - 空白：空格、制表符、`\n`, `\r`, `\f`
  - 行注释：`-- ...`
  - 块注释：`/* ... */`

`Lexer` 封装了 `logos::Lexer`，向上层（`parse.rs`）提供 `next_token()`、`span()` 等接口，用于生成带位置信息的 token 流。

### 2.5 语法分析：parser.lalrpop + parse.rs

语法分析基于 LALRPOP：

- `parser.lalrpop`：定义 CoC 的文法，生成 `parser::ModuleParser` 和 `parser::TermParser`
- `build.rs`：在构建阶段运行 `lalrpop`，生成对应的 Rust 代码
- `parse.rs`：
  - 封装 `Parser` 结构，内部持有 `ModuleParser` 和 `TermParser`
  - 将 `Lexer` 输出的 token 流转换为 LALRPOP 需要的 `(start, Token, end)` 三元组
  - 提供：
    - `parse_module(&self, input: &str) -> CocResult<Module>`
    - `parse_term(&self, input: &str) -> CocResult<Term>`
  - 负责将 LALRPOP 的错误（`ParseError<usize, Token, LexicalError>`）转换为本项目的 `ParseError`（`errors.rs`）

生成的 AST 定义在 `ast.rs` 中，包括：

- `Term`：`Var`, `App`, `Abs`, `Pi`, `Sort`, `Let`, `Match`, `Constructor`, `Const`, `Meta`, `Proj`, `Sigma`, `Pair`, `Fst`, `Snd` 等
- `Declaration`：`Definition`, `Axiom`, `Inductive`, `Structure`
- `Module`：一组 `Declaration`
- `Universe` / `UniverseConstraint` / `UniverseContext`：用于宇宙层级与多态

### 2.6 类型检查：typecheck.rs

`TypeChecker` 是整个工程的核心，负责：

- 模块级检查：
  - `check_module(&mut self, module: &Module) -> TypeResult<Context>`
  - 遍历所有声明，增量更新 `Context`
- 声明级检查（`check_declaration`）：
  - `Definition`：处理带/不带宇宙参数的函数定义，支持递归
  - `Axiom`：仅检查类型是「合法类型」（`check_is_type`）
  - `Inductive`：注册归纳类型本身及其构造子，处理 universe 参数
  - `Structure`：生成投影函数（`Struct.field`）的类型
- 表达式级推断与检查：
  - `infer(&mut self, term, ctx) -> TypeResult<Term>`：推断类型
  - `check(&mut self, term, ty, ctx) -> TypeResult<()>`：在给定类型下检查
  - 支持依赖函数类型（`Pi`）、`Let`、`Match`、`Sigma`、`Pair` 等
- 定义等价（definitional equality）：
  - `definitionally_equal`：通过 `normalize`（beta/eta/let 归约）+ `alpha_equivalent` 判断
  - 若存在 universe meta，则调用 `Solver` + `UniverseSolver` 生成与解决约束
- 隐式参数与约束求解：
  - 在函数应用时（`infer_app` / `infer_app_with_constraints`），对隐式参数生成 meta 和约束
  - 交由 `solver.rs` 中的 `Solver` 统一求解

`TypeChecker` 依赖多个子模块协作：`Context` 提供符号环境，`Unifier` 处理 term 级统一，`Solver` 处理复杂约束，`UniverseSolver` 管理宇宙层级，`term_utils` 提供替换与复杂构造处理。

### 2.7 上下文与符号表：context.rs

`Context` 负责管理类型检查所需的所有环境信息：

- 变量绑定：`bindings: HashMap<String, Term>`
- 宇宙变量与约束：`universe_context: UniverseContext`
- 构造子：`constructors: HashMap<String, Term>`
- Axiom：`axioms: HashMap<String, Term>`
- 带宇宙参数的定义：`definitions: HashMap<String, Definition>`

提供的关键接口包括：

- `extend` / `extend_many` / `remove`：变量作用域管理
- `lookup` / `lookup_axiom` / `lookup_constructor` / `lookup_definition`：符号查找
- `add_constructor` / `add_axiom` / `add_definition`：注册新符号
- `extend_universe_many` / `add_universe_constraint` / `fresh_universe_var`：宇宙变量管理

`ContextError` 用于描述上下文相关错误（未绑定变量、未绑定构造子等），会被包装进 `TypeError`。

### 2.8 统一与约束求解：unification.rs + solver.rs + universe_solver.rs

这三部分共同支撑 CoC 中复杂的类型等价判定和隐式参数推断：

- `unification.rs`：实现一个较“传统”的统一器 `Unifier`，用于：
  - Term 级统一（含 meta 变量）
  - Universe 级统一（基于 `Substitution` 替换 `Universe`）
  - 提供 occurs-check，避免自引用类型
- `solver.rs`：实现更强大的约束求解器 `Solver`：
  - 约束类型 `Constraint`：`Unify`, `HasType`, `UnifyUniverse`, `UniverseLevel`, `Delayed`
  - 支持约束队列、优先级、延迟与唤醒机制
  - 支持 Miller pattern（`?M x1 ... xn = t`）等高级模式的特殊处理
  - 生成并应用 `Substitution`，更新所有约束
- `universe_solver.rs`：专门处理 `UniverseConstraint`：
  - 简化版的宇宙层级统一（`Equal`, `LessEq`）
  - 管理 `Universe::ScopedVar` 与 `Universe::Meta` 的替换
  - 用于 `TypeChecker::pi_rule` 和 universe 多态的约束生成

### 2.9 错误与诊断：errors.rs + diagnostics.rs

错误系统分层设计：

- `CocError`：顶层统一错误类型（词法/语法/类型/IO）
- `TypeError`：类型检查阶段的细致错误（上下文错误、统一错误、模式匹配错误、内部错误等）
- `ParseError`：LALRPOP 解析阶段错误（非法 token、意外 EOF、词法错误）

`diagnostics.rs` 使用 `ariadne` 将错误渲染为带颜色与源代码片段的终端输出，对用户更友好。

---

## 三、工程文件体系与构建流程

### 3.1 Cargo.toml 与依赖管理

独立工程的 `Cargo.toml` 主要内容：

- **基础信息：**
  - `name = "coc"`
  - `edition = "2021"`
- **依赖：**
  - `lalrpop-util` / `lalrpop`：语法分析器生成
  - `logos`：词法分析
  - `ariadne`：错误诊断输出
  - `thiserror`：错误类型派生宏
- **构建依赖：**
  - `lalrpop` 作为 `build-dependencies`，在编译前生成解析器代码

在从 `typechecker-zoo` 分离为独立工程时，移除了对 workspace lints 的继承（`[lints] workspace = true`），使工程可以单独通过 `cargo build` 构建。

### 3.2 build.rs：LALRPOP 构建脚本

`build.rs` 的职责是：

- 在 `cargo build` 时自动调用 `lalrpop::process_root()` 等 API
- 根据 `src/parser.lalrpop` 生成对应的 `parser.rs` 模块代码
- 确保 `parser_impl::parser` 可以被 `parse.rs` 使用

对用户而言，这一步是透明的，只需要保证本地已安装 Rust 工具链，构建时会自动完成。

### 3.3 examples/：语法与特性示例

`examples/` 目录存放了大量 `.coc` 文件，用于：

- 演示语言特性（依赖类型、隐式参数、Universe、多态、Sigma、结构体等）
- 作为手动测试或文档示例的来源

其中关键文件：

- `syntax_showcase.coc`：覆盖最完整的语法特性的示例文件，适合作为“语言参考”的入口
- `final_demo.coc` / `implicit_comprehensive.coc` 等：侧重特定特性的演示

可以通过：

```bash
cargo run -- check \"examples/syntax_showcase.coc\"
```

直接在独立工程中验证与体验 CoC 的语法与类型行为。

### 3.4 tests/：集成测试与 golden 测试

`tests/` 目录结合 Rust 的测试框架和 golden 文件实现：

- `*_tests.rs`：调用 `lib.rs` 中的 API（如 `run_golden_test`）对 `.coc` 输入文件进行自动化测试
- `*.coc` + `*.out`：golden 测试
  - `.coc`：输入文件（CoC 程序）
  - `.out`：期望的标准输出（例如每行的 `term : type` 或错误消息）

通过：

```bash
cargo test
```

可以跑通所有内建测试，保证：

- 解析器与类型检查器在不同特性组合下的稳定性
- 隐式参数、Universe 多态、Sigma 类型、模式匹配等高级特性的行为没有回归

---

## 四、工程使用方式与扩展建议

### 4.1 作为命令行工具使用

在 `d:\post\KOS\typechecker\coc` 下：

```bash
# 构建
cargo build

# 类型检查单个文件
cargo run -- check \"examples/syntax_showcase.coc\"

# 启动 REPL
cargo run -- repl

# 解析并打印 AST
cargo run -- parse \"examples/syntax_showcase.coc\"
```

### 4.2 作为库集成到其他 Rust 项目

在其他项目的 `Cargo.toml` 中添加依赖（本地路径示例）：

```toml
[dependencies]
coc = { path = \"../coc\" }
```

然后在代码中使用：

```rust
use coc::{check_term, typecheck_module};
```

### 4.3 扩展与维护建议

- **新增语法：**
  - 修改 `parser.lalrpop` 和 `lexer.rs` 以支持新的关键字或语法构造
  - 在 `ast.rs` 中添加新的 `Term`/`Declaration` 变体
  - 在 `typecheck.rs` 中实现新的推断/检查规则
- **新增类型构造：**
  - 如果是新的数据类型形态（如记录、GADT），需要同步更新：
    - `ast.rs`（抽象语法）
    - `typecheck.rs`（检查规则）
    - `unification.rs` / `solver.rs`（必要时）
    - `term_utils.rs`（替换与分析工具）
- **引入新特性（如更强的 Universe 系统、Coinductive 类型等）：**
  - 考虑先在 `examples/` 中写出期望语法与行为
  - 在 `tests/` 中加入 golden 测试，驱动实现

---

## 五、小结

独立的 `coc` 工程提供了一个结构清晰、模块分明的 CoC 类型检查器实现，主要特点是：

- 词法/语法/类型检查清晰分层，便于理解和扩展
- 通过 `Context`、`Unifier`、`Solver` 和 `UniverseSolver` 等模块协作，实现了较完整的依赖类型与宇宙多态支持
- `examples/` 与 `tests/` 提供了丰富的使用与验证样例

`coc-implementation.md` 可作为后续阅读代码、扩展特性或重用该类型检查器时的架构参考文档。

