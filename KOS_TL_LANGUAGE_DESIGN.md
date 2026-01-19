# KOS-TL 语言设计规范 / KOS-TL Language Design Specification

[中文](#中文) | [English](#english)

---

<a name="中文"></a>
## 中文

## 1. 语言概述

KOS-TL (Knowledge Operation System - Type Logic Language) 是一个函数式依赖类型语言，用于表述知识操作全生命周期。它结合了：

- **Coq/Lean 的证明能力**：支持依赖类型、证明构造、类型检查
- **KOS-TL 的领域特性**：知识对象原子化、确定性演化、计算反射
- **C 语言的执行效率**：编译为底层 C 代码，通过现有 KOS-TL 运行时执行

## 2. 语言设计原则

### 2.1 核心原则

1. **知识对象原子化**：所有数据必须携带证明 `<d, p>`
2. **类型即证明**：类型系统基于直觉主义类型论（ITT）
3. **确定性演化**：所有状态迁移必须可验证
4. **分层抽象**：支持 Core/Kernel/Runtime 三层抽象

### 2.2 设计目标

- **可读性**：类似 Coq/Lean 的数学风格语法
- **可验证性**：编译时进行完整的类型检查和证明验证
- **可执行性**：编译为高效的 C 代码
- **可扩展性**：支持领域本体和自定义类型

## 3. 语法设计

### 3.1 基础语法结构

```kos
-- 注释：单行注释使用 --
{- 多行注释 -}

-- 模块声明
module Finance where

-- 导入模块
import Core
import Manufacturing

-- 类型声明
type AccountID : U₁
type TransactionID : U₁
type Time : U₁
type Amount : U₁

-- 依赖类型声明
type TransactionEvent : Type₁
  = Σ(tx_id : TransactionID)
    Σ(from : AccountID)
    Σ(to : AccountID)
    Σ(amt : Amount)
    Σ(curr : Currency)
    Σ(t : Time)
    Prop

-- 函数定义（Π类型）
def isHighValue : Π(tx : TransactionEvent) → Prop
  = λ(tx : TransactionEvent) → 
    (tx.amt > 100000)

-- 证明构造
def highValueProof : Π(tx : TransactionEvent) → 
                     Π(p : isHighValue tx) → Prop
  = λ(tx : TransactionEvent) → 
    λ(p : isHighValue tx) → 
      p

-- 事件精化（Runtime Elaboration）
def elabTransaction : bitstream → Option TransactionEvent
  = λ(bs : bitstream) →
    match parseTransaction bs with
    | Some data → 
        let tx = mkTransaction data in
        if typeCheck tx TransactionEvent then
          Some tx
        else
          None
    | None → None

-- 状态演化（Kernel Layer）
def step : State → Event → Option State
  = λ(σ : State) → 
    λ(e : Event) →
      match validateEvent σ e with
      | Some proof → 
          let σ' = applyEvent σ e proof in
          Some σ'
      | None → None

-- 模式匹配
def analyzeTransaction : TransactionEvent → Result
  = λ(tx : TransactionEvent) →
    match tx with
    | <tx_id, from, to, amt, curr, t, _> →
        if amt > 100000 then
          Suspicious "High value transaction"
        else if from.country ≠ to.country then
          Suspicious "Cross-border transaction"
        else
          Normal
```

### 3.2 类型系统语法

```kos
-- 基础类型
U₀, U₁, U₂, ...        -- 计算轴 Universe
Type₀, Type₁, Type₂, ... -- 逻辑轴 Universe
Prop : Type₁           -- 命题类型

-- 类型构造器
A × B                  -- 积类型（非依赖）
Σ(x : A) → B           -- 依赖和类型
Π(x : A) → B           -- 依赖积类型
A + B                  -- 和类型（联合类型）
A → B                  -- 函数类型（Π的语法糖）

-- 类型注解
x : A                  -- 变量 x 的类型为 A
f : A → B              -- 函数 f 的类型
```

### 3.3 证明构造语法

```kos
-- 证明项构造
<d, p>                 -- Σ类型的构造（数据+证明）
λ(x : A) → t           -- λ抽象
f x                    -- 函数应用
let x = e₁ in e₂       -- let 绑定
match e with           -- 模式匹配
  | pattern₁ → e₁
  | pattern₂ → e₂

-- 证明策略（类似 Coq 的 tactics）
assumption             -- 使用假设
reflexivity            -- 自反性证明
apply f                -- 应用函数/引理
intro x                -- 引入变量
split                  -- 分解 Σ 类型
```

### 3.4 领域特定语法

```kos
-- 本体定义
ontology Finance where
  AccountID : U₁
  TransactionID : U₁
  TransactionEvent : Type₁ = Σ(...) → Prop
  
-- 事件定义
event TransactionEvent where
  tx_id : TransactionID
  from : AccountID
  to : AccountID
  amt : Amount
  curr : Currency
  t : Time
  proof : Prop

-- 状态定义
state SystemState where
  knowledge : Knowledge
  time : Time
  intents : List Intent

-- 规则定义
rule MoneyLaunderingRule : 
  Π(tx : TransactionEvent) →
  Π(acc : AccountID) →
  (isSuspicious tx acc) → Prop
  = λ(tx : TransactionEvent) →
    λ(acc : AccountID) →
    λ(p : isSuspicious tx acc) →
      traceMoneyLaundering tx acc
```

## 4. 编译器架构

### 4.1 编译流程

```
KOS-TL 源代码
    ↓
[词法分析] → Token 流
    ↓
[语法分析] → AST (抽象语法树)
    ↓
[类型检查] → 类型化的 AST
    ↓
[证明验证] → 验证后的 AST
    ↓
[代码生成] → C 代码
    ↓
[C 编译器] → 可执行文件
```

### 4.2 编译器模块

#### 4.2.1 词法分析器 (Lexer)

- 识别关键字、标识符、操作符、字面量
- 处理注释和空白
- 输出 Token 流

#### 4.2.2 语法分析器 (Parser)

- 构建抽象语法树 (AST)
- 处理优先级和结合性
- 错误恢复和报告

#### 4.2.3 类型检查器 (Type Checker)

- 双向类型检查（Bidirectional Type Checking）
- Universe 层级检查
- 依赖类型解析

#### 4.2.4 证明验证器 (Proof Verifier)

- 验证证明项的正确性
- 检查类型约束
- 生成证明对象

#### 4.2.5 代码生成器 (Code Generator)

- 将类型化的 AST 转换为 C 代码
- 调用底层 KOS-TL C API
- 优化代码生成

## 5. 与底层 C 实现的映射

### 5.1 类型映射

| KOS-TL 类型 | C 实现 |
|------------|--------|
| `U₀, U₁, ...` | `kos_term*` (KOS_U) |
| `Type₀, Type₁, ...` | `kos_term*` (KOS_TYPE) |
| `Prop` | `kos_term*` (KOS_PROP) |
| `Σ(x:A) → B` | `kos_term*` (KOS_SIGMA) |
| `Π(x:A) → B` | `kos_term*` (KOS_PI) |
| `A + B` | `kos_term*` (KOS_SUM) |
| `A → B` | `kos_term*` (KOS_PI) |

### 5.2 函数映射

| KOS-TL 构造 | C API 调用 |
|------------|-----------|
| `λ(x:A) → t` | `kos_mk_pi(...)` |
| `<d, p>` | `kos_mk_pair(d, p)` |
| `f x` | 函数调用 + 类型检查 |
| `match e with ...` | switch/case + 模式匹配 |

### 5.3 运行时映射

| KOS-TL 操作 | C 运行时 |
|------------|---------|
| `elab : bitstream → Event` | `kos_elab(...)` |
| `step : State → Event → State` | `kos_step(...)` |
| `materialize : State → unit` | `kos_materialize(...)` |

## 6. 标准库设计

### 6.1 Core 库

```kos
module Core where
  -- 基础类型
  type Unit : U₁
  type Bool : U₁
  type Nat : U₁
  type String : U₁
  
  -- 基础操作
  def id : Π(A : Type₁) → A → A
  def compose : Π(A B C : Type₁) → (B → C) → (A → B) → (A → C)
  
  -- 类型检查
  def typeCheck : Π(A : Type₁) → Π(x : A) → Prop
```

### 6.2 Kernel 库

```kos
module Kernel where
  -- 状态类型
  type State : Type₁
  type Event : Type₁
  
  -- 状态演化
  def step : State → Event → Option State
  def validate : State → Event → Option Proof
```

### 6.3 Runtime 库

```kos
module Runtime where
  -- 信号处理
  type bitstream : U₁
  def elab : bitstream → Option Event
  
  -- 物理存储
  def materialize : State → Unit
  def capture : Unit → bitstream
```

## 7. 示例程序

### 7.1 简单类型定义

```kos
module Example where

type Person : U₁
type Age : U₁

type PersonWithAge : Type₁
  = Σ(p : Person) Σ(a : Age) Prop

def mkPerson : Person → Age → PersonWithAge
  = λ(p : Person) → 
    λ(a : Age) → 
      <p, <a, proof>>
```

### 7.2 事件处理

```kos
module Finance where

def processTransaction : TransactionEvent → Result
  = λ(tx : TransactionEvent) →
    let suspicious = checkSuspicious tx in
    match suspicious with
    | Some proof → 
        traceMoneyLaundering tx proof
    | None → 
        normalProcessing tx
```

### 7.3 状态演化

```kos
module System where

def evolve : State → Event → Option State
  = λ(σ : State) → 
    λ(e : Event) →
      match validateEvent σ e with
      | Some p → 
          let σ' = applyEvent σ e p in
          Some σ'
      | None → None
```

## 8. 实现计划

### 阶段 1：语言规范设计
- [x] 语法规范定义
- [ ] 语义规范定义
- [ ] 类型系统规范
- [ ] 标准库规范

### 阶段 2：编译器实现
- [ ] 词法分析器
- [ ] 语法分析器
- [ ] 类型检查器
- [ ] 证明验证器
- [ ] 代码生成器

### 阶段 3：工具链开发
- [ ] 编译器命令行工具
- [ ] 构建系统集成
- [ ] 调试工具
- [ ] 性能分析工具

### 阶段 4：IDE 开发
- [ ] VS Code 扩展
- [ ] 语法高亮
- [ ] 代码补全
- [ ] 类型提示
- [ ] 错误诊断
- [ ] 证明辅助

## 9. 参考实现

- **Coq**: 证明助手和函数式语言
- **Lean**: 现代证明助手
- **Agda**: 依赖类型函数式语言
- **Idris**: 通用依赖类型语言
- **F***: 函数式验证语言

## 10. 下一步工作

1. 完善语法规范
2. 实现词法分析器
3. 实现语法分析器（使用解析器生成器或手写）
4. 实现类型检查器
5. 实现代码生成器
6. 开发 VS Code 扩展

---

<a name="english"></a>
## English

# KOS-TL Language Design Specification

## 1. Language Overview

KOS-TL (Knowledge Operation System - Type Logic Language) is a functional dependent type language for expressing the full lifecycle of knowledge operations. It combines:

- **Coq/Lean's proof capabilities**: Support for dependent types, proof construction, type checking
- **KOS-TL's domain features**: Knowledge object atomization, deterministic evolution, computational reflection
- **C language execution efficiency**: Compiles to low-level C code, executed through existing KOS-TL runtime

## 2. Language Design Principles

### 2.1 Core Principles

1. **Knowledge Object Atomization**: All data must carry proofs `<d, p>`
2. **Types as Proofs**: Type system based on Intuitionistic Type Theory (ITT)
3. **Deterministic Evolution**: All state transitions must be verifiable
4. **Layered Abstraction**: Support for Core/Kernel/Runtime three-layer abstraction

### 2.2 Design Goals

- **Readability**: Mathematical-style syntax similar to Coq/Lean
- **Verifiability**: Complete type checking and proof verification at compile time
- **Executability**: Compiles to efficient C code
- **Extensibility**: Support for domain ontologies and custom types

## 3. Syntax Design

### 3.1 Basic Syntax Structure

```kos
-- Comments: single-line comments use --
{- Multi-line comments -}

-- Module declaration
module Finance where

-- Import modules
import Core
import Manufacturing

-- Type declarations
type AccountID : U₁
type TransactionID : U₁
type Time : U₁
type Amount : U₁

-- Dependent type declaration
type TransactionEvent : Type₁
  = Σ(tx_id : TransactionID)
    Σ(from : AccountID)
    Σ(to : AccountID)
    Σ(amt : Amount)
    Σ(curr : Currency)
    Σ(t : Time)
    Prop

-- Function definition (Π type)
def isHighValue : Π(tx : TransactionEvent) → Prop
  = λ(tx : TransactionEvent) → 
    (tx.amt > 100000)

-- Proof construction
def highValueProof : Π(tx : TransactionEvent) → 
                     Π(p : isHighValue tx) → Prop
  = λ(tx : TransactionEvent) → 
    λ(p : isHighValue tx) → 
      p

-- Event elaboration (Runtime Elaboration)
def elabTransaction : bitstream → Option TransactionEvent
  = λ(bs : bitstream) →
    match parseTransaction bs with
    | Some data → 
        let tx = mkTransaction data in
        if typeCheck tx TransactionEvent then
          Some tx
        else
          None
    | None → None

-- State evolution (Kernel Layer)
def step : State → Event → Option State
  = λ(σ : State) → 
    λ(e : Event) →
      match validateEvent σ e with
      | Some proof → 
          let σ' = applyEvent σ e proof in
          Some σ'
      | None → None

-- Pattern matching
def analyzeTransaction : TransactionEvent → Result
  = λ(tx : TransactionEvent) →
    match tx with
    | <tx_id, from, to, amt, curr, t, _> →
        if amt > 100000 then
          Suspicious "High value transaction"
        else if from.country ≠ to.country then
          Suspicious "Cross-border transaction"
        else
          Normal
```

### 3.2 Type System Syntax

```kos
-- Basic types
U₀, U₁, U₂, ...        -- Computational axis Universe
Type₀, Type₁, Type₂, ... -- Logical axis Universe
Prop : Type₁           -- Proposition type

-- Type constructors
A × B                  -- Product type (non-dependent)
Σ(x : A) → B           -- Dependent sum type
Π(x : A) → B           -- Dependent product type
A + B                  -- Sum type (union type)
A → B                  -- Function type (syntactic sugar for Π)

-- Type annotations
x : A                  -- Variable x has type A
f : A → B              -- Function f's type
```

### 3.3 Proof Construction Syntax

```kos
-- Proof term construction
<d, p>                 -- Σ type construction (data + proof)
λ(x : A) → t           -- λ abstraction
f x                    -- Function application
let x = e₁ in e₂       -- let binding
match e with           -- Pattern matching
  | pattern₁ → e₁
  | pattern₂ → e₂

-- Proof tactics (similar to Coq's tactics)
assumption             -- Use assumption
reflexivity            -- Reflexivity proof
apply f                -- Apply function/lemma
intro x                -- Introduce variable
split                  -- Decompose Σ type
```

### 3.4 Domain-Specific Syntax

```kos
-- Ontology definition
ontology Finance where
  AccountID : U₁
  TransactionID : U₁
  TransactionEvent : Type₁ = Σ(...) → Prop
  
-- Event definition
event TransactionEvent where
  tx_id : TransactionID
  from : AccountID
  to : AccountID
  amt : Amount
  curr : Currency
  t : Time
  proof : Prop

-- State definition
state SystemState where
  knowledge : Knowledge
  time : Time
  intents : List Intent

-- Rule definition
rule MoneyLaunderingRule : 
  Π(tx : TransactionEvent) →
  Π(acc : AccountID) →
  (isSuspicious tx acc) → Prop
  = λ(tx : TransactionEvent) →
    λ(acc : AccountID) →
    λ(p : isSuspicious tx acc) →
      traceMoneyLaundering tx acc
```

## 4. Compiler Architecture

### 4.1 Compilation Pipeline

```
KOS-TL Source Code
    ↓
[Lexical Analysis] → Token stream
    ↓
[Syntax Analysis] → AST (Abstract Syntax Tree)
    ↓
[Type Checking] → Typed AST
    ↓
[Proof Verification] → Verified AST
    ↓
[Code Generation] → C code
    ↓
[C Compiler] → Executable
```

### 4.2 Compiler Modules

#### 4.2.1 Lexer

- Recognize keywords, identifiers, operators, literals
- Handle comments and whitespace
- Output token stream

#### 4.2.2 Parser

- Build abstract syntax tree (AST)
- Handle precedence and associativity
- Error recovery and reporting

#### 4.2.3 Type Checker

- Bidirectional type checking
- Universe level checking
- Dependent type resolution

#### 4.2.4 Proof Verifier

- Verify correctness of proof terms
- Check type constraints
- Generate proof objects

#### 4.2.5 Code Generator

- Convert typed AST to C code
- Call underlying KOS-TL C API
- Optimize code generation

## 5. Mapping to Underlying C Implementation

### 5.1 Type Mapping

| KOS-TL Type | C Implementation |
|------------|--------|
| `U₀, U₁, ...` | `kos_term*` (KOS_U) |
| `Type₀, Type₁, ...` | `kos_term*` (KOS_TYPE) |
| `Prop` | `kos_term*` (KOS_PROP) |
| `Σ(x:A) → B` | `kos_term*` (KOS_SIGMA) |
| `Π(x:A) → B` | `kos_term*` (KOS_PI) |
| `A + B` | `kos_term*` (KOS_SUM) |
| `A → B` | `kos_term*` (KOS_PI) |

### 5.2 Function Mapping

| KOS-TL Construction | C API Call |
|------------|-----------|
| `λ(x:A) → t` | `kos_mk_pi(...)` |
| `<d, p>` | `kos_mk_pair(d, p)` |
| `f x` | Function call + type checking |
| `match e with ...` | switch/case + pattern matching |

### 5.3 Runtime Mapping

| KOS-TL Operation | C Runtime |
|------------|---------|
| `elab : bitstream → Event` | `kos_elab(...)` |
| `step : State → Event → State` | `kos_step(...)` |
| `materialize : State → unit` | `kos_materialize(...)` |

## 6. Standard Library Design

### 6.1 Core Library

```kos
module Core where
  -- Basic types
  type Unit : U₁
  type Bool : U₁
  type Nat : U₁
  type String : U₁
  
  -- Basic operations
  def id : Π(A : Type₁) → A → A
  def compose : Π(A B C : Type₁) → (B → C) → (A → B) → (A → C)
  
  -- Type checking
  def typeCheck : Π(A : Type₁) → Π(x : A) → Prop
```

### 6.2 Kernel Library

```kos
module Kernel where
  -- State types
  type State : Type₁
  type Event : Type₁
  
  -- State evolution
  def step : State → Event → Option State
  def validate : State → Event → Option Proof
```

### 6.3 Runtime Library

```kos
module Runtime where
  -- Signal processing
  type bitstream : U₁
  def elab : bitstream → Option Event
  
  -- Physical storage
  def materialize : State → Unit
  def capture : Unit → bitstream
```

## 7. Example Programs

### 7.1 Simple Type Definition

```kos
module Example where

type Person : U₁
type Age : U₁

type PersonWithAge : Type₁
  = Σ(p : Person) Σ(a : Age) Prop

def mkPerson : Person → Age → PersonWithAge
  = λ(p : Person) → 
    λ(a : Age) → 
      <p, <a, proof>>
```

### 7.2 Event Handling

```kos
module Finance where

def processTransaction : TransactionEvent → Result
  = λ(tx : TransactionEvent) →
    let suspicious = checkSuspicious tx in
    match suspicious with
    | Some proof → 
        traceMoneyLaundering tx proof
    | None → 
        normalProcessing tx
```

### 7.3 State Evolution

```kos
module System where

def evolve : State → Event → Option State
  = λ(σ : State) → 
    λ(e : Event) →
      match validateEvent σ e with
      | Some p → 
          let σ' = applyEvent σ e p in
          Some σ'
      | None → None
```

## 8. Implementation Plan

### Phase 1: Language Specification Design
- [x] Syntax specification definition
- [ ] Semantics specification definition
- [ ] Type system specification
- [ ] Standard library specification

### Phase 2: Compiler Implementation
- [ ] Lexer
- [ ] Parser
- [ ] Type checker
- [ ] Proof verifier
- [ ] Code generator

### Phase 3: Toolchain Development
- [ ] Compiler command-line tool
- [ ] Build system integration
- [ ] Debugging tools
- [ ] Performance analysis tools

### Phase 4: IDE Development
- [ ] VS Code extension
- [ ] Syntax highlighting
- [ ] Code completion
- [ ] Type hints
- [ ] Error diagnostics
- [ ] Proof assistance

## 9. Reference Implementations

- **Coq**: Proof assistant and functional language
- **Lean**: Modern proof assistant
- **Agda**: Dependent type functional language
- **Idris**: General-purpose dependent type language
- **F***: Functional verification language

## 10. Next Steps

1. Complete syntax specification
2. Implement lexer
3. Implement parser (using parser generator or handwritten)
4. Implement type checker
5. Implement code generator
6. Develop VS Code extension









