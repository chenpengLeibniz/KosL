# Calculus of Inductive Constructions (CIC) 内核

## 概述

本模块实现了完整的 Calculus of Inductive Constructions (CIC) 内核，参考了 [typechecker-zoo](https://github.com/sdiehl/typechecker-zoo/) 项目的设计思路。

CIC 是 Calculus of Constructions (CoC) 的扩展，增加了：
- **归纳类型**（Inductive Types）
- **构造子**（Constructors）
- **消除子**（Eliminators）
- **模式匹配**（Pattern Matching）
- **递归定义**（Recursive Definitions）

## 核心概念

### 1. 双向类型检查

CIC内核使用双向类型检查算法（Bidirectional Type Checking）：

- **检查模式**（Check Mode）：`Γ ⊢ M : A` - 给定类型，检查项是否符合
- **推断模式**（Infer Mode）：`Γ ⊢ M : ?` - 推断项的类型

### 2. 环境（Context）

环境 `Γ` 存储变量绑定、定义和归纳类型定义：

```c
Γ = x₁ : A₁, x₂ : A₂ := M₂, Inductive I : Type := ...
```

### 3. 归纳类型

归纳类型定义包括：
- 类型名称和类型本身
- 类型参数（Π类型链）
- 构造子列表
- 严格正性检查

示例：
```kos
Inductive Nat : U₁ :=
  | zero : Nat
  | succ : Nat → Nat
```

### 4. 归约

CIC支持两种归约：
- **β-归约**：`(λx:A.M) N → M[x := N]`
- **ι-归约**：模式匹配归约，用于归纳类型

## 数据结构

### cic_core

CIC内核的主要状态结构：

```c
typedef struct {
    cic_context* ctx;        // 当前环境
    bool has_error;          // 是否有错误
    char* error_message;     // 错误消息
    ASTNode* error_node;     // 出错的AST节点
    int max_universe_level;  // 最大Universe层级
} cic_core;
```

### cic_context

环境结构（链表实现）：

```c
typedef struct {
    cic_entry* head;         // 环境条目链表
    size_t length;           // 环境长度
} cic_context;
```

### cic_inductive_def

归纳类型定义：

```c
typedef struct {
    char* name;              // 归纳类型名称
    kos_term* type;          // 归纳类型本身
    kos_term* parameters;    // 类型参数
    cic_constructor* constructors; // 构造子数组
    size_t constructor_count;
    bool is_positive;        // 是否为严格正类型
} cic_inductive_def;
```

## API 使用示例

### 创建CIC内核

```c
cic_core* core = cic_core_create();
if (!core) {
    // 错误处理
}
```

### 添加变量到环境

```c
kos_term* nat_type = kos_mk_universe_computational(1);
cic_context_add_var(core->ctx, "n", nat_type);
```

### 类型检查

```c
kos_term* term = ...;  // 要检查的项
kos_term* type = ...;  // 期望的类型

if (cic_check(core, term, type)) {
    printf("类型检查通过\n");
} else {
    printf("类型检查失败: %s\n", cic_get_error(core));
}
```

### 创建归纳类型

```c
kos_term* nat_sort = kos_mk_universe_computational(1);
cic_inductive_def* nat = cic_inductive_create("Nat", nat_sort);

// 添加构造子
kos_term* zero_type = nat_sort;  // zero : Nat
cic_inductive_add_constructor(nat, "zero", zero_type);

kos_term* succ_type = kos_mk_pi("n", nat_sort, nat_sort);  // succ : Nat → Nat
cic_inductive_add_constructor(nat, "succ", succ_type);

// 检查归纳类型
if (cic_check_inductive(core, nat)) {
    // 添加到环境
    cic_context_add_inductive(core->ctx, nat);
}
```

## 实现状态

### 已完成
- ✅ 基础数据结构定义
- ✅ 环境操作（创建、添加、查找）
- ✅ 错误处理框架

### 进行中
- 🔄 双向类型检查算法
- 🔄 归约实现（β、ι）

### 待实现
- ⏳ 完整的类型推断
- ⏳ 模式匹配类型检查
- ⏳ 递归定义检查（严格正性）
- ⏳ AST转换
- ⏳ Universe层级检查

## 参考

- [typechecker-zoo](https://github.com/sdiehl/typechecker-zoo/) - 现代类型检查算法实现集合
- [Calculus of Inductive Constructions](https://coq.inria.fr/refman/language/cic.html) - Coq文档
- [A Mechanical Formalization of Higher-Ranked Polymorphic Type Inference](https://www.microsoft.com/en-us/research/publication/a-mechanical-formalization-of-higher-ranked-polymorphic-type-inference/) - 类型推断的形式化
