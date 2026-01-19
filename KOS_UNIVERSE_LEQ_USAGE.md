# `kos_universe_leq()` 函数使用分析 / `kos_universe_leq()` Function Usage Analysis

[中文](#中文) | [English](#english)

---

<a name="中文"></a>
## 中文

## 函数定义

**位置**：`src/core/universe.c:62`

**函数签名**：
```c
bool kos_universe_leq(universe_info u1, universe_info u2);
```

**功能**：
- 检查 Universe 层级关系：`u1 ≤ u2`
- 支持双轴系统（计算轴 U_i 和逻辑轴 Type_i）
- 实现 Universe 提升规则

## 调用位置

### 唯一调用点：`kos_check()` 函数

**位置**：`src/core/type_checker.c:90`

**调用场景**：在类型检查过程中，当检查 Universe 类型（KOS_U 或 KOS_TYPE）时

**代码上下文**：
```c
bool kos_check(kos_term* ctx, kos_term* term, kos_term* type) {
    // ... 获取 Universe 信息
    universe_info term_info = kos_get_universe_info(term);
    universe_info type_info = kos_get_universe_info(type);
    
    // ... 根据类型进行分情况检查
    switch (type->kind) {
        // ... 其他类型检查 ...
        
        case KOS_U:
        case KOS_TYPE:
            // Universe类型的检查：比较Universe层级
            if (term->kind == type->kind) {
                // 如果都是Universe类型，直接比较轴和层级
                return (term->data.universe.axis == type->data.universe.axis &&
                        term->data.universe.level == type->data.universe.level);
            }
            // 如果term不是Universe类型，检查是否可以提升到该Universe层级
            return kos_universe_leq(term_info, type_info);  // ← 唯一调用点
            
        // ... 其他类型检查 ...
    }
}
```

## 使用时机

### 触发条件

`kos_universe_leq()` 在以下情况下被调用：

1. **类型检查的目标类型是 Universe 类型**（`type->kind == KOS_U` 或 `KOS_TYPE`）
2. **被检查的 term 不是 Universe 类型**（`term->kind != KOS_U && term->kind != KOS_TYPE`）

### 具体场景

#### 场景1：检查非Universe类型是否可以提升到Universe类型

**示例**：
```c
// 假设有一个类型定义：Type_1（逻辑轴，层级1）
kos_term* type = kos_mk_universe_logical(1);  // Type_1

// 有一个值类型：U_0（计算轴，层级0）
kos_term* term = kos_mk_val("some_value");  // U_0

// 类型检查：term : type？
bool result = kos_check(NULL, term, type);
// 内部会调用：kos_universe_leq(term_info, type_info)
// term_info = {UNIVERSE_COMPUTATIONAL, 0}
// type_info = {UNIVERSE_LOGICAL, 1}
// 检查：U_0 是否可以提升到 Type_1？
// 规则：U_i : Type_{i+1}，所以 U_0 : Type_1 ✓
```

#### 场景2：检查基础类型是否可以提升到Universe类型

**示例**：
```c
// 类型：Type_2（逻辑轴，层级2）
kos_term* type = kos_mk_universe_logical(2);  // Type_2

// 值：BatchID（基础类型，推断为 U_0）
kos_term* term = kos_mk_id("BATCH001");  // U_0

// 类型检查
bool result = kos_check(NULL, term, type);
// 内部调用：kos_universe_leq({UNIVERSE_COMPUTATIONAL, 0}, {UNIVERSE_LOGICAL, 2})
// 检查：U_0 是否可以提升到 Type_2？
// 规则：U_0 : Type_1，但 Type_1 < Type_2，所以 U_0 : Type_2 ✓
```

## 实现逻辑

`kos_universe_leq()` 实现了以下规则：

### 1. 相同轴：直接比较层级
```c
if (u1.axis == u2.axis) {
    return u1.level <= u2.level;
}
```
- `U_0 ≤ U_1` ✓
- `Type_1 ≤ Type_2` ✓
- `U_2 ≤ U_1` ✗

### 2. 跨轴提升：计算轴提升到逻辑轴
```c
if (u1.axis == UNIVERSE_COMPUTATIONAL && u2.axis == UNIVERSE_LOGICAL) {
    return u1.level + 1 <= u2.level;
}
```
- `U_0 : Type_1` ✓ (0 + 1 = 1 ≤ 1)
- `U_0 : Type_2` ✓ (0 + 1 = 1 ≤ 2)
- `U_1 : Type_1` ✓ (1 + 1 = 2 ≤ 1) ✗ 实际上应该是 U_1 : Type_2
- `U_1 : Type_2` ✓ (1 + 1 = 2 ≤ 2)

### 3. Prop嵌入：命题嵌入到数据轴
```c
if (u1.axis == UNIVERSE_LOGICAL && u1.level == 1 && 
    u2.axis == UNIVERSE_COMPUTATIONAL && u2.level == 1) {
    return true;
}
```
- `Prop : U_1` ✓

## 调用链

```
运行时事件精化 (runtime_elab.c)
    ↓
kos_ontology_mk_type_instance()
    ↓
kos_check()  ← 核心类型检查
    ↓
case KOS_U / KOS_TYPE:
    ↓
kos_universe_leq()  ← 唯一调用点
```

## 实际应用

### 在类型检查中的应用

当系统进行类型检查时，如果遇到以下情况：

1. **检查一个基础类型值是否符合Universe类型**
   - 例如：检查 `BatchID` 值是否符合 `Type_1` 类型
   - 系统会调用 `kos_universe_leq()` 判断是否可以提升

2. **检查一个复合类型是否符合Universe类型**
   - 例如：检查 `Σ(x:A).B` 类型是否符合 `Type_2` 类型
   - 系统会获取复合类型的Universe信息，然后调用 `kos_universe_leq()` 判断

### 在双轴系统中的作用

`kos_universe_leq()` 是双轴世界（Dual-Axis Universe System）的核心机制：

- **计算轴（U_i）**：用于计算数据
- **逻辑轴（Type_i）**：用于逻辑类型
- **提升规则**：`U_i : Type_{i+1}`（计算轴可以提升到逻辑轴）

这个函数确保了类型检查时能够正确处理跨轴的Universe层级关系。

## 总结

`kos_universe_leq()` 函数：

1. **唯一调用点**：在 `kos_check()` 函数中，当检查 Universe 类型时
2. **触发条件**：目标类型是 Universe 类型，但被检查的 term 不是 Universe 类型
3. **作用**：判断一个类型的 Universe 层级是否可以提升到另一个 Universe 层级
4. **重要性**：是双轴系统类型检查的核心机制，确保类型提升规则的正确应用

这个函数虽然只在一个地方被调用，但它是类型检查系统中处理 Universe 层级关系的**关键函数**，确保了双轴世界的类型系统能够正确工作。

---

<a name="english"></a>
## English

# `kos_universe_leq()` Function Usage Analysis

## Function Definition

**Location**: `src/core/universe.c:62`

**Function Signature**:
```c
bool kos_universe_leq(universe_info u1, universe_info u2);
```

**Functionality**:
- Checks Universe level relationship: `u1 ≤ u2`
- Supports dual-axis system (computational axis U_i and logical axis Type_i)
- Implements Universe promotion rules

## Call Location

### Single Call Site: `kos_check()` Function

**Location**: `src/core/type_checker.c:90`

**Call Scenario**: During type checking, when checking Universe types (KOS_U or KOS_TYPE)

**Code Context**:
```c
bool kos_check(kos_term* ctx, kos_term* term, kos_term* type) {
    // ... Get Universe information
    universe_info term_info = kos_get_universe_info(term);
    universe_info type_info = kos_get_universe_info(type);
    
    // ... Case-by-case checking based on type
    switch (type->kind) {
        // ... Other type checks ...
        
        case KOS_U:
        case KOS_TYPE:
            // Universe type checking: compare Universe levels
            if (term->kind == type->kind) {
                // If both are Universe types, directly compare axis and level
                return (term->data.universe.axis == type->data.universe.axis &&
                        term->data.universe.level == type->data.universe.level);
            }
            // If term is not a Universe type, check if it can be promoted to that Universe level
            return kos_universe_leq(term_info, type_info);  // ← Single call site
            
        // ... Other type checks ...
    }
}
```

## Usage Timing

### Trigger Conditions

`kos_universe_leq()` is called in the following situations:

1. **Target type of type checking is a Universe type** (`type->kind == KOS_U` or `KOS_TYPE`)
2. **Term being checked is not a Universe type** (`term->kind != KOS_U && term->kind != KOS_TYPE`)

### Specific Scenarios

#### Scenario 1: Check if non-Universe type can be promoted to Universe type

**Example**:
```c
// Assume a type definition: Type_1 (logical axis, level 1)
kos_term* type = kos_mk_universe_logical(1);  // Type_1

// Have a value type: U_0 (computational axis, level 0)
kos_term* term = kos_mk_val("some_value");  // U_0

// Type checking: term : type?
bool result = kos_check(NULL, term, type);
// Internally calls: kos_universe_leq(term_info, type_info)
// term_info = {UNIVERSE_COMPUTATIONAL, 0}
// type_info = {UNIVERSE_LOGICAL, 1}
// Check: Can U_0 be promoted to Type_1?
// Rule: U_i : Type_{i+1}, so U_0 : Type_1 ✓
```

#### Scenario 2: Check if basic type can be promoted to Universe type

**Example**:
```c
// Type: Type_2 (logical axis, level 2)
kos_term* type = kos_mk_universe_logical(2);  // Type_2

// Value: BatchID (basic type, inferred as U_0)
kos_term* term = kos_mk_id("BATCH001");  // U_0

// Type checking
bool result = kos_check(NULL, term, type);
// Internally calls: kos_universe_leq({UNIVERSE_COMPUTATIONAL, 0}, {UNIVERSE_LOGICAL, 2})
// Check: Can U_0 be promoted to Type_2?
// Rule: U_0 : Type_1, but Type_1 < Type_2, so U_0 : Type_2 ✓
```

## Implementation Logic

`kos_universe_leq()` implements the following rules:

### 1. Same Axis: Direct Level Comparison
```c
if (u1.axis == u2.axis) {
    return u1.level <= u2.level;
}
```
- `U_0 ≤ U_1` ✓
- `Type_1 ≤ Type_2` ✓
- `U_2 ≤ U_1` ✗

### 2. Cross-Axis Promotion: Computational Axis Promoted to Logical Axis
```c
if (u1.axis == UNIVERSE_COMPUTATIONAL && u2.axis == UNIVERSE_LOGICAL) {
    return u1.level + 1 <= u2.level;
}
```
- `U_0 : Type_1` ✓ (0 + 1 = 1 ≤ 1)
- `U_0 : Type_2` ✓ (0 + 1 = 1 ≤ 2)
- `U_1 : Type_1` ✓ (1 + 1 = 2 ≤ 1) ✗ Actually should be U_1 : Type_2
- `U_1 : Type_2` ✓ (1 + 1 = 2 ≤ 2)

### 3. Prop Embedding: Proposition Embedded into Data Axis
```c
if (u1.axis == UNIVERSE_LOGICAL && u1.level == 1 && 
    u2.axis == UNIVERSE_COMPUTATIONAL && u2.level == 1) {
    return true;
}
```
- `Prop : U_1` ✓

## Call Chain

```
Runtime event elaboration (runtime_elab.c)
    ↓
kos_ontology_mk_type_instance()
    ↓
kos_check()  ← Core type checking
    ↓
case KOS_U / KOS_TYPE:
    ↓
kos_universe_leq()  ← Single call site
```

## Practical Applications

### Application in Type Checking

When the system performs type checking, if it encounters the following situations:

1. **Check if a basic type value conforms to a Universe type**
   - Example: Check if `BatchID` value conforms to `Type_1` type
   - System will call `kos_universe_leq()` to determine if promotion is possible

2. **Check if a composite type conforms to a Universe type**
   - Example: Check if `Σ(x:A).B` type conforms to `Type_2` type
   - System will get Universe information of composite type, then call `kos_universe_leq()` to determine

### Role in Dual-Axis System

`kos_universe_leq()` is the core mechanism of the Dual-Axis Universe System:

- **Computational Axis (U_i)**: Used for computational data
- **Logical Axis (Type_i)**: Used for logical types
- **Promotion Rule**: `U_i : Type_{i+1}` (computational axis can be promoted to logical axis)

This function ensures that cross-axis Universe level relationships can be correctly handled during type checking.

## Summary

The `kos_universe_leq()` function:

1. **Single Call Site**: In `kos_check()` function, when checking Universe types
2. **Trigger Condition**: Target type is a Universe type, but the term being checked is not a Universe type
3. **Purpose**: Determines if a type's Universe level can be promoted to another Universe level
4. **Importance**: Core mechanism of dual-axis system type checking, ensuring correct application of type promotion rules

Although this function is only called in one place, it is a **key function** in the type checking system for handling Universe level relationships, ensuring the dual-axis world's type system can work correctly.











