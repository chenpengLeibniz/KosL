# 类型系统重构计划

## 问题分析

从类型论的角度，当前系统存在以下根本性问题：

1. **问题1**: `kos_ontology.h` 中定义了 `AtomicTypeDef`, `PredicateTypeDef`, `EventTypeDef` 等 C 语言结构体，但这些并不是通过类型构造（type construction）的方式定义的。在直觉类型论（ITT）中，所有类型都应该通过类型构造器（Π、Σ、Sum等）来构造。

2. **问题2**: 类型实例化时，应该通过类型检查来验证是否符合类型构造的条件，而不是直接通过C语言变量定义。

## 重构方案

### 核心原则

1. **所有类型定义都应该是 `kos_term*` 类型**，通过类型构造器（Π、Σ、Sum等）构造
2. **类型本体存储类型定义的集合**（`kos_term*`），而不是 C 语言的 struct 数组
3. **类型实例化通过类型检查验证**，使用 `kos_check` 或 `kos_type_check` 函数

### 重构步骤

#### 第一步：重构 `kos_ontology.h`

✅ **已完成**：创建了新的头文件，移除了 C 语言 struct 定义

- 移除了 `AtomicTypeDef`, `PredicateTypeDef`, `EventTypeDef`
- 新定义了 `TypeDefinition` 结构，包含：
  - `char* name`: 类型名称
  - `kos_term* type_def`: 类型定义（通过类型构造器构造）
  - `kos_term* ctx`: 类型定义的上下文
- 重构了 `TypeOntology` 结构，只存储 `TypeDefinition` 数组

#### 第二步：重构 `ontology_manager.c`

需要完全重写实现文件：

- 实现基于 `TypeDefinition` 的 CRUD 操作
- 所有类型定义都通过类型构造器（`kos_mk_pi`, `kos_mk_sigma`, `kos_mk_sum` 等）构造
- 类型实例化通过 `kos_check` 或 `kos_type_check` 验证

#### 第三步：更新领域代码

需要更新所有使用旧类型定义的文件：

- `src/domain/manufacturing/ontology_setup.c`
- `src/domain/manufacturing/ontology_crud.c`
- `src/domain/manufacturing/types.c`
- `include/kos_manufacturing.h`

#### 第四步：实现类型构造示例

需要提供类型构造的示例：

- 如何用 Σ 类型构造事件类型（如 FailEvt）
- 如何用 Π 类型构造谓词类型（如 InRoute）
- 如何用 Sum 类型构造联合类型

## 类型构造示例

### 事件类型构造（使用 Σ 类型）

在类型论中，事件类型应该通过 Σ 类型构造：

```
FailEvt ≡ Σ(b: BatchID). Σ(err: ErrorCode). Σ(t: Time). Prop
```

这应该通过嵌套的 `kos_mk_sigma` 调用来构造。

### 谓词类型构造（使用 Π 类型）

谓词类型应该通过 Π 类型构造：

```
InRoute ≡ Π(b: BatchID). Π(m: Machine). Prop
```

这应该通过 `kos_mk_pi` 调用来构造。

### 类型实例化验证

类型实例化时，必须通过类型检查验证：

```c
// 1. 查找类型定义
kos_term* type_def = kos_ontology_find_type_definition(ontology, "FailEvt");

// 2. 构造数据项
kos_term* data_term = ...;  // 构造事件数据

// 3. 通过类型检查验证
bool valid = kos_check(ctx, data_term, type_def);

// 4. 如果验证通过，构造实例
if (valid) {
    kos_term* instance = kos_mk_pair(data_term, proof);
    return instance;
}
```

## 当前状态

- ✅ 新头文件已创建（`kos_ontology.h`）
- ⏳ 实现文件需要重构（`ontology_manager.c`）
- ⏳ 领域代码需要更新
- ⏳ 需要添加类型构造的辅助函数

## 下一步行动

1. 重构 `ontology_manager.c` 实现文件
2. 提供类型构造的辅助函数
3. 更新领域代码使用新的类型系统
4. 添加类型构造的示例和文档































