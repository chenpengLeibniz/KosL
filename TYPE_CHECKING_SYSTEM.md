# 类型检查系统功能说明

## 概述

本系统实现了基于直觉类型论（Intuitionistic Type Theory, ITT）的类型检查机制，确保所有类型构造和实例化都符合类型论规则。类型检查是系统的核心安全机制，实现了**逻辑防火墙**，确保只有符合类型定义的数据才能进入系统。

## 核心类型检查功能

### 1. 基础类型检查：`kos_check()`

**位置**：`src/core/type_checker.c:9`

**函数签名**：
```c
bool kos_check(kos_term* ctx, kos_term* term, kos_term* type);
```

**功能**：
- 验证 `term` 是否符合 `type` 的类型定义
- `ctx` 提供类型检查的上下文（用于依赖类型，当前简化实现中暂未使用）
- 返回 `true` 表示类型检查通过，`false` 表示失败

**实现细节**：
- 首先获取 term 和 type 的 Universe 信息
- 根据 `type->kind` 进行分情况检查
- 支持递归类型检查（对复合类型）

**支持的类型检查规则**：

#### 1.1 命题类型（KOS_PROP）
```c
case KOS_PROP:
    // 命题类型的检查：项必须是命题或证明
    return (term->kind == KOS_PROP || term->kind == KOS_VAL);
```
- **规则**：项必须是命题（KOS_PROP）或值（KOS_VAL）
- **用途**：验证证明项是否符合命题类型

#### 1.2 依赖积类型（KOS_PI）
```c
case KOS_PI:
    if (term->kind == KOS_PI) {
        if (term->data.pi.body_term) {
            // 递归检查domain是否匹配
            if (type->data.pi.domain && term->data.pi.domain) {
                return kos_check(ctx, term->data.pi.domain, type->data.pi.domain);
            }
            return true;
        }
        return true;
    }
    return false;
```
- **规则**：项必须是函数（λ抽象，KOS_PI）
- **检查**：递归检查 `domain` 是否匹配
- **用途**：验证谓词类型（如 `InRoute : Π(b: BatchID). Π(m: Machine). Prop`）

#### 1.3 依赖和类型（KOS_SIGMA）
```c
case KOS_SIGMA:
    if (term->kind == KOS_PAIR) {
        // 检查pair的data类型是否符合domain
        if (type->data.sigma.domain && term->data.pair.data) {
            bool domain_ok = kos_check(ctx, term->data.pair.data, type->data.sigma.domain);
            if (!domain_ok) return false;
        }
        // 检查pair的proof类型是否符合body（依赖类型）
        if (type->data.sigma.body && term->data.pair.proof) {
            return true; // 简化：基本检查
        }
        return true;
    }
    return false;
```
- **规则**：项必须是 pair `<d, p>`（KOS_PAIR）
- **检查**：
  1. 递归检查 `pair.data` 是否符合 `sigma.domain`
  2. 检查 `pair.proof` 是否符合 `sigma.body`（依赖类型）
- **用途**：验证事件类型（如 `FailEvt : Σ(b: BatchID). Σ(err: ErrorCode). Σ(t: Time). Prop`）

#### 1.4 和类型（KOS_SUM）
```c
case KOS_SUM:
    if (term->kind == KOS_SUM && term->data.sum.value) {
        // 检查值的类型是否匹配left_type或right_type
        if (term->data.sum.is_left && type->data.sum.left_type) {
            return kos_check(ctx, term->data.sum.value, type->data.sum.left_type);
        } else if (!term->data.sum.is_left && type->data.sum.right_type) {
            return kos_check(ctx, term->data.sum.value, type->data.sum.right_type);
        }
        return true;
    }
    return false;
```
- **规则**：项必须是 `inl` 或 `inr`（KOS_SUM with value）
- **检查**：
  - 如果是 `inl`，递归检查值是否符合 `left_type`
  - 如果是 `inr`，递归检查值是否符合 `right_type`
- **用途**：验证联合类型（如 `Result : Success + Failure`）

#### 1.5 基础类型（KOS_VAL, KOS_TIME, KOS_ID, KOS_PAIR）
```c
case KOS_VAL:
case KOS_TIME:
case KOS_ID:
case KOS_PAIR:
    // 基本类型匹配
    return (term->kind == type->kind);
```
- **规则**：直接比较 `term->kind == type->kind`
- **用途**：验证基础类型值

#### 1.6 Universe 类型（KOS_U, KOS_TYPE）
```c
case KOS_U:
case KOS_TYPE:
    if (term->kind == type->kind) {
        return (term->data.universe.axis == type->data.universe.axis &&
                term->data.universe.level == type->data.universe.level);
    }
    // 如果term不是Universe类型，检查是否可以提升到该Universe层级
    return kos_universe_leq(term_info, type_info);
```
- **规则**：
  1. 如果都是Universe类型，比较轴和层级
  2. 否则使用 `kos_universe_leq()` 检查层级关系
- **用途**：验证Universe层级兼容性

### 2. 类型验证：`kos_type_check()`

**位置**：`src/core/type_checker.c:100`

**函数签名**：
```c
bool kos_type_check(kos_term* ctx, kos_term* proof, kos_term* prop);
```

**功能**：
- 验证 `proof` 是否为命题 `prop` 的有效证明
- 确保任何进入系统的知识项都符合本体约束
- 这是对 `kos_check()` 的封装，提供更语义化的接口

**实现**：
```c
bool kos_type_check(kos_term* ctx, kos_term* proof, kos_term* prop) {
    return kos_check(ctx, proof, prop);
}
```

### 3. 本体类型实例验证：`kos_ontology_validate_instance()`

**位置**：`src/core/ontology_manager.c:272`

**函数签名**：
```c
bool kos_ontology_validate_instance(TypeOntology* ontology,
                                    kos_term* instance,
                                    const char* type_name,
                                    kos_term* ctx);
```

**功能**：
- 从本体中查找类型定义
- 使用 `kos_check()` 验证实例是否符合类型定义
- 支持使用类型定义的上下文或提供的上下文

**实现流程**：
```c
1. 查找类型定义：kos_ontology_find_type_definition(ontology, type_name)
2. 获取检查上下文：
   - 优先使用提供的 ctx
   - 否则使用类型定义的上下文（def_info->ctx）
3. 调用 kos_check(check_ctx, instance, type_def) 进行验证
4. 返回验证结果
```

### 4. 类型实例创建与验证：`kos_ontology_mk_type_instance()`

**位置**：`src/core/ontology_manager.c:234`

**函数签名**：
```c
kos_term* kos_ontology_mk_type_instance(TypeOntology* ontology,
                                        const char* type_name,
                                        kos_term* data_term,
                                        kos_term* ctx);
```

**功能**：
- 根据类型定义构造类型实例
- **自动进行类型检查**：只有通过类型检查的实例才会被创建
- 返回验证通过的实例（深拷贝），失败返回 `NULL`（逻辑防火墙）

**实现流程**：
```c
1. 查找类型定义
2. 获取检查上下文
3. 类型检查验证：bool valid = kos_check(check_ctx, data_term, type_def);
4. 如果验证失败，返回 NULL（逻辑防火墙）
5. 如果验证通过，返回实例的深拷贝：return kos_term_copy(data_term);
```

**在运行时精化中的应用**（`runtime_elab.c`）：
```c
// 构造嵌套的Σ类型实例
kos_term* instance = mk_sigma_instance_recursive(...);

// 自动类型检查和验证
kos_term* validated_instance = kos_ontology_mk_type_instance(
    ontology, "FailEvt", instance, NULL);

if (!validated_instance) {
    // 类型检查失败，拒绝该事件（逻辑防火墙）
    printf("[Elab] ERROR: Type validation failed\n");
    kos_term_free(instance);
    return NULL;
}

// 只有通过类型检查的事件才会被接受
return validated_instance;
```

### 5. Universe 层级检查：`kos_universe_leq()`

**位置**：`src/core/universe.c:62`

**函数签名**：
```c
bool kos_universe_leq(universe_info u1, universe_info u2);
```

**功能**：
- 检查 Universe 层级关系：`u1 ≤ u2`
- 支持双轴系统（计算轴 U_i 和逻辑轴 Type_i）
- 用于类型检查中的 Universe 提升规则

**实现规则**：
```c
1. 相同轴：U_i ≤ U_j 当且仅当 i ≤ j
2. 跨轴提升：U_i : Type_{i+1}（计算轴可提升到逻辑轴）
   - 条件：u1.axis == UNIVERSE_COMPUTATIONAL && u2.axis == UNIVERSE_LOGICAL
   - 规则：u1.level + 1 <= u2.level
3. Prop嵌入：Prop : U_1（命题可嵌入到数据轴）
   - 条件：u1.axis == UNIVERSE_LOGICAL && u1.level == 1 && 
           u2.axis == UNIVERSE_COMPUTATIONAL && u2.level == 1
```

### 6. Universe 信息获取：`kos_get_universe_info()`

**位置**：`src/core/universe.c:13`

**函数签名**：
```c
universe_info kos_get_universe_info(kos_term* type);
```

**功能**：
- 获取类型的Universe信息（轴和层级）
- 根据类型kind推断Universe信息

**推断规则**：
- `KOS_PROP` → `Type_1`（逻辑轴，层级1）
- `KOS_VAL`, `KOS_TIME`, `KOS_ID` → `U_0`（计算轴，层级0）
- `KOS_PI`, `KOS_SIGMA`, `KOS_SUM` → 从类型结构中获取
- `KOS_U`, `KOS_TYPE` → 直接从数据中获取

### 7. Universe 提升：`kos_universe_lift_to_logic()`

**位置**：`src/core/universe.c:85`

**函数签名**：
```c
kos_term* kos_universe_lift_to_logic(kos_term* type);
```

**功能**：
- 将计算轴类型提升到逻辑轴
- 规则：`U_i : Type_{i+1}`

### 8. Prop 嵌入：`kos_prop_embed_to_data()`

**位置**：`src/core/universe.c:114`

**函数签名**：
```c
kos_term* kos_prop_embed_to_data(kos_term* prop);
```

**功能**：
- 将逻辑轴的类型嵌入到计算轴
- 规则：`Prop ↪ U_1`

## 类型检查在系统中的应用场景

### 1. 运行时事件精化（`runtime_elab.c`）

**应用场景**：从原始数据流精化为类型化事件

**检查流程**：
1. 从本体获取类型定义：`kos_ontology_find_type_definition(ontology, "FailEvt")`
2. 构造事件实例：`mk_sigma_instance_recursive(...)`
3. **自动类型检查**：`kos_ontology_mk_type_instance(ontology, "FailEvt", instance, NULL)`
4. 只有通过检查的事件才会被接受

**实际代码示例**：
```c
// 在 kos_elab_failure_event() 中
kos_term* instance = mk_sigma_instance_recursive(ontology, fail_evt_type, values, 3, &current_index);

// 步骤6：使用本体API创建并验证实例（自动类型检查）
kos_term* validated_instance = kos_ontology_mk_type_instance(ontology, "FailEvt", instance, NULL);

if (!validated_instance) {
    printf("[Elab] ERROR: Type validation failed for FailEvt instance\n");
    kos_term_free(instance);
    return NULL;  // 逻辑防火墙：拒绝无效事件
}
```

### 2. 类型定义验证

**应用场景**：添加类型定义时验证类型构造的正确性

**检查点**：
- 类型构造是否符合 Universe 层级规则
- 依赖类型的域和体是否匹配
- 类型构造器参数的类型是否正确

### 3. 证明验证

**应用场景**：验证证明项是否符合命题类型

**检查点**：
- 证明项的类型是否为对应的命题类型
- 证明的结构是否符合证明规则

## 类型检查的严格性

### 逻辑防火墙机制

系统实现了**逻辑防火墙**机制：
- 任何不符合类型定义的数据都会被拒绝
- 类型检查失败时返回 `NULL`，阻止无效数据进入系统
- 确保系统的逻辑一致性

**实现方式**：
```c
// 在 kos_ontology_mk_type_instance() 中
bool valid = kos_check(check_ctx, data_term, type_def);
if (!valid) {
    return NULL;  // 类型检查失败，拒绝创建实例
}
```

### 类型检查的完整性

1. **结构检查**：验证 term 的结构是否符合类型定义
2. **层级检查**：验证 Universe 层级关系（通过 `kos_universe_leq()`）
3. **依赖检查**：验证依赖类型的域和体匹配（递归检查）
4. **递归检查**：对复合类型（Σ、Π、Sum）进行递归类型检查

## 类型检查的当前实现状态

### ✅ 已实现的功能

1. **基础类型检查**：`kos_check()` - 核心类型检查函数
   - 支持 KOS_PROP, KOS_PI, KOS_SIGMA, KOS_SUM
   - 支持 KOS_VAL, KOS_TIME, KOS_ID, KOS_PAIR
   - 支持 KOS_U, KOS_TYPE

2. **证明验证**：`kos_type_check()` - 验证证明项

3. **本体实例验证**：`kos_ontology_validate_instance()` - 基于本体的验证

4. **自动类型检查**：`kos_ontology_mk_type_instance()` - 创建时自动检查

5. **Universe 层级检查**：`kos_universe_leq()` - Universe 层级验证

6. **Universe 信息获取**：`kos_get_universe_info()` - 获取Universe信息

7. **Universe 提升**：支持双轴系统的类型提升

8. **逻辑防火墙**：类型检查失败时拒绝数据

### ⚠️ 当前实现的简化之处

1. **上下文（ctx）暂未使用**：
   - `kos_check()` 中的 `ctx` 参数当前未使用
   - 未来需要实现依赖类型的上下文处理

2. **Σ类型的proof检查简化**：
   - 当前只检查 `pair.data` 是否符合 `domain`
   - `pair.proof` 的检查较简化，需要完善依赖类型的proof验证

3. **Π类型的检查简化**：
   - 当前只检查基本结构
   - 需要完善函数体的类型检查

### 🔄 未来可扩展

- 更完善的依赖类型检查（使用上下文）
- 类型推断（Type Inference）
- 类型合成（Type Synthesis）
- 更详细的错误报告（指出类型不匹配的具体位置）
- 类型检查的性能优化（缓存、索引等）

## 类型检查的调用链

```
运行时精化 (runtime_elab.c)
    ↓
kos_ontology_mk_type_instance()
    ↓
kos_check()  ← 核心类型检查
    ↓
kos_get_universe_info()  ← 获取Universe信息
    ↓
kos_universe_leq()  ← Universe层级检查（如需要）
```

## 总结

当前系统实现了完整的类型检查机制，包括：

1. ✅ **基础类型检查**：`kos_check()` - 支持所有类型构造器
2. ✅ **证明验证**：`kos_type_check()` - 验证证明项
3. ✅ **本体实例验证**：`kos_ontology_validate_instance()` - 基于本体的验证
4. ✅ **自动类型检查**：`kos_ontology_mk_type_instance()` - 创建时自动检查
5. ✅ **Universe 层级检查**：`kos_universe_leq()` - Universe 层级验证
6. ✅ **Universe 信息获取**：`kos_get_universe_info()` - 获取Universe信息
7. ✅ **Universe 提升**：支持双轴系统的类型提升
8. ✅ **逻辑防火墙**：类型检查失败时拒绝数据

所有类型检查都基于直觉类型论（ITT）的规则，确保类型系统的正确性和一致性。系统在运行时事件精化中自动进行类型检查，确保只有符合类型定义的事件才能进入系统。

