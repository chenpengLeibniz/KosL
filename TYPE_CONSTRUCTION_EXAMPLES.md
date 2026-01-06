# 类型构造示例和文档

## 概述

本文档说明如何基于直觉类型论（Intuitionistic Type Theory, ITT）的原则，使用类型构造器（Π类型、Σ类型、Sum类型等）来构造所有类型定义。所有类型都是通过类型构造器构造的 `kos_term*`，而不是C语言结构体。

## 核心原则

1. **所有类型都是类型构造的产物**：不存在"原子类型"的预定义结构体，所有类型都通过类型构造器（Π、Σ、Sum等）构造
2. **类型定义是 `kos_term*`**：类型定义本身是类型系统中的项（term）
3. **类型实例化需要类型检查**：实例化时必须通过 `kos_check` 或 `kos_type_check` 验证是否符合类型构造条件

## 类型构造器回顾

### 1. 依赖积类型（Π类型）

**语法**：`Π(x:A).B`

**含义**：对于类型 A 中的每个 x，都有类型 B（B 可能依赖于 x）

**构造规则**：
- 引入（构造）：`λx:A.t : Π(x:A).B` （当 `Γ, x:A ⊢ t : B`）
- 消除（应用）：`f a : B[a/x]` （当 `f : Π(x:A).B` 且 `a : A`）

**用途**：函数类型、谓词类型

### 2. 依赖和类型（Σ类型）

**语法**：`Σ(x:A).B`

**含义**：类型 A 和类型 B 的依赖对，其中 B 依赖于 A 中的值

**构造规则**：
- 引入（配对）：`<d, p> : Σ(x:A).B` （当 `d : A` 且 `p : B[d/x]`）
- 消除（拆分）：`split(p, x.y.u)` 

**用途**：带证明的数据对、事件类型

### 3. 和类型（Sum类型）

**语法**：`A + B`

**含义**：类型 A 或类型 B 的联合

**构造规则**：
- 引入（左注入）：`inl a : A + B` （当 `a : A`）
- 引入（右注入）：`inr b : A + B` （当 `b : B`）
- 消除（案例分析）：`case s of inl x => t1 | inr y => t2`

**用途**：联合类型、可选类型

## 类型构造示例

### 示例1：基础类型（使用基础Sort）

在类型论中，基础类型如 `BatchID`、`Time` 等应该通过基础Sort（如 `Val`、`ID`、`Time`）构造，而不是C语言结构体。

#### 旧方式（错误）：
```c
typedef struct {
    char batch_id[64];
} BatchID;  // ❌ 直接用C语言结构体定义
```

#### 新方式（正确）：
```c
// BatchID 类型定义：基于基础Sort ID
kos_term* batch_id_type = kos_mk_id("BatchID");

// BatchID 实例：必须是 kos_term* 类型
kos_term* batch_id_instance = kos_mk_id("Batch_202310-01");

// 类型检查验证
bool valid = kos_check(ctx, batch_id_instance, batch_id_type);
```

### 示例2：事件类型（使用Σ类型）

事件类型应该通过嵌套的Σ类型构造，表示"数据 + 证明"的结构。

#### FailEvt 事件类型构造

**类型论定义**：
```
FailEvt ≡ Σ(b: BatchID). Σ(err: ErrorCode). Σ(t: Time). Prop
```

**含义**：失败事件是一个带证明的数据对，包含批次ID、错误代码、时间，以及证明（例如：证明时间在QA班次内）

**代码构造**：
```c
// 步骤1：定义基础类型
kos_term* batch_id_type = kos_mk_id("BatchID");
kos_term* error_code_type = kos_mk_prop("ErrorCode");
kos_term* time_type = kos_mk_time("Time");
kos_term* prop_type = kos_mk_prop("Prop");

// 步骤2：构造最内层 Σ 类型：Σ(t: Time). Prop
kos_term* time_prop_sigma = kos_mk_sigma(time_type, prop_type);

// 步骤3：构造中间层 Σ 类型：Σ(err: ErrorCode). (Σ(t: Time). Prop)
kos_term* error_time_prop_sigma = kos_mk_sigma(error_code_type, time_prop_sigma);

// 步骤4：构造最外层 Σ 类型：Σ(b: BatchID). (Σ(err: ErrorCode). (Σ(t: Time). Prop))
kos_term* fail_evt_type = kos_mk_sigma(batch_id_type, error_time_prop_sigma);

// 步骤5：将类型定义添加到本体
kos_ontology_add_type_definition(ontology, "FailEvt", fail_evt_type, NULL);
```

**类型实例化**：
```c
// 步骤1：查找类型定义
kos_term* fail_evt_type_def = kos_ontology_find_type_definition(ontology, "FailEvt");

// 步骤2：构造数据项（必须是 kos_term* 类型）
kos_term* batch_id = kos_mk_id("Batch_202310-01");
kos_term* error_code = kos_mk_prop("HARD_ERR");
kos_term* time_val = kos_mk_time("1696233600");
kos_term* proof = kos_mk_prop("Proof(QAShift)");  // QA班次证明

// 步骤3：构造嵌套的对（从内到外）
kos_term* time_proof_pair = kos_mk_pair(time_val, proof);
kos_term* error_time_proof_pair = kos_mk_pair(error_code, time_proof_pair);
kos_term* fail_evt_data = kos_mk_pair(batch_id, error_time_proof_pair);

// 步骤4：类型检查验证
bool valid = kos_check(ctx, fail_evt_data, fail_evt_type_def);

// 步骤5：如果验证通过，返回实例
if (valid) {
    return fail_evt_data;  // 类型检查通过，实例有效
} else {
    return NULL;  // 类型检查失败，实例无效
}
```

### 示例3：谓词类型（使用Π类型）

谓词类型应该通过Π类型构造，表示"对于所有输入，都有命题类型"。

#### InRoute 谓词类型构造

**类型论定义**：
```
InRoute ≡ Π(b: BatchID). Π(m: Machine). Prop
```

**含义**：InRoute 是一个谓词，对于每个批次ID b 和机器 m，返回一个命题（真或假）

**代码构造**：
```c
// 步骤1：定义基础类型
kos_term* batch_id_type = kos_mk_id("BatchID");
kos_term* machine_type = kos_mk_id("Machine");
kos_term* prop_type = kos_mk_prop("Prop");

// 步骤2：构造内层 Π 类型：Π(m: Machine). Prop
kos_term* machine_prop_pi = kos_mk_pi(machine_type, prop_type);

// 步骤3：构造外层 Π 类型：Π(b: BatchID). (Π(m: Machine). Prop)
kos_term* in_route_type = kos_mk_pi(batch_id_type, machine_prop_pi);

// 步骤4：将类型定义添加到本体
kos_ontology_add_type_definition(ontology, "InRoute", in_route_type, NULL);
```

**类型实例化（应用）**：
```c
// 步骤1：查找类型定义
kos_term* in_route_type_def = kos_ontology_find_type_definition(ontology, "InRoute");

// 步骤2：构造λ抽象（谓词的具体实现）
// 这里需要构造一个函数，对于给定的批次和机器，返回命题
kos_term* batch_param = kos_mk_id("b");
kos_term* machine_param = kos_mk_id("m");
kos_term* body_term = kos_mk_prop("InRoute(b, m)");  // 谓词体

kos_term* lambda_m = kos_mk_lambda(machine_param, body_term);
kos_term* lambda_bm = kos_mk_lambda(batch_param, lambda_m);

// 步骤3：类型检查验证
bool valid = kos_check(ctx, lambda_bm, in_route_type_def);

// 步骤4：应用谓词（类型消除）
kos_term* batch = kos_mk_id("Batch_202310-01");
kos_term* machine = kos_mk_id("Machine_03");

kos_term* applied1 = kos_mk_app(lambda_bm, batch);      // 应用批次参数
kos_term* applied2 = kos_mk_app(applied1, machine);     // 应用机器参数
// applied2 : Prop，表示 "InRoute(Batch_202310-01, Machine_03)"
```

### 示例4：联合类型（使用Sum类型）

联合类型应该通过Sum类型构造，表示"类型A或类型B"。

#### Result 类型构造

**类型论定义**：
```
Result ≡ Success + Failure
```

**含义**：结果类型要么是成功，要么是失败

**代码构造**：
```c
// 步骤1：定义基础类型
kos_term* success_type = kos_mk_prop("Success");
kos_term* failure_type = kos_mk_prop("Failure");

// 步骤2：构造Sum类型：Success + Failure
kos_term* result_type = kos_mk_sum(success_type, failure_type);

// 步骤3：将类型定义添加到本体
kos_ontology_add_type_definition(ontology, "Result", result_type, NULL);
```

**类型实例化**：
```c
// 步骤1：查找类型定义
kos_term* result_type_def = kos_ontology_find_type_definition(ontology, "Result");

// 步骤2：构造成功实例（左注入）
kos_term* success_value = kos_mk_prop("SuccessData");
kos_term* success_instance = kos_mk_inl(success_type, failure_type, success_value);

// 步骤3：构造失败实例（右注入）
kos_term* failure_value = kos_mk_prop("FailureData");
kos_term* failure_instance = kos_mk_inr(success_type, failure_type, failure_value);

// 步骤4：类型检查验证
bool valid_success = kos_check(ctx, success_instance, result_type_def);
bool valid_failure = kos_check(ctx, failure_instance, result_type_def);

// 步骤5：案例分析（类型消除）
kos_term* success_branch = kos_mk_prop("handle_success");
kos_term* failure_branch = kos_mk_prop("handle_failure");
kos_term* case_result = kos_mk_case(success_instance, success_branch, failure_branch);
```

### 示例5：复杂事件类型（ProcStep）

**类型论定义**：
```
ProcStep ≡ Σ(b: BatchID). Σ(m: Machine). Σ(dur: TimeRange). Proof(InRoute(b, m))
```

**含义**：工艺步骤是一个带证明的数据对，包含批次、机器、时间区间，以及证明（批次在该机器上的工艺路线证明）

**代码构造**：
```c
// 步骤1：定义基础类型
kos_term* batch_id_type = kos_mk_id("BatchID");
kos_term* machine_type = kos_mk_id("Machine");
kos_term* time_range_type = kos_mk_prop("TimeRange");  // 时间区间类型
kos_term* in_route_type = kos_ontology_find_type_definition(ontology, "InRoute");

// 步骤2：构造证明类型（应用谓词类型）
kos_term* batch_val = kos_mk_id("b");
kos_term* machine_val = kos_mk_id("m");
kos_term* in_route_proof_type = kos_mk_app(kos_mk_app(in_route_type, batch_val), machine_val);
// in_route_proof_type : Prop，表示 "InRoute(b, m)"

// 步骤3：构造最内层 Σ 类型：Σ(dur: TimeRange). Proof(InRoute(b, m))
kos_term* time_range_proof_sigma = kos_mk_sigma(time_range_type, in_route_proof_type);

// 步骤4：构造中间层 Σ 类型：Σ(m: Machine). (Σ(dur: TimeRange). Proof(...))
kos_term* machine_time_range_proof_sigma = kos_mk_sigma(machine_type, time_range_proof_sigma);

// 步骤5：构造最外层 Σ 类型：Σ(b: BatchID). (Σ(m: Machine). (Σ(dur: TimeRange). Proof(...)))
kos_term* proc_step_type = kos_mk_sigma(batch_id_type, machine_time_range_proof_sigma);

// 步骤6：将类型定义添加到本体
kos_ontology_add_type_definition(ontology, "ProcStep", proc_step_type, NULL);
```

## 类型实例化的完整流程

### 标准流程

1. **查找类型定义**：从本体中查找类型定义的 `kos_term*`
2. **构造数据项**：构造要实例化的数据项（必须是 `kos_term*` 类型）
3. **类型检查验证**：使用 `kos_check` 或 `kos_type_check` 验证数据项是否符合类型构造条件
4. **构造实例**：如果验证通过，构造类型实例（可能需要添加证明）
5. **返回结果**：返回验证通过的实例，或返回 NULL（如果验证失败）

### 代码模板

```c
kos_term* kos_ontology_mk_type_instance(TypeOntology* ontology,
                                        const char* type_name,
                                        kos_term* data_term,
                                        kos_term* ctx) {
    // 步骤1：查找类型定义
    kos_term* type_def = kos_ontology_find_type_definition(ontology, type_name);
    if (!type_def) {
        return NULL;  // 类型定义不存在
    }
    
    // 步骤2：类型检查验证
    bool valid = kos_check(ctx, data_term, type_def);
    if (!valid) {
        return NULL;  // 类型检查失败
    }
    
    // 步骤3：构造实例（如果需要，添加证明）
    // 对于Σ类型，可能需要构造 <data, proof> 对
    // 对于Π类型，可能需要构造λ抽象
    // 对于Sum类型，可能需要使用 inl 或 inr
    
    return instance;  // 返回验证通过的实例
}
```

## 类型检查的重要性

### 为什么需要类型检查？

1. **确保类型安全**：只有符合类型构造条件的项才能被接受
2. **保证逻辑正确性**：类型检查确保数据项的结构和约束满足类型定义的要求
3. **遵循类型论原则**：类型论的核心是"类型即规范"，类型检查是验证规范是否满足的唯一方式

### 类型检查的时机

- **类型定义时**：验证类型定义本身是否合法（类型构造是否正确）
- **类型实例化时**：验证数据项是否符合类型构造条件（必须）
- **类型应用时**：验证函数应用、案例分析等是否符合类型规则

## 总结

1. **所有类型都是类型构造的产物**：通过Π类型、Σ类型、Sum类型等构造
2. **类型定义是 `kos_term*`**：类型定义本身是类型系统中的项
3. **类型实例化需要类型检查**：必须通过 `kos_check` 验证
4. **不存在C语言结构体定义的类型**：所有类型都通过类型构造器构造

这种设计确保了类型系统完全符合直觉类型论的原则，所有类型都是通过类型构造器构造的，类型实例化必须通过类型检查验证。
















