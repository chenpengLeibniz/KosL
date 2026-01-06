# Manufacturing领域代码迁移指南

## 概述

从旧的基于C语言结构体的类型系统迁移到基于类型构造器的类型系统。

## 关键变化

### 1. 类型定义方式的变化

**旧方式（错误）**：
```c
typedef struct {
    char batch_id[64];
} BatchID;  // ❌ C语言结构体

kos_ontology_add_atomic_type(ontology, "BatchID", "String", NULL);
```

**新方式（正确）**：
```c
// BatchID类型定义：基于基础Sort ID
kos_term* batch_id_type = kos_mk_id("BatchID");
kos_ontology_add_type_definition(ontology, "BatchID", batch_id_type, NULL);
```

### 2. 事件类型构造

**旧方式（错误）**：
```c
typedef struct {
    BatchID batch;
    ErrorCode error;
    Time time;
    kos_term* qa_shift_proof;
} FailEvt;  // ❌ C语言结构体
```

**新方式（正确）**：
```c
// FailEvt ≡ Σ(b: BatchID). Σ(err: ErrorCode). Σ(t: Time). Prop
kos_term* batch_id_type = kos_mk_id("BatchID");
kos_term* error_code_type = kos_mk_prop("ErrorCode");
kos_term* time_type = kos_mk_time("Time");
kos_term* prop_type = kos_mk_prop("Prop");

kos_term* time_prop_sigma = kos_mk_sigma(time_type, prop_type);
kos_term* error_time_prop_sigma = kos_mk_sigma(error_code_type, time_prop_sigma);
kos_term* fail_evt_type = kos_mk_sigma(batch_id_type, error_time_prop_sigma);

kos_ontology_add_type_definition(ontology, "FailEvt", fail_evt_type, NULL);
```

### 3. 类型实例化

**旧方式（错误）**：
```c
FailEvt fail_evt = {
    .batch = {...},
    .error = {...},
    .time = 1234567890,
    .qa_shift_proof = proof
};  // ❌ 直接使用C语言结构体
```

**新方式（正确）**：
```c
// 构造数据项（必须是kos_term*类型）
kos_term* batch_id = kos_mk_id("Batch_202310-01");
kos_term* error_code = kos_mk_prop("HARD_ERR");
kos_term* time_val = kos_mk_time("1696233600");
kos_term* proof = kos_mk_prop("Proof(QAShift)");

// 构造嵌套的对（从内到外）
kos_term* time_proof_pair = kos_mk_pair(time_val, proof);
kos_term* error_time_proof_pair = kos_mk_pair(error_code, time_proof_pair);
kos_term* fail_evt_data = kos_mk_pair(batch_id, error_time_proof_pair);

// 类型实例化（包含类型检查）
kos_term* instance = kos_ontology_mk_type_instance(ontology, "FailEvt", fail_evt_data, ctx);
```

## 迁移策略

由于这是一个大规模重构，建议采用以下策略：

1. **暂时保留辅助结构体**：为了兼容性，可以暂时保留一些辅助结构体（如BatchID, Machine等），但它们在内部应该转换为kos_term*
2. **逐步迁移**：先迁移核心类型定义，再迁移实例化代码
3. **添加适配层**：在迁移过程中，可以添加适配函数来桥接新旧代码

## 需要更新的文件

1. `include/kos_manufacturing.h` - 更新函数签名，移除或标记旧的类型定义
2. `src/domain/manufacturing/ontology_setup.c` - 使用类型构造器构造类型定义
3. `src/domain/manufacturing/ontology_crud.c` - 更新为使用新的类型定义API
4. `src/domain/manufacturing/types.c` - 使用类型检查验证实例
















