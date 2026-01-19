# 类型构造API设计文档

## 概述

本文档说明新的类型构造API的设计和使用方法。所有API都基于类型论原则，使用类型构造器（Π、Σ、Sum等）构造类型定义。

## 核心数据结构

### TypeDefinition

```c
typedef struct {
    char* name;              // 类型名称（如 "BatchID", "FailEvt"）
    kos_term* type_def;      // 类型定义（kos_term*），通过类型构造器构造
    kos_term* ctx;           // 类型定义的上下文（可选，用于依赖类型）
} TypeDefinition;
```

### TypeOntology

```c
typedef struct {
    char domain_name[64];              // 领域名称，如 "manufacturing"
    TypeDefinition* type_definitions;  // 类型定义数组
    size_t type_count;                 // 类型定义数量
    size_t capacity;                   // 数组容量
} TypeOntology;
```

## API接口

### 类型本体管理

#### kos_ontology_create

```c
TypeOntology* kos_ontology_create(const char* domain_name);
```

创建空的本体。

**参数**：
- `domain_name`: 领域名称（如 "manufacturing"）

**返回**：新创建的本体指针，失败返回 NULL

**示例**：
```c
TypeOntology* ontology = kos_ontology_create("manufacturing");
```

#### kos_ontology_free

```c
void kos_ontology_free(TypeOntology* ontology);
```

释放本体，递归释放所有资源。

**参数**：
- `ontology`: 要释放的本体指针

### 类型定义管理

#### kos_ontology_add_type_definition

```c
int kos_ontology_add_type_definition(TypeOntology* ontology,
                                     const char* name,
                                     kos_term* type_def,
                                     kos_term* ctx);
```

添加类型定义。`type_def` 必须是通过类型构造器（`kos_mk_pi`, `kos_mk_sigma`, `kos_mk_sum`等）构造的 `kos_term*`。

**参数**：
- `ontology`: 本体指针
- `name`: 类型名称
- `type_def`: 类型定义（kos_term*），通过类型构造器构造
- `ctx`: 类型定义的上下文（可选，用于依赖类型）

**返回**：成功返回 0，失败返回 -1

**示例**：
```c
// 构造FailEvt类型：Σ(b: BatchID). Σ(err: ErrorCode). Σ(t: Time). Prop
kos_term* batch_id_type = kos_mk_id("BatchID");
kos_term* error_code_type = kos_mk_prop("ErrorCode");
kos_term* time_type = kos_mk_time("Time");
kos_term* prop_type = kos_mk_prop("Prop");

kos_term* time_prop_sigma = kos_mk_sigma(time_type, prop_type);
kos_term* error_time_prop_sigma = kos_mk_sigma(error_code_type, time_prop_sigma);
kos_term* fail_evt_type = kos_mk_sigma(batch_id_type, error_time_prop_sigma);

kos_ontology_add_type_definition(ontology, "FailEvt", fail_evt_type, NULL);
```

#### kos_ontology_find_type_definition

```c
kos_term* kos_ontology_find_type_definition(TypeOntology* ontology, const char* name);
```

查找类型定义，返回类型定义的 `kos_term*`。

**参数**：
- `ontology`: 本体指针
- `name`: 类型名称

**返回**：类型定义的 `kos_term*`，未找到返回 NULL

**示例**：
```c
kos_term* fail_evt_type = kos_ontology_find_type_definition(ontology, "FailEvt");
```

#### kos_ontology_get_type_definition_info

```c
TypeDefinition* kos_ontology_get_type_definition_info(TypeOntology* ontology, const char* name);
```

获取类型定义的完整信息（包括名称、类型定义、上下文）。

**参数**：
- `ontology`: 本体指针
- `name`: 类型名称

**返回**：类型定义信息指针，未找到返回 NULL

#### kos_ontology_remove_type_definition

```c
int kos_ontology_remove_type_definition(TypeOntology* ontology, const char* name);
```

删除类型定义。

**参数**：
- `ontology`: 本体指针
- `name`: 类型名称

**返回**：成功返回 0，失败返回 -1

#### kos_ontology_update_type_definition

```c
int kos_ontology_update_type_definition(TypeOntology* ontology,
                                        const char* name,
                                        kos_term* new_type_def,
                                        kos_term* new_ctx);
```

更新类型定义。

**参数**：
- `ontology`: 本体指针
- `name`: 类型名称
- `new_type_def`: 新的类型定义
- `new_ctx`: 新的上下文

**返回**：成功返回 0，失败返回 -1

### 类型实例化

#### kos_ontology_mk_type_instance

```c
kos_term* kos_ontology_mk_type_instance(TypeOntology* ontology,
                                        const char* type_name,
                                        kos_term* data_term,
                                        kos_term* ctx);
```

根据类型定义构造类型实例。实例化过程：
1. 查找类型定义（type_def : kos_term*）
2. 通过类型检查器验证 data_term 是否符合类型构造条件
3. 如果通过类型检查，返回构造的实例；否则返回 NULL

**参数**：
- `ontology`: 本体指针
- `type_name`: 类型名称
- `data_term`: 要实例化的数据项（必须是 kos_term* 类型）
- `ctx`: 类型检查的上下文

**返回**：验证通过的实例（kos_term*），失败返回 NULL

**示例**：
```c
// 构造FailEvt实例
kos_term* batch_id = kos_mk_id("Batch_202310-01");
kos_term* error_code = kos_mk_prop("HARD_ERR");
kos_term* time_val = kos_mk_time("1696233600");
kos_term* proof = kos_mk_prop("Proof(QAShift)");

kos_term* time_proof_pair = kos_mk_pair(time_val, proof);
kos_term* error_time_proof_pair = kos_mk_pair(error_code, time_proof_pair);
kos_term* fail_evt_data = kos_mk_pair(batch_id, error_time_proof_pair);

kos_term* instance = kos_ontology_mk_type_instance(ontology, "FailEvt", fail_evt_data, ctx);
if (instance) {
    // 实例化成功
} else {
    // 类型检查失败
}
```

#### kos_ontology_validate_instance

```c
bool kos_ontology_validate_instance(TypeOntology* ontology,
                                    kos_term* instance,
                                    const char* type_name,
                                    kos_term* ctx);
```

验证实例是否符合类型定义（通过类型检查）。使用 `kos_check` 或 `kos_type_check` 验证 instance 是否符合 type_def。

**参数**：
- `ontology`: 本体指针
- `instance`: 要验证的实例
- `type_name`: 类型名称
- `ctx`: 类型检查的上下文

**返回**：验证通过返回 true，失败返回 false

**示例**：
```c
kos_term* instance = ...;  // 某个实例
bool valid = kos_ontology_validate_instance(ontology, instance, "FailEvt", ctx);
if (valid) {
    // 实例符合类型定义
} else {
    // 实例不符合类型定义
}
```

### 持久化存储

#### kos_ontology_save_to_file

```c
int kos_ontology_save_to_file(TypeOntology* ontology, const char* filename);
```

保存本体到文件（JSON格式）。类型定义（kos_term*）会被序列化为 JSON。

**参数**：
- `ontology`: 本体指针
- `filename`: 文件名

**返回**：成功返回 0，失败返回 -1

#### kos_ontology_load_from_file

```c
TypeOntology* kos_ontology_load_from_file(const char* filename);
```

从文件加载本体。JSON 会被反序列化为 kos_term* 类型定义。

**参数**：
- `filename`: 文件名

**返回**：加载的本体指针，失败返回 NULL

#### kos_ontology_serialize

```c
char* kos_ontology_serialize(TypeOntology* ontology);
```

序列化本体为JSON字符串。

**参数**：
- `ontology`: 本体指针

**返回**：JSON字符串，失败返回 NULL（需要调用者释放）

#### kos_ontology_deserialize

```c
TypeOntology* kos_ontology_deserialize(const char* json_str);
```

从JSON字符串反序列化本体。

**参数**：
- `json_str`: JSON字符串

**返回**：反序列化的本体指针，失败返回 NULL

### 类型构造辅助函数

这些辅助函数是对 `kos_mk_pi`, `kos_mk_sigma`, `kos_mk_sum` 的封装，提供更语义化的接口。

#### kos_ontology_mk_sigma_type

```c
kos_term* kos_ontology_mk_sigma_type(kos_term* domain, kos_term* body);
```

构造依赖和类型（Σ类型），用于事件类型等。

**参数**：
- `domain`: 域类型
- `body`: 体类型（依赖域）

**返回**：构造的Σ类型（kos_term*）

**示例**：
```c
kos_term* batch_id_type = kos_mk_id("BatchID");
kos_term* error_code_type = kos_mk_prop("ErrorCode");
kos_term* sigma_type = kos_ontology_mk_sigma_type(batch_id_type, error_code_type);
```

#### kos_ontology_mk_pi_type

```c
kos_term* kos_ontology_mk_pi_type(kos_term* domain, kos_term* body);
```

构造依赖积类型（Π类型），用于谓词类型等。

**参数**：
- `domain`: 域类型
- `body`: 体类型（依赖域）

**返回**：构造的Π类型（kos_term*）

**示例**：
```c
kos_term* batch_id_type = kos_mk_id("BatchID");
kos_term* prop_type = kos_mk_prop("Prop");
kos_term* pi_type = kos_ontology_mk_pi_type(batch_id_type, prop_type);
```

#### kos_ontology_mk_sum_type

```c
kos_term* kos_ontology_mk_sum_type(kos_term* left_type, kos_term* right_type);
```

构造和类型（Sum类型），用于联合类型。

**参数**：
- `left_type`: 左类型
- `right_type`: 右类型

**返回**：构造的Sum类型（kos_term*）

**示例**：
```c
kos_term* success_type = kos_mk_prop("Success");
kos_term* failure_type = kos_mk_prop("Failure");
kos_term* sum_type = kos_ontology_mk_sum_type(success_type, failure_type);
```

## 使用流程

### 1. 创建本体

```c
TypeOntology* ontology = kos_ontology_create("manufacturing");
```

### 2. 定义类型

```c
// 定义FailEvt类型
kos_term* fail_evt_type = ...;  // 通过类型构造器构造
kos_ontology_add_type_definition(ontology, "FailEvt", fail_evt_type, NULL);
```

### 3. 实例化类型

```c
// 构造数据项
kos_term* data_term = ...;  // 构造数据项

// 实例化（包含类型检查）
kos_term* instance = kos_ontology_mk_type_instance(ontology, "FailEvt", data_term, ctx);
```

### 4. 验证实例

```c
// 验证实例
bool valid = kos_ontology_validate_instance(ontology, instance, "FailEvt", ctx);
```

### 5. 持久化

```c
// 保存到文件
kos_ontology_save_to_file(ontology, "manufacturing_ontology.json");

// 释放资源
kos_ontology_free(ontology);
```

## 注意事项

1. **类型定义必须是类型构造的产物**：所有类型定义都必须是通过类型构造器（`kos_mk_pi`, `kos_mk_sigma`, `kos_mk_sum`等）构造的 `kos_term*`
2. **类型实例化必须通过类型检查**：实例化时必须通过 `kos_check` 验证数据项是否符合类型构造条件
3. **不存在C语言结构体定义的类型**：所有类型都通过类型构造器构造，不存在预定义的C语言结构体类型
4. **类型定义和类型实例都是 `kos_term*`**：类型定义本身是类型系统中的项，类型实例也是类型系统中的项































