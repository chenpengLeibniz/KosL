# 本体类型在内存中的表示和存储

## 概述

从本体文件（JSON格式）加载的类型在内存中通过**类型论的结构**表示，所有类型定义都是通过类型构造器（Π、Σ、Sum等）构造的 `kos_term*` 结构。

## 内存数据结构层次

### 1. 顶层结构：`TypeOntology`

**定义位置**：`include/kos_ontology.h:24`

```c
typedef struct {
    char domain_name[64];              // 领域名称，如 "manufacturing"
    TypeDefinition* type_definitions;  // 类型定义数组（动态数组）
    size_t type_count;                 // 类型定义数量
    size_t capacity;                   // 数组容量（用于动态扩展）
} TypeOntology;
```

**内存布局**：
```
TypeOntology (在堆上分配)
├── domain_name[64]          (64字节，固定大小)
├── type_definitions*        (指针，指向 TypeDefinition 数组)
├── type_count               (size_t，当前类型数量)
└── capacity                 (size_t，数组容量)
```

**存储位置**：
- `TypeOntology` 结构本身：堆内存（通过 `calloc` 分配）
- `type_definitions` 数组：堆内存（通过 `realloc` 动态扩展）

### 2. 类型定义条目：`TypeDefinition`

**定义位置**：`include/kos_ontology.h:16`

```c
typedef struct {
    char* name;              // 类型名称（如 "BatchID", "FailEvt"）
    kos_term* type_def;      // 类型定义（kos_term*），通过类型构造器构造
    kos_term* ctx;           // 类型定义的上下文（可选，用于依赖类型）
} TypeDefinition;
```

**内存布局**：
```
TypeDefinition (在数组中，每个元素)
├── name*                   (指针，指向堆上分配的字符串)
├── type_def*               (指针，指向 kos_term 结构)
└── ctx*                    (指针，指向 kos_term 结构，可为 NULL)
```

**存储位置**：
- `TypeDefinition` 数组：堆内存（通过 `realloc` 动态扩展）
- `name` 字符串：堆内存（通过 `malloc` 分配）
- `type_def` 和 `ctx`：堆内存（通过 `kos_term_copy` 深拷贝）

### 3. 类型定义核心：`kos_term`

**定义位置**：`include/kos_core.h:42`

```c
typedef struct kos_term {
    term_kind kind;                    // 类型种类（KOS_SIGMA, KOS_PI, KOS_SUM等）
    universe_info universe;            // Universe层级信息（用于类型检查）
    union {
        struct { 
            char* val; 
            struct kos_term* type; 
        } atomic;                      // 基础数据（用于VAL, TIME, ID, PROP）
        
        struct { 
            struct kos_term* data;     // 数据部分
            struct kos_term* proof;    // 证明部分 - Σ-Types核心
        } pair;                        // <d, p> - 带证数据对
        
        struct { 
            struct kos_term* domain;   // 域类型
            struct kos_term* body;      // 体类型（依赖域）
        } sigma;                       // Σ(x:A).B - 依赖和类型
        
        struct {
            struct kos_term* domain;   // 域类型 A
            struct kos_term* body;     // 体类型 B（依赖域）
            struct kos_term* body_term;// λ抽象体（可选，用于构造）
        } pi;                          // Π(x:A).B - 依赖积类型
        
        struct {
            struct kos_term* left_type;  // 左类型 A
            struct kos_term* right_type; // 右类型 B
            bool is_left;                // true = inl, false = inr
            struct kos_term* value;      // 值（A或B类型的项）
        } sum;                          // A + B - 和类型
        
        struct {
            universe_axis axis;         // UNIVERSE_COMPUTATIONAL (U_i) 或 UNIVERSE_LOGICAL (Type_i)
            int level;                   // 层级 i
        } universe;                      // Universe类型（U_i 或 Type_i）
    } data;
} kos_term;
```

**内存布局**（以 Σ 类型为例）：
```
kos_term (在堆上分配)
├── kind = KOS_SIGMA
├── universe = {axis, level}
└── data.sigma
    ├── domain*  (指向另一个 kos_term，表示域类型)
    └── body*    (指向另一个 kos_term，表示体类型，可能是嵌套的Σ类型)
```

## 内存存储示例

### 示例1：`FailEvt` 类型的内存表示

**类型定义**：`FailEvt = Σ(b: BatchID). Σ(err: ErrorCode). Σ(t: Time). Prop`

**内存结构**：
```
TypeOntology (堆)
└── type_definitions[0]
    ├── name* → "FailEvt" (堆)
    ├── type_def* → kos_term (堆)
    │   ├── kind = KOS_SIGMA
    │   ├── universe = {UNIVERSE_LOGICAL, 1}
    │   └── data.sigma
    │       ├── domain* → kos_term (堆)  // BatchID
    │       │   ├── kind = KOS_ID
    │       │   └── data.atomic.val* → "BatchID"
    │       └── body* → kos_term (堆)    // Σ(err: ErrorCode). Σ(t: Time). Prop
    │           ├── kind = KOS_SIGMA
    │           └── data.sigma
    │               ├── domain* → kos_term (堆)  // ErrorCode
    │               └── body* → kos_term (堆)    // Σ(t: Time). Prop
    │                   ├── kind = KOS_SIGMA
    │                   └── data.sigma
    │                       ├── domain* → kos_term (堆)  // Time
    │                       └── body* → kos_term (堆)     // Prop
    │                           ├── kind = KOS_PROP
    │                           └── data.atomic.val* → "Prop"
    └── ctx* = NULL
```

### 示例2：`InRoute` 谓词类型的内存表示

**类型定义**：`InRoute = Π(b: BatchID). Π(m: Machine). Prop`

**内存结构**：
```
TypeDefinition
├── name* → "InRoute" (堆)
├── type_def* → kos_term (堆)
│   ├── kind = KOS_PI
│   ├── universe = {UNIVERSE_LOGICAL, 1}
│   └── data.pi
│       ├── domain* → kos_term (堆)  // BatchID
│       └── body* → kos_term (堆)    // Π(m: Machine). Prop
│           ├── kind = KOS_PI
│           └── data.pi
│               ├── domain* → kos_term (堆)  // Machine
│               └── body* → kos_term (堆)     // Prop
└── ctx* = NULL
```

## 加载过程

### 1. 从文件加载（当前状态）

**函数**：`kos_ontology_load_from_file()` (`src/core/ontology_manager.c:346`)

**流程**：
```c
1. 打开文件，读取文件大小
2. 分配内存，读取整个文件内容到 json_str
3. 调用 kos_ontology_deserialize(json_str) 反序列化
4. 释放 json_str
5. 返回 TypeOntology*
```

**注意**：`kos_ontology_deserialize()` 目前**未实现**（返回 NULL），需要实现完整的 JSON 解析。

### 2. 反序列化过程（待实现）

**函数**：`kos_ontology_deserialize()` (`src/core/ontology_manager.c:612`)

**预期流程**：
```c
1. 解析JSON对象，提取：
   - domain_name
   - type_count
   - type_definitions 数组

2. 创建 TypeOntology 结构：
   - 调用 kos_ontology_create(domain_name)

3. 对每个类型定义：
   a. 提取 name
   b. 反序列化 type_def：
      - 调用 kos_term_deserialize(json_type_def)
      - 这会递归解析 kos_term 结构（根据 kind 类型）
   c. 反序列化 ctx（如果存在）：
      - 调用 kos_term_deserialize(json_ctx)
   d. 调用 kos_ontology_add_type_definition() 添加类型定义

4. 返回 TypeOntology*
```

### 3. 添加类型定义

**函数**：`kos_ontology_add_type_definition()` (`src/core/ontology_manager.c:87`)

**内存操作**：
```c
1. 检查容量，必要时扩展数组（realloc）
2. 分配并复制名称（malloc + strcpy）
3. 深拷贝类型定义（kos_term_copy）
4. 深拷贝上下文（如果提供，kos_term_copy）
5. 增加 type_count
```

**深拷贝**：使用 `kos_term_copy()` 递归复制整个 `kos_term` 树结构。

## 序列化过程（保存到文件）

### 1. 序列化为JSON

**函数**：`kos_ontology_serialize()` (`src/core/ontology_manager.c:439`)

**流程**：
```c
1. 创建动态缓冲区（初始8KB，可扩展）
2. 写入 JSON 对象开始标记
3. 写入 domain_name
4. 写入 type_count
5. 写入 type_definitions 数组开始标记
6. 对每个类型定义：
   a. 写入名称
   b. 序列化 type_def：
      - 调用 kos_term_serialize(def->type_def)
      - 这会递归序列化整个 kos_term 树结构为 JSON
   c. 序列化 ctx（如果存在）
7. 写入数组和对象结束标记
8. 返回 JSON 字符串
```

### 2. kos_term 序列化

**函数**：`kos_term_serialize()` (`src/core/storage.c`)

**序列化规则**（根据 `kind`）：
- `KOS_SIGMA`: `{"kind":"SIGMA","domain":...,"body":...}`
- `KOS_PI`: `{"kind":"PI","domain":...,"body":...}`
- `KOS_SUM`: `{"kind":"SUM","left_type":...,"right_type":...}`
- `KOS_ID`: `{"kind":"ID","val":"..."}`
- `KOS_TIME`: `{"kind":"TIME","val":"..."}`
- `KOS_PROP`: `{"kind":"PROP","val":"..."}`
- 等等...

## 内存管理

### 1. 分配

- **TypeOntology**: `calloc(1, sizeof(TypeOntology))`
- **TypeDefinition 数组**: `realloc()` 动态扩展
- **名称字符串**: `malloc(strlen(name) + 1)`
- **kos_term**: `kos_term_copy()` 内部使用 `calloc` 分配

### 2. 释放

**函数**：`kos_ontology_free()` (`src/core/ontology_manager.c:35`)

**释放流程**：
```c
1. 遍历 type_definitions 数组
2. 对每个 TypeDefinition：
   a. free(def->name)              // 释放名称字符串
   b. kos_term_free(def->type_def) // 递归释放类型定义树
   c. kos_term_free(def->ctx)      // 递归释放上下文树（如果存在）
3. free(ontology->type_definitions) // 释放数组
4. free(ontology)                   // 释放本体结构
```

**递归释放**：`kos_term_free()` 会递归释放整个 `kos_term` 树结构。

## 内存布局总结

```
堆内存布局（示例）：
┌─────────────────────────────────┐
│ TypeOntology                    │
│ ├─ domain_name[64]             │
│ ├─ type_definitions* ──────────┼─→ ┌──────────────────────┐
│ ├─ type_count                   │   │ TypeDefinition[0]    │
│ └─ capacity                     │   │ ├─ name* ───────────┼─→ "FailEvt"
└─────────────────────────────────┘   │ ├─ type_def* ────────┼─→ kos_term (Σ类型)
                                       │ └─ ctx* = NULL       │
                                       ├─ TypeDefinition[1]   │
                                       │ ├─ name* ────────────┼─→ "InRoute"
                                       │ ├─ type_def* ────────┼─→ kos_term (Π类型)
                                       │ └─ ctx* = NULL       │
                                       ├─ ...                 │
                                       └─ TypeDefinition[N]   │
                                         └─ ...
```

## 关键特点

1. **类型论表示**：所有类型都是通过类型构造器（Π、Σ、Sum等）构造的 `kos_term*` 结构
2. **递归结构**：类型定义是递归的树结构（嵌套的 Σ、Π 类型）
3. **深拷贝**：添加类型定义时使用深拷贝，确保独立性
4. **动态扩展**：`TypeDefinition` 数组可以动态扩展（初始容量16，按需翻倍）
5. **内存安全**：所有分配的内存都有对应的释放函数，避免内存泄漏

## 当前限制

1. **反序列化未实现**：`kos_ontology_deserialize()` 目前返回 NULL，需要实现完整的 JSON 解析
2. **JSON解析库**：当前使用简单的字符串操作，建议使用更完善的 JSON 解析库（如 cJSON）
3. **大本体优化**：对于非常大的本体（数千个类型），可能需要考虑：
   - 索引优化（哈希表查找类型定义）
   - 分片加载（只加载常用类型）
   - 内存映射（mmap）用于大文件

## 总结

从本体文件加载的类型在内存中通过以下层次结构表示：

1. **TypeOntology** → 顶层容器
2. **TypeDefinition[]** → 类型定义数组
3. **kos_term*** → 类型定义本身（递归树结构）

所有类型都是通过类型构造器构造的 `kos_term*` 结构，保持了类型论的一致性。内存管理采用深拷贝和递归释放，确保内存安全。
















