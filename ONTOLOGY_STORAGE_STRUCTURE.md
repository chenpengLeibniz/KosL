# 本体类型在内存中的存储方式

## 答案：**动态数组（Dynamic Array）**

本体类型在内存中是以**动态数组**的方式存储的，而不是链表。

## 数据结构定义

### TypeOntology 结构

**位置**：`include/kos_ontology.h:24`

```c
typedef struct {
    char domain_name[64];              // 领域名称，如 "manufacturing"
    TypeDefinition* type_definitions;  // 类型定义数组（动态数组指针）
    size_t type_count;                 // 类型定义数量（当前元素个数）
    size_t capacity;                   // 数组容量（分配的内存大小）
} TypeOntology;
```

**关键字段**：
- `type_definitions`：指向 `TypeDefinition` 数组的指针
- `type_count`：当前存储的类型定义数量
- `capacity`：数组的容量（已分配的内存可以容纳的元素数量）

### TypeDefinition 结构

```c
typedef struct {
    char* name;              // 类型名称（如 "BatchID", "FailEvt"）
    kos_term* type_def;      // 类型定义（kos_term*），通过类型构造器构造
    kos_term* ctx;           // 类型定义的上下文（可选，用于依赖类型）
} TypeDefinition;
```

## 内存布局

```
TypeOntology (在堆上)
├── domain_name[64]          (64字节，固定大小)
├── type_definitions* ──────┐
├── type_count = 217        │
└── capacity = 256           │
                             │
                             ↓
                    ┌────────────────────────────┐
                    │ TypeDefinition[0]          │ (堆上连续内存)
                    │ ├─ name* → "BatchID"      │
                    │ ├─ type_def* → kos_term*  │
                    │ └─ ctx* = NULL            │
                    ├─ TypeDefinition[1]        │
                    │ ├─ name* → "Machine"      │
                    │ ├─ type_def* → kos_term*  │
                    │ └─ ctx* = NULL            │
                    ├─ ...                      │
                    ├─ TypeDefinition[216]      │
                    └─ TypeDefinition[217-255]  │ (未使用，预留空间)
```

## 动态数组的实现机制

### 1. 初始化

**位置**：`src/core/ontology_manager.c:162`

```c
TypeOntology* kos_ontology_create(const char* domain_name) {
    // ...
    ontology->type_definitions = NULL;  // 初始为空
    ontology->type_count = 0;            // 初始数量为0
    ontology->capacity = 0;              // 初始容量为0
    // ...
}
```

### 2. 动态扩展（扩容）

**位置**：`src/core/ontology_manager.c:213`

```c
#define INITIAL_CAPACITY 16  // 初始容量

static int expand_capacity(TypeOntology* ontology) {
    // 计算新容量：初始16，之后每次翻倍
    size_t new_capacity = ontology->capacity == 0 
        ? INITIAL_CAPACITY 
        : ontology->capacity * 2;
    
    // 使用 realloc 扩展数组
    TypeDefinition* new_array = (TypeDefinition*)realloc(
        ontology->type_definitions, 
        new_capacity * sizeof(TypeDefinition));
    
    if (!new_array) {
        return -1;
    }
    
    ontology->type_definitions = new_array;
    ontology->capacity = new_capacity;
    
    return 0;
}
```

**扩容策略**：
- 初始容量：16
- 扩容规则：容量不足时，容量翻倍（16 → 32 → 64 → 128 → 256 → ...）
- 扩容方式：使用 `realloc()` 重新分配更大的连续内存块

### 3. 添加元素

**位置**：`src/core/ontology_manager.c:233`

```c
int kos_ontology_add_type_definition(...) {
    // 检查容量，必要时扩容
    if (ontology->type_count >= ontology->capacity) {
        if (expand_capacity(ontology) != 0) {
            return -1;
        }
    }
    
    // 直接通过数组索引访问
    TypeDefinition* new_def = &ontology->type_definitions[ontology->type_count];
    
    // 设置新元素的数据
    // ...
    
    ontology->type_count++;  // 增加计数
    return 0;
}
```

### 4. 访问元素

**位置**：`src/core/ontology_manager.c:291`

```c
TypeDefinition* kos_ontology_get_type_definition_info(TypeOntology* ontology, const char* name) {
    // 线性搜索（O(n)）
    for (size_t i = 0; i < ontology->type_count; i++) {
        if (strcmp(ontology->type_definitions[i].name, name) == 0) {
            return &ontology->type_definitions[i];  // 数组索引访问
        }
    }
    return NULL;
}
```

### 5. 遍历所有元素

**位置**：`src/core/ontology_manager.c:187`

```c
void kos_ontology_free(TypeOntology* ontology) {
    if (ontology->type_definitions) {
        // 使用 for 循环遍历数组
        for (size_t i = 0; i < ontology->type_count; i++) {
            TypeDefinition* def = &ontology->type_definitions[i];
            // 释放每个元素的资源
            // ...
        }
        free(ontology->type_definitions);  // 释放整个数组
    }
}
```

## 为什么使用数组而不是链表？

### 数组的优势

1. **内存局部性**：
   - 数组元素在内存中连续存储
   - CPU 缓存友好，访问速度快
   - 适合顺序访问和批量操作

2. **随机访问**：
   - O(1) 时间复杂度通过索引访问任意元素
   - 适合需要按索引访问的场景

3. **内存效率**：
   - 每个元素只需要存储数据，不需要额外的指针
   - 链表每个节点需要额外的 `next` 指针（8字节）

4. **实现简单**：
   - 使用 `realloc` 即可扩展
   - 不需要管理节点之间的链接关系

### 数组的劣势

1. **查找效率**：
   - 当前实现使用线性搜索（O(n)）
   - 如果类型数量很大（数千个），查找可能较慢

2. **插入/删除**：
   - 删除元素需要移动后续元素（O(n)）
   - 但类型定义通常不需要频繁删除

### 未来优化建议

如果类型数量非常大（数千个），可以考虑：

1. **哈希表索引**：
   ```c
   typedef struct {
       // ... 现有字段 ...
       HashTable* name_index;  // 名称到索引的哈希表
   } TypeOntology;
   ```
   - 查找时间复杂度：O(1)（平均情况）
   - 空间开销：额外的哈希表

2. **排序数组 + 二分查找**：
   - 保持数组按名称排序
   - 查找时间复杂度：O(log n)
   - 插入时间复杂度：O(n)（需要移动元素）

3. **混合结构**：
   - 数组存储数据（保持内存局部性）
   - 哈希表或B树提供快速查找索引

## 内存使用示例

假设有 217 个类型定义：

```
初始状态：
  capacity = 0
  type_count = 0
  type_definitions = NULL

添加第1个类型：
  capacity = 16 (初始容量)
  type_count = 1
  type_definitions → [TypeDefinition[0], ..., TypeDefinition[15]]

添加第16个类型：
  capacity = 32 (扩容：16 * 2)
  type_count = 16
  type_definitions → [TypeDefinition[0], ..., TypeDefinition[31]]

添加第32个类型：
  capacity = 64 (扩容：32 * 2)
  type_count = 32
  type_definitions → [TypeDefinition[0], ..., TypeDefinition[63]]

...

添加第217个类型：
  capacity = 256 (扩容：128 * 2)
  type_count = 217
  type_definitions → [TypeDefinition[0], ..., TypeDefinition[255]]
  (其中 TypeDefinition[217-255] 未使用)
```

## 总结

- **存储方式**：**动态数组**（Dynamic Array）
- **内存布局**：连续内存块，元素按顺序存储
- **扩容策略**：容量不足时翻倍扩容（16 → 32 → 64 → ...）
- **访问方式**：通过数组索引 `type_definitions[i]` 访问
- **查找方式**：当前使用线性搜索（O(n)），可优化为哈希表或排序数组

这种设计在类型数量适中（数百到数千）时非常高效，既保证了内存局部性，又提供了简单的实现。
















