# KOS Kernel Layer API 参考

本文档列出 KOS Kernel 层（L1）的所有公开 API 接口。Kernel 层基于小步操作语义（Small-Step Operational Semantics），实现确定性状态演化。

## 目录

1. [核心数据结构](#核心数据结构)
2. [状态管理](#状态管理)
3. [Event Queue Manager](#event-queue-manager)
4. [Evolution Scheduler](#evolution-scheduler)
5. [前置/后置条件验证](#前置后置条件验证)
6. [知识集更新](#知识集更新)
7. [State Mirror](#state-mirror)

---

## 核心数据结构

### `kos_state_t` (状态三元组 σ = 〈K, TS, P〉)

Kernel 层的核心状态结构，对应 Kos.pdf 2.2.2 中的状态三元组模型。

```c
typedef struct {
    kos_term* K;    // 当前已验证的知识集（Knowledge Set）
    int TS;         // 逻辑时钟（Logical Clock）
    queue_t* P;     // 待处理事件队列（Pending Event Queue）
} kos_state_t;
```

**说明**：
- `K`：当前已验证的知识集，使用 Core 层的 `kos_term` 表示（通常为 Σ 链结构）
- `TS`：逻辑时钟，每次状态演化单调递增
- `P`：事件队列，存储来自 Runtime 层的精化事件对 `<e, p>`

### `queue_t` (事件队列)

FIFO 队列，用于存储待处理的事件对。

```c
typedef struct queue_node {
    kos_term* data;           // 事件对 <e, p>
    struct queue_node* next;
} queue_node;

typedef struct {
    queue_node* front;        // 队列头部
    queue_node* rear;         // 队列尾部
    size_t size;              // 队列大小
} queue_t;
```

**说明**：
- 队列中的每个节点存储一个事件对 `<e, p>`（`KOS_PAIR` 类型）
- 队列遵循严格顺序（Sequential Commit），确保因果链的唯一性

---

## 状态管理

### `kos_state_create`
创建初始状态 σ = 〈K, TS, P〉。

```c
kos_state_t* kos_state_create(kos_term* initial_K);
```

**参数**：
- `initial_K` - 初始知识集（可为 `NULL`，表示空知识集）

**返回值**：
- `kos_state_t*` - 新创建的状态，失败返回 `NULL`

**说明**：
- 初始化 `TS = 0`（逻辑时钟从 0 开始）
- 创建空的事件队列 `P`
- `initial_K` 会被深拷贝，调用者仍拥有原对象的所有权

### `kos_state_free`
释放状态（递归释放所有资源）。

```c
void kos_state_free(kos_state_t* sigma);
```

**说明**：
- 释放知识集 `K`（递归释放所有子项）
- 释放事件队列 `P`（包括队列中的所有事件对）
- 释放状态结构本身

---

## Event Queue Manager

Event Queue Manager 负责接收来自 Runtime 层的事件包 `<e, p>`，进行严格排序和依赖冲突检测（对应 Kos.pdf 2.2.3）。

### `kos_queue_create`
初始化事件队列。

```c
queue_t* kos_queue_create(void);
```

**返回值**：
- `queue_t*` - 新创建的空队列，失败返回 `NULL`

### `kos_queue_free`
释放事件队列。

```c
void kos_queue_free(queue_t* queue);
```

**说明**：
- 递归释放队列中的所有节点和事件对
- 释放队列结构本身

### `kos_queue_enqueue`
入队：添加事件对 `<e, p>` 到队列（严格顺序）。

```c
int kos_queue_enqueue(queue_t* queue, kos_term* event_pair);
```

**参数**：
- `queue` - 目标队列
- `event_pair` - 事件对（必须是 `KOS_PAIR` 类型）

**返回值**：
- `0` - 成功
- `-1` - 失败（参数无效或内存分配失败）

**说明**：
- 事件对会被深拷贝，队列拥有副本的所有权
- 队列遵循 FIFO 顺序，确保因果链的唯一性

### `kos_queue_dequeue`
出队：从队列头部取出事件对。

```c
kos_term* kos_queue_dequeue(queue_t* queue);
```

**返回值**：
- `kos_term*` - 事件对，队列为空返回 `NULL`

**说明**：
- 调用者负责释放返回的事件对
- 队列大小会相应减少

### `kos_queue_peek`
查看队列头部（不取出）。

```c
kos_term* kos_queue_peek(queue_t* queue);
```

**返回值**：
- `kos_term*` - 队列头部的事件对，队列为空返回 `NULL`

**说明**：
- 只查看不取出，队列状态不变
- 返回的事件对仍由队列管理，不应释放

### `kos_queue_is_empty`
检查队列是否为空。

```c
bool kos_queue_is_empty(queue_t* queue);
```

### `kos_queue_size`
获取队列大小。

```c
size_t kos_queue_size(queue_t* queue);
```

---

## Evolution Scheduler

Evolution Scheduler 驱动系统的核心演化循环，实现 "Peek-Verify-Reduce-Confirm" 流程（对应 Kos.pdf 2.2.3）。

### `kos_step`
STEP 算子：从队列中取出事件并执行状态演化。

```c
bool kos_step(kos_state_t* sigma);
```

**返回值**：
- `true` - 演化成功
- `false` - 演化失败（队列为空、前置条件不满足等）

**流程**：
1. **Peek**：查看队列头部事件
2. **Verify**：验证前置条件 `Pre(e)`
3. **Reduce**：执行归约操作
4. **Confirm**：验证后置条件 `Post(e)` 并更新状态

**说明**：
- 如果演化成功，事件会从队列中移除
- 逻辑时钟 `TS` 会单调递增
- 知识集 `K` 会更新为 `K' = Σ(new_fact, K)`

### `kos_kernel_step`
直接对给定事件对执行演化。

```c
bool kos_kernel_step(kos_state_t* sigma, kos_term* event_pair);
```

**参数**：
- `sigma` - 当前状态
- `event_pair` - 事件对 `<e, p>`

**返回值**：
- `true` - 演化成功
- `false` - 演化失败

**说明**：
- 不涉及队列操作，直接对给定事件执行演化
- 适用于从外部直接提供事件对的场景

### `kos_evolution_cycle`
完整的 Peek-Verify-Reduce-Confirm 循环。

```c
bool kos_evolution_cycle(kos_state_t* sigma);
```

**返回值**：
- `true` - 至少执行了一次成功的演化
- `false` - 队列为空或所有事件都失败

**说明**：
- 持续处理队列中的事件，直到队列为空或遇到错误
- 包含最大迭代次数限制（1000），防止无限循环
- 适用于批量处理事件队列的场景

---

## 前置/后置条件验证

### `kos_verify_precondition`
验证事件的前置条件 `Pre(e)`。

```c
bool kos_verify_precondition(kos_term* event_pair, kos_state_t* sigma);
```

**参数**：
- `event_pair` - 事件对 `<e, p>`
- `sigma` - 当前状态

**返回值**：
- `true` - 前置条件满足
- `false` - 前置条件不满足

**说明**：
- 使用 Core 层的 `kos_type_check` 验证 `p : e`（在当前知识集 `K` 上）
- 对应 Kos.pdf 2.2.4 的 "Closed-Loop Evolution" 设计决策

### `kos_verify_postcondition`
验证事件的后置条件 `Post(e)`。

```c
bool kos_verify_postcondition(kos_term* event_pair, kos_state_t* old_sigma, kos_state_t* new_sigma);
```

**参数**：
- `event_pair` - 事件对 `<e, p>`
- `old_sigma` - 演化前的状态
- `new_sigma` - 演化后的状态

**返回值**：
- `true` - 后置条件满足
- `false` - 后置条件不满足

**说明**：
- 验证新状态 `K'` 是否满足事件的后置条件
- 当前为简化实现，主要检查逻辑时钟是否递增
- 未来可以扩展为更精细的后置条件类型检查

---

## 知识集更新

### `kos_update_knowledge`
更新知识集（添加新证明的事实）。

```c
kos_term* kos_update_knowledge(kos_term* K, kos_term* new_fact);
```

**参数**：
- `K` - 当前知识集
- `new_fact` - 新事实（经过归约的事件）

**返回值**：
- `kos_term*` - 更新后的知识集 `K'`

**说明**：
- 使用 Σ 链结构表示知识集的单调增长：`K' = Σ(new_fact, K)`
- 如果 `K` 为空，直接返回 `new_fact` 的副本
- 如果 `K` 不为空，构造 `KOS_PAIR(new_fact, K)` 表示追加
- 调用者负责释放返回的新知识集（如果与旧知识集不同）

---

## State Mirror

State Mirror 维护最新的"真理视图"，提供一致的上下文（对应 Kos.pdf 2.2.3）。

### `kos_state_get_K`
获取当前知识集 `K`（只读视图）。

```c
const kos_term* kos_state_get_K(const kos_state_t* sigma);
```

**返回值**：
- `const kos_term*` - 当前知识集，状态为空返回 `NULL`

**说明**：
- 返回只读视图，不应修改返回的知识集
- 用于 Runtime 层的状态查询和 Core 层的上下文验证

### `kos_state_get_TS`
获取当前逻辑时钟 `TS`。

```c
int kos_state_get_TS(const kos_state_t* sigma);
```

**返回值**：
- `int` - 逻辑时钟值，状态为空返回 `-1`

### `kos_state_get_queue_size`
获取当前事件队列大小。

```c
size_t kos_state_get_queue_size(const kos_state_t* sigma);
```

**返回值**：
- `size_t` - 队列中待处理事件的数量

### `kos_state_is_empty`
检查状态是否为空（`K` 为空且队列为空）。

```c
bool kos_state_is_empty(const kos_state_t* sigma);
```

**返回值**：
- `true` - 状态为空
- `false` - 状态不为空

---

## 使用示例

### 示例 1：创建状态并处理事件

```c
#include "kos_kernel.h"
#include "kos_core.h"

int main(void) {
    // 1. 创建初始知识集 K
    kos_term* initial_K = kos_mk_prop("InitialKnowledge");
    
    // 2. 创建初始状态 σ = 〈K, TS=0, P=∅〉
    kos_state_t* sigma = kos_state_create(initial_K);
    if (!sigma) {
        fprintf(stderr, "Failed to create state\n");
        return 1;
    }
    
    // 3. 构造一个事件对 <e, p>
    kos_term* event_type = kos_mk_prop("FailEvt");
    kos_term* event_proof = kos_mk_prop("FailEvtProof");
    kos_term* event_pair = kos_mk_pair(event_type, event_proof);
    
    // 4. 将事件加入队列
    if (kos_queue_enqueue(sigma->P, event_pair) != 0) {
        fprintf(stderr, "Failed to enqueue event\n");
        kos_state_free(sigma);
        return 1;
    }
    
    // 5. 执行状态演化
    bool success = kos_step(sigma);
    printf("Evolution result: %s\n", success ? "OK" : "FAIL");
    printf("Logical clock TS: %d\n", kos_state_get_TS(sigma));
    
    // 6. 清理资源
    kos_term_free(event_pair);
    kos_term_free(initial_K);
    kos_state_free(sigma);
    
    return success ? 0 : 1;
}
```

### 示例 2：批量处理事件队列

```c
// 假设已有多个事件对 event_pairs[]
kos_state_t* sigma = kos_state_create(NULL);

// 将所有事件加入队列
for (int i = 0; i < event_count; i++) {
    kos_queue_enqueue(sigma->P, event_pairs[i]);
}

// 执行完整的演化循环
bool all_success = kos_evolution_cycle(sigma);

printf("Processed %zu events, final TS: %d\n",
       kos_state_get_queue_size(sigma),
       kos_state_get_TS(sigma));

kos_state_free(sigma);
```

---

## 设计原则（对应 Kos.pdf 2.2.4）

1. **强顺序提交（Sequential Commit）**：
   - Runtime 层可以并行精化信号，但 Kernel 层坚持顺序提交
   - 确保因果链的唯一性，呈现确定性的线性演化路径

2. **闭环演化（Closed-Loop Evolution）**：
   - 执行前验证 `Pre(e)`，执行后验证 `Post(e)`
   - 保证系统始终在"被证明为真"的状态之间转换

3. **逻辑时间与物理时间解耦**：
   - Kernel 层维护独立的逻辑顺序
   - 物理时间戳仅作为证据锚点，同步仅在 Runtime 层的物化阶段发生

4. **确定性归约**：
   - 严格禁止非确定性选择
   - 相同初始状态和事件序列会产生数学上唯一、可重现的知识视图

---

## 注意事项

1. **内存管理**：
   - `kos_state_create` 返回的状态需要用 `kos_state_free` 释放
   - `kos_queue_enqueue` 会深拷贝事件对，队列拥有副本所有权
   - `kos_queue_dequeue` 返回的事件对需要调用者释放
   - `kos_update_knowledge` 返回的新知识集需要调用者释放（如果与旧知识集不同）

2. **状态一致性**：
   - 状态演化是原子的：要么全部成功，要么全部失败
   - 逻辑时钟 `TS` 单调递增，不会回退

3. **事件格式**：
   - 所有事件对必须是 `KOS_PAIR` 类型
   - `pair.data` 是事件类型/前置条件 `e`
   - `pair.proof` 是该事件的证明 `p`

4. **知识集结构**：
   - 当前使用 Σ 链结构：`K = Σ(fact_n, Σ(fact_{n-1}, ... Σ(fact_1, NULL)))`
   - 未来可以考虑更高效的数据结构（如哈希表或树）

---

## 相关文档

- `CORE_API.md` - Core 层 API 参考
- `Kos.pdf` 2.2 - Kernel Layer 形式化规范
- `examples/kernel_example.c` - Kernel 层使用示例（待创建）
