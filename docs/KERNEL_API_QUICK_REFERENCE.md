# KOS Kernel Layer API 快速参考

## 核心数据结构

```c
// 状态三元组 σ = 〈K, TS, P〉
typedef struct {
    kos_term* K;    // 当前已验证的知识集
    int TS;         // 逻辑时钟（单调递增）
    queue_t* P;     // 待处理事件队列（FIFO）
} kos_state_t;

// 事件队列
typedef struct {
    queue_node* front;
    queue_node* rear;
    size_t size;
} queue_t;
```

---

## 1. 状态管理

### `kos_state_create`
创建初始状态 σ = 〈K, TS=0, P=∅〉

```c
kos_state_t* kos_state_create(kos_term* initial_K);
```

**参数**：
- `initial_K` - 初始知识集（可为 `NULL`）

**返回值**：
- `kos_state_t*` - 新创建的状态，失败返回 `NULL`

---

### `kos_state_free`
释放状态（递归释放所有资源）

```c
void kos_state_free(kos_state_t* sigma);
```

---

## 2. Event Queue Manager

### `kos_queue_create`
初始化事件队列

```c
queue_t* kos_queue_create(void);
```

**返回值**：
- `queue_t*` - 新创建的空队列，失败返回 `NULL`

---

### `kos_queue_free`
释放事件队列

```c
void kos_queue_free(queue_t* queue);
```

---

### `kos_queue_enqueue`
入队：添加事件对 `<e, p>` 到队列（严格顺序）

```c
int kos_queue_enqueue(queue_t* queue, kos_term* event_pair);
```

**参数**：
- `queue` - 目标队列
- `event_pair` - 事件对（必须是 `KOS_PAIR` 类型）

**返回值**：
- `0` - 成功
- `-1` - 失败

**说明**：
- 事件对会被深拷贝，队列拥有副本的所有权

---

### `kos_queue_dequeue`
出队：从队列头部取出事件对

```c
kos_term* kos_queue_dequeue(queue_t* queue);
```

**返回值**：
- `kos_term*` - 事件对，队列为空返回 `NULL`

**说明**：
- 调用者负责释放返回的事件对

---

### `kos_queue_peek`
查看队列头部（不取出）

```c
kos_term* kos_queue_peek(queue_t* queue);
```

**返回值**：
- `kos_term*` - 队列头部的事件对，队列为空返回 `NULL`

**说明**：
- 只查看不取出，队列状态不变

---

### `kos_queue_is_empty`
检查队列是否为空

```c
bool kos_queue_is_empty(queue_t* queue);
```

---

### `kos_queue_size`
获取队列大小

```c
size_t kos_queue_size(queue_t* queue);
```

---

## 3. Evolution Scheduler

### `kos_step`
STEP 算子：从队列中取出事件并执行状态演化

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

---

### `kos_kernel_step`
直接对给定事件对执行演化

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

---

### `kos_evolution_cycle`
完整的 Peek-Verify-Reduce-Confirm 循环

```c
bool kos_evolution_cycle(kos_state_t* sigma);
```

**返回值**：
- `true` - 至少执行了一次成功的演化
- `false` - 队列为空或所有事件都失败

**说明**：
- 持续处理队列中的事件，直到队列为空或遇到错误
- 包含最大迭代次数限制（1000），防止无限循环

---

## 4. 前置/后置条件验证

### `kos_verify_precondition`
验证事件的前置条件 `Pre(e)`

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

---

### `kos_verify_postcondition`
验证事件的后置条件 `Post(e)`

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

---

## 5. 知识集更新

### `kos_update_knowledge`
更新知识集（添加新证明的事实）

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
- 调用者负责释放返回的新知识集（如果与旧知识集不同）

---

## 6. State Mirror

### `kos_state_get_K`
获取当前知识集 `K`（只读视图）

```c
const kos_term* kos_state_get_K(const kos_state_t* sigma);
```

**返回值**：
- `const kos_term*` - 当前知识集，状态为空返回 `NULL`

**说明**：
- 返回只读视图，不应修改返回的知识集

---

### `kos_state_get_TS`
获取当前逻辑时钟 `TS`

```c
int kos_state_get_TS(const kos_state_t* sigma);
```

**返回值**：
- `int` - 逻辑时钟值，状态为空返回 `-1`

---

### `kos_state_get_queue_size`
获取当前事件队列大小

```c
size_t kos_state_get_queue_size(const kos_state_t* sigma);
```

**返回值**：
- `size_t` - 队列中待处理事件的数量

---

### `kos_state_is_empty`
检查状态是否为空（`K` 为空且队列为空）

```c
bool kos_state_is_empty(const kos_state_t* sigma);
```

**返回值**：
- `true` - 状态为空
- `false` - 状态不为空

---

## 使用示例

### 基本流程

```c
// 1. 创建初始状态
kos_term* initial_K = kos_mk_prop("InitialKnowledge");
kos_state_t* sigma = kos_state_create(initial_K);

// 2. 构造事件并加入队列
kos_term* event_pair = kos_mk_pair(event_type, event_proof);
kos_queue_enqueue(sigma->P, event_pair);

// 3. 执行演化
bool success = kos_step(sigma);

// 4. 查询状态
int TS = kos_state_get_TS(sigma);
const kos_term* K = kos_state_get_K(sigma);

// 5. 清理
kos_state_free(sigma);
```

### 批量处理

```c
// 将所有事件加入队列
for (int i = 0; i < event_count; i++) {
    kos_queue_enqueue(sigma->P, event_pairs[i]);
}

// 执行完整的演化循环
kos_evolution_cycle(sigma);
```

---

## API 分类统计

| 分类 | API 数量 | 说明 |
|------|---------|------|
| 状态管理 | 2 | 创建/释放状态 |
| Event Queue Manager | 7 | 队列操作（创建/释放/入队/出队/查看/状态查询） |
| Evolution Scheduler | 3 | 状态演化（单步/直接/批量） |
| 前置/后置条件验证 | 2 | 验证 Pre(e) 和 Post(e) |
| 知识集更新 | 1 | 更新知识集 K |
| State Mirror | 4 | 状态查询接口 |
| **总计** | **19** | |

---

## 相关文档

- `KERNEL_API.md` - 完整的 API 参考文档（包含详细说明和使用示例）
- `KERNEL_IMPLEMENTATION_SUMMARY.md` - Kernel 层实现总结
- `Kos.pdf` 2.2 - Kernel Layer 形式化规范
- `examples/kernel_example.c` - 使用示例代码
