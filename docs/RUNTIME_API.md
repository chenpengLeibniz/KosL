# KOS Runtime Layer API 参考

本文档列出 KOS Runtime 层（L2）的所有公开 API 接口。Runtime 层作为逻辑世界和物理世界之间的关键锚点，实现双向精化和物化。

## 目录

1. [核心数据结构](#核心数据结构)
2. [Elaboration Engine](#elaboration-engine)
3. [Physical Storage Manager](#physical-storage-manager)
4. [Scheduler Relay](#scheduler-relay)
5. [系统初始化](#系统初始化)
6. [轨迹重放和自愈](#轨迹重放和自愈)

---

## 核心数据结构

### `bitstream` (原始信号流)

```c
typedef struct {
    unsigned char* data;   // 信号数据
    size_t length;         // 数据长度
} bitstream;
```

### `signal_source_t` (信号源)

```c
typedef struct {
    runtime_signal_source source_type;  // 源类型
    void* source_handle;                // 源句柄
    const char* source_name;            // 源名称
} signal_source_t;
```

### `elaboration_record_t` (精化记录)

用于轨迹记录和重放：

```c
typedef struct {
    kos_term* event_pair;      // 精化后的事件对 <e, p>
    bitstream original_signal;  // 原始信号
    int logical_clock;         // 精化时的逻辑时钟
    size_t signal_index;       // 信号索引
} elaboration_record_t;
```

### `storage_backend_t` (存储后端)

```c
typedef struct storage_backend {
    storage_backend_type type;
    void* backend_handle;
    
    // 后端操作接口
    int (*write)(struct storage_backend* backend, const kos_term* fact, int logical_clock);
    int (*read)(struct storage_backend* backend, int logical_clock, kos_term** fact);
    int (*commit)(struct storage_backend* backend);
    int (*rollback)(struct storage_backend* backend);
    void (*free)(struct storage_backend* backend);
} storage_backend_t;
```

---

## Elaboration Engine

Elaboration Engine 负责连接物理 I/O 设备，监控外部中断和传感器数据流，执行双向精化（对应 Kos.pdf 2.3.3）。

### `kos_elab`
elab 算子：将物理比特流映射为带逻辑证明的事件对象 `<e, p>`。

```c
kos_term* kos_elab(bitstream raw_signal, kos_term* ontology);
```

**参数**：
- `raw_signal` - 原始信号（物理比特流）
- `ontology` - 本体（知识集），用于构造证明

**返回值**：
- `kos_term*` - 事件对 `<e, p>`（`KOS_PAIR` 类型），失败返回 `NULL`

**说明**：
- 如果无法构造证明，返回 `NULL`（逻辑防火墙）
- 对应 Kos.pdf 2.3.4 的 "Input Filtering Decision Based on Elaboration"

### `kos_elab_batch`
批量精化：处理多个信号。

```c
kos_term** kos_elab_batch(bitstream* signals, size_t count, kos_term* ontology, size_t* success_count);
```

**返回值**：
- `kos_term**` - 成功精化的事件对数组，调用者负责释放

### `kos_elab_record`
记录精化轨迹（用于重放和自愈）。

```c
elaboration_record_t* kos_elab_record(kos_term* event_pair, bitstream original_signal, int logical_clock);
```

### `kos_elab_record_free`
释放精化记录。

```c
void kos_elab_record_free(elaboration_record_t* record);
```

---

## Physical Storage Manager

Physical Storage Manager 抽象底层媒体差异，管理数据库和内存映射，确保物化操作的原子性和持久性（对应 Kos.pdf 2.3.3）。

### `kos_materialize`
M 算子：将抽象逻辑结论下沉为物理动作，实现原子提交栅栏。

```c
int kos_materialize(kos_state_t* sigma, storage_backend_t* backend);
```

**参数**：
- `sigma` - 当前状态
- `backend` - 存储后端

**返回值**：
- `0` - 成功
- `-1` - 失败

**说明**：
- 实现两阶段提交（Prepare + Commit）
- 只有物理写入成功后，逻辑演化才算完成
- 对应 Kos.pdf 2.3.4 的 "Atomic Commit Fence"

### `kos_materialize_fact`
物化单个事实（两阶段提交）。

```c
int kos_materialize_fact(storage_backend_t* backend, const kos_term* fact, int logical_clock);
```

### `kos_storage_create`
创建存储后端。

```c
storage_backend_t* kos_storage_create(storage_backend_type type, const char* config);
```

**支持的后端类型**：
- `STORAGE_BACKEND_MEMORY` - 内存存储（测试用）
- `STORAGE_BACKEND_FILE` - 文件存储（待实现）
- `STORAGE_BACKEND_DATABASE` - 数据库存储（待实现）
- `STORAGE_BACKEND_CUSTOM` - 自定义后端（待实现）

### `kos_storage_free`
释放存储后端。

```c
void kos_storage_free(storage_backend_t* backend);
```

---

## Scheduler Relay

Scheduler Relay 作为 Kernel 顺序演化的缓冲区，负责多线程信号的并发接收和有序排队（对应 Kos.pdf 2.3.3）。

### `kos_signal_buffer_create`
创建信号缓冲区。

```c
signal_buffer_t* kos_signal_buffer_create(size_t capacity);
```

### `kos_signal_buffer_free`
释放信号缓冲区。

```c
void kos_signal_buffer_free(signal_buffer_t* buffer);
```

### `kos_signal_buffer_add`
添加信号到缓冲区（并发安全）。

```c
int kos_signal_buffer_add(signal_buffer_t* buffer, bitstream signal);
```

**返回值**：
- `0` - 成功
- `-1` - 失败

### `kos_signal_buffer_take`
从缓冲区取出信号（有序，FIFO）。

```c
bitstream kos_signal_buffer_take(signal_buffer_t* buffer);
```

**返回值**：
- `bitstream` - 信号，缓冲区为空返回空信号

### `kos_signal_buffer_is_empty`
检查缓冲区是否为空。

```c
bool kos_signal_buffer_is_empty(signal_buffer_t* buffer);
```

### `kos_signal_buffer_size`
获取缓冲区大小。

```c
size_t kos_signal_buffer_size(signal_buffer_t* buffer);
```

---

## 系统初始化

### `kos_runtime_init`
初始化系统（创建初始状态）。

```c
kos_state_t* kos_runtime_init(kos_term* initial_ontology);
```

**参数**：
- `initial_ontology` - 初始本体（可为 `NULL`）

**返回值**：
- `kos_state_t*` - 初始状态，失败返回 `NULL`

### `kos_runtime_free`
释放运行时系统。

```c
void kos_runtime_free(kos_state_t* sigma);
```

### `kos_capture_physical_signal`
捕获物理信号（从标准输入或API）。

```c
bitstream kos_capture_physical_signal(signal_source_t* source);
```

**参数**：
- `source` - 信号源（可为 `NULL`，表示标准输入）

### `kos_signal_source_create`
创建信号源。

```c
signal_source_t* kos_signal_source_create(runtime_signal_source type, void* handle, const char* name);
```

**支持的源类型**：
- `RUNTIME_SOURCE_STDIN` - 标准输入
- `RUNTIME_SOURCE_FILE` - 文件
- `RUNTIME_SOURCE_SOCKET` - 网络套接字（待实现）
- `RUNTIME_SOURCE_SENSOR` - 传感器设备（待实现）
- `RUNTIME_SOURCE_CUSTOM` - 自定义源（待实现）

### `kos_signal_source_free`
释放信号源。

```c
void kos_signal_source_free(signal_source_t* source);
```

---

## 轨迹重放和自愈

基于确定性归约特性，Runtime 层支持轨迹重放和灾后自愈（对应 Kos.pdf 2.3.4）。

### `kos_replay_elaboration_trajectory`
重放精化轨迹（用于灾后自愈）。

```c
int kos_replay_elaboration_trajectory(elaboration_record_t* records, size_t count,
                                      kos_state_t* sigma, storage_backend_t* backend);
```

**返回值**：
- `0` - 所有事件重放成功
- `-1` - 部分或全部事件重放失败

### `kos_save_elaboration_trajectory`
保存精化轨迹到文件。

```c
int kos_save_elaboration_trajectory(elaboration_record_t* records, size_t count, const char* filename);
```

### `kos_load_elaboration_trajectory`
从文件加载精化轨迹。

```c
elaboration_record_t* kos_load_elaboration_trajectory(const char* filename, size_t* count);
```

**返回值**：
- `elaboration_record_t*` - 精化记录数组，调用者负责释放
- `count` - 记录数量（通过输出参数返回）

---

## 使用示例

### 示例 1：基本精化和物化流程

```c
#include "kos_runtime.h"
#include "kos_kernel.h"

int main(void) {
    // 1. 初始化系统
    kos_state_t* sigma = kos_runtime_init(NULL);
    
    // 2. 创建存储后端
    storage_backend_t* backend = kos_storage_create(STORAGE_BACKEND_MEMORY, NULL);
    
    // 3. 捕获物理信号
    bitstream signal = kos_capture_physical_signal(NULL);
    
    // 4. 精化信号（逻辑防火墙）
    kos_term* event_pair = kos_elab(signal, sigma->K);
    if (!event_pair) {
        printf("Signal rejected by logical firewall\n");
        return 1;
    }
    
    // 5. 将事件加入队列
    kos_queue_enqueue(sigma->P, event_pair);
    
    // 6. 执行演化
    bool success = kos_step(sigma);
    
    // 7. 物化到物理存储（原子提交栅栏）
    if (success) {
        kos_materialize(sigma, backend);
    }
    
    // 8. 清理
    kos_storage_free(backend);
    kos_runtime_free(sigma);
    
    return 0;
}
```

### 示例 2：使用 Scheduler Relay 处理并发信号

```c
// 创建信号缓冲区
signal_buffer_t* buffer = kos_signal_buffer_create(100);

// 并发接收信号（多线程场景）
for (int i = 0; i < thread_count; i++) {
    bitstream signal = capture_signal_from_thread(i);
    kos_signal_buffer_add(buffer, signal);
}

// 有序处理信号（单线程，保证因果顺序）
while (!kos_signal_buffer_is_empty(buffer)) {
    bitstream signal = kos_signal_buffer_take(buffer);
    kos_term* event_pair = kos_elab(signal, sigma->K);
    if (event_pair) {
        kos_queue_enqueue(sigma->P, event_pair);
        kos_step(sigma);
    }
}

kos_signal_buffer_free(buffer);
```

### 示例 3：轨迹重放和自愈

```c
// 加载轨迹
size_t count = 0;
elaboration_record_t* records = kos_load_elaboration_trajectory("trajectory.bin", &count);

// 重放轨迹
kos_state_t* recovered_sigma = kos_runtime_init(NULL);
storage_backend_t* backend = kos_storage_create(STORAGE_BACKEND_MEMORY, NULL);

int result = kos_replay_elaboration_trajectory(records, count, recovered_sigma, backend);

if (result == 0) {
    printf("Trajectory replay successful\n");
} else {
    printf("Trajectory replay failed\n");
}

// 清理
for (size_t i = 0; i < count; i++) {
    kos_elab_record_free(&records[i]);
}
free(records);
kos_storage_free(backend);
kos_runtime_free(recovered_sigma);
```

---

## 设计原则（对应 Kos.pdf 2.3.4）

1. **基于精化的输入过滤**：
   - 所有输入必须通过 `elab` 算子
   - 无法构造证明的信号被拒绝（逻辑防火墙）

2. **原子提交栅栏**：
   - 两阶段提交机制（Prepare + Commit）
   - 只有物理写入成功后，逻辑时钟才正式前进

3. **资源抽象和多后端即插即用**：
   - 物化算子 `M` 设计为可插拔后端
   - 支持内存、文件、数据库等多种存储后端

4. **确定性轨迹重放和灾后自愈**：
   - 完整记录所有精化事件的原始轨迹
   - 利用 Kernel 层的确定性归约特性进行重放
   - 提供极强的逻辑健壮性和自愈能力

---

## 注意事项

1. **内存管理**：
   - `kos_elab` 返回的事件对需要调用者释放
   - `kos_elab_batch` 返回的数组需要逐个释放后释放数组本身
   - `kos_elab_record` 返回的记录需要调用 `kos_elab_record_free` 释放

2. **逻辑防火墙**：
   - `kos_elab` 返回 `NULL` 表示信号被拒绝
   - 被拒绝的信号不应进入 Kernel 层

3. **原子提交**：
   - `kos_materialize` 使用两阶段提交
   - 如果提交失败，会自动回滚

4. **轨迹记录**：
   - 建议在每次成功精化后记录轨迹
   - 轨迹文件可以用于灾后恢复和系统调试

---

## 相关文档

- `KERNEL_API.md` - Kernel 层 API 参考
- `CORE_API.md` - Core 层 API 参考
- `Kos.pdf` 2.3 - Runtime Layer 形式化规范
- `examples/runtime_example.c` - Runtime 层使用示例（待创建）
