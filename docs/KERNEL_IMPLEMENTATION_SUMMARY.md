# KOS Kernel Layer 实现总结

本文档总结 Kernel 层（L1）的完整实现，包括架构设计、API 接口和使用示例。

## 实现概览

Kernel 层已按照 Kos.pdf 2.2 节的规范完整实现，包括：

1. ✅ **状态三元组模型**：`σ = 〈K, TS, P〉`
2. ✅ **Event Queue Manager**：事件队列管理和严格顺序提交
3. ✅ **Evolution Scheduler**：Peek-Verify-Reduce-Confirm 演化循环
4. ✅ **State Mirror**：状态查询接口
5. ✅ **前置/后置条件验证**：闭环演化保证
6. ✅ **知识库 (Knowledge Base)**：Core 验证过的依赖项集合，支持依赖图可视化
7. ✅ **事件驱动小步操作**：事件类型 → 操作栈，按小步依次执行

## 核心数据结构

### 状态三元组 `kos_state_t`

```c
typedef struct {
    kos_term* K;    // 当前已验证的知识集
    int TS;         // 逻辑时钟（单调递增）
    queue_t* P;     // 待处理事件队列（FIFO）
} kos_state_t;
```

**设计要点**：
- `K` 使用 Core 层的 `kos_term` 表示，通常为 Σ 链结构
- `TS` 每次演化单调递增，确保因果链的唯一性
- `P` 使用 FIFO 队列，保证严格顺序提交

## 三大核心模块

### 1. Event Queue Manager

**职责**：接收来自 Runtime 层的事件包 `<e, p>`，进行严格排序和依赖冲突检测。

**API**：
- `kos_queue_create/free` - 队列创建/销毁
- `kos_queue_enqueue` - 入队（深拷贝，严格顺序）
- `kos_queue_dequeue` - 出队
- `kos_queue_peek` - 查看队列头部（不取出）
- `kos_queue_is_empty/size` - 队列状态查询

**设计原则**：
- 严格顺序提交（Sequential Commit）
- 事件对深拷贝，队列拥有所有权
- FIFO 顺序，确保因果链唯一性

### 2. Evolution Scheduler

**职责**：驱动系统的核心演化循环，实现 "Peek-Verify-Reduce-Confirm" 流程。

**API**：
- `kos_step` - 从队列取出事件并执行演化
- `kos_kernel_step` - 直接对给定事件执行演化
- `kos_evolution_cycle` - 批量处理事件队列的完整循环

**Peek-Verify-Reduce-Confirm 流程**：

```
1. Peek: 查看队列头部事件（不取出）
   └─> kos_queue_peek(sigma->P)

2. Verify: 验证前置条件 Pre(e)
   └─> kos_verify_precondition(event_pair, sigma)
       └─> kos_type_check(sigma->K, p, e)

3. Reduce: 执行归约操作
   └─> kos_reduce(e)
       └─> β/ι 归约

4. Confirm: 验证后置条件 Post(e) 并更新状态
   └─> kos_update_knowledge(K, reduced_e)
   └─> TS++
   └─> kos_verify_postcondition(...)
```

**设计原则**：
- 确定性归约：相同初始状态和事件序列产生唯一结果
- 原子性：演化要么全部成功，要么全部失败
- 单调性：逻辑时钟和知识集单调增长

### 3. State Mirror

**职责**：维护最新的"真理视图"，提供一致的上下文。

**API**：
- `kos_state_get_K` - 获取当前知识集（只读视图）
- `kos_state_get_TS` - 获取逻辑时钟
- `kos_state_get_queue_size` - 获取队列大小
- `kos_state_is_empty` - 检查状态是否为空

**设计原则**：
- 只读视图，不修改状态
- 用于 Runtime 层的状态查询和 Core 层的上下文验证

## 关键设计决策（对应 Kos.pdf 2.2.4）

### 1. 强顺序提交与异步精化解耦

- **问题**：高频物理信号生成与耗时逻辑验证的矛盾
- **解决方案**：Runtime 层可以并行精化，Kernel 层坚持顺序提交
- **效果**：确保因果链唯一性，呈现确定性线性演化路径

### 2. 闭环演化：前置条件验证和后置条件证据合成

- **问题**：确保操作后系统状态继续符合本体定义
- **解决方案**：
  - 执行前验证 `Pre(e)`（使用 Core 层类型检查）
  - 执行后验证 `Post(e)`（检查新状态）
- **效果**：系统始终在"被证明为真"的状态之间转换

### 3. 逻辑时间与物理时间解耦

- **问题**：分布式网络中物理时间顺序与因果逻辑不一致
- **解决方案**：
  - Kernel 层维护独立的逻辑顺序
  - 物理时间戳仅作为证据锚点
  - 同步仅在 Runtime 层的物化阶段发生
- **效果**：解决"幽灵时间"问题，确保因果顺序的绝对保真

### 4. 确定性归约

- **问题**：非确定性选择导致系统不可预测
- **解决方案**：
  - 严格禁止非确定性选择
  - 如果事件指向多个演化分支，视为 Core 层类型错误
- **效果**：相同初始状态和事件序列产生数学上唯一、可重现的知识视图

## 知识集更新机制

### Σ 链结构

知识集使用 Σ 链结构表示单调增长：

```
K' = Σ(new_fact, K)
```

**实现**：
```c
kos_term* kos_update_knowledge(kos_term* K, kos_term* new_fact) {
    if (!K) {
        return kos_term_copy(new_fact);
    }
    // K' = Σ(new_fact, K)
    return kos_mk_pair(kos_term_copy(new_fact), kos_term_copy(K));
}
```

**优点**：
- 保持知识集的单调增长
- 支持因果追溯（通过 Σ 链回溯）
- 符合依赖类型理论的语义

**未来优化**：
- 可以考虑更高效的数据结构（如哈希表或树）
- 支持知识集的去重和合并

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

## 文件结构

```
include/kos_kernel.h          # Kernel 层 API 声明
include/kos_knowledge_base.h # 知识库 API
include/kos_event_op.h       # 事件-操作栈 API
src/kernel/state_step.c      # 状态管理与演化
src/kernel/knowledge_base.c # 知识库实现
src/kernel/event_op_registry.c # 事件驱动小步实现
examples/kernel_example.c    # 使用示例
docs/KERNEL_API.md           # API 参考文档
docs/KERNEL_IMPLEMENTATION_SUMMARY.md  # 本文档
```

## 测试建议

1. **单元测试**：
   - 队列操作（enqueue/dequeue/peek）
   - 状态创建和销毁
   - 前置/后置条件验证

2. **集成测试**：
   - 完整的状态演化流程
   - 批量事件处理
   - 错误处理（无效事件、队列为空等）

3. **性能测试**：
   - 大量事件的队列处理
   - 知识集增长的性能影响
   - 内存使用情况

## 知识库 (Knowledge Base)

知识库是 Core 层验证过的依赖项集合，对应 Kos.pdf 2.2.2 中的 K = { (id_i, t_i, A_i) | Γ_Core ⊢ t_i : A_i }。**项之间的关系通过依赖实现**，支持依赖图可视化。

**来源**：
- **启动抽取**：系统启动时从关联的传统数据库或其他数据源抽取
- **运行时物化**：系统运行过程中，通过外部事件处理最终物化形成的新项（事件、根因等）

**依赖类型**（`kos_kb_dep_type_t`）：
- `KOS_KB_DEP_TYPE_REF` - 类型/项引用（term 中引用其他项）
- `KOS_KB_DEP_TYPE_SUBTERM` - 子项依赖
- `KOS_KB_DEP_TYPE_CAUSAL` - 因果依赖（如事件→根因）
- `KOS_KB_DEP_TYPE_EXPLICIT` - 显式添加的依赖

**API**（`include/kos_knowledge_base.h`）：
- `kos_kb_create/free` - 创建/释放知识库
- `kos_kb_add_item` - 添加知识项（id, t, A, logical_ts, source）
- `kos_kb_add_dependency` - 添加依赖边
- `kos_kb_add_dependency_typed` - 添加带类型的依赖边
- `kos_kb_infer_dependencies` - 从项结构推断依赖（遍历 term/type，匹配 kb 项 id）
- `kos_kb_export_dependency_graph_json` - 导出依赖图 JSON（含 label、relation）
- `kos_kb_export_visualization_html` - 导出 standalone HTML（D3.js 力导向图）
- `kos_kb_to_sigma_chain` / `kos_kb_merge_from_sigma_chain` - 与 Σ 链互转

**与状态集成**：`kos_state_set_kb(sigma, kb)` 将知识库关联到状态；演化时物化项自动加入 KB。

## 事件驱动小步操作

以事件为核心：某一类事件对应到一个可执行序列的栈，按小步操作语义依次从栈中获取对应函数执行，直到结束。

**API**（`include/kos_event_op.h`）：
- `kos_event_op_registry_create/free` - 创建/释放操作注册表
- `kos_event_op_register` - 注册事件类型 → 操作栈
- `kos_small_step_ctx_create/free` - 创建/释放小步执行上下文
- `kos_step_small` - 执行一小步（一个操作）
- `kos_evolution_cycle_small` - 持续小步执行直到队列为空

**内置操作**：`kos_op_verify_pre`、`kos_op_reduce`、`kos_op_update_knowledge`、`kos_op_confirm_post`（对应 Peek-Verify-Reduce-Update-Confirm）。

**默认行为**：未注册的事件类型使用默认操作栈（上述四步）。

## 未来扩展方向

1. **依赖冲突检测**：
   - 在 `kos_queue_enqueue` 中检测事件之间的依赖关系
   - 拒绝会导致冲突的事件

2. **后置条件验证增强**：
   - 实现更精细的后置条件类型检查
   - 支持后置条件的自动合成

3. **知识集优化**：
   - 实现知识集的去重和合并
   - 支持更高效的数据结构

4. **状态快照和回滚**：
   - 支持状态快照的创建和恢复
   - 实现确定性的状态回滚机制

5. **并发支持**：
   - 在保持顺序提交的前提下，支持多线程事件入队
   - 实现线程安全的状态查询接口

## 相关文档

- `KERNEL_API.md` - 完整的 API 参考文档
- `CORE_API.md` - Core 层 API 参考
- `Kos.pdf` 2.2 - Kernel Layer 形式化规范
- `examples/kernel_example.c` - 使用示例代码
