# KOS-TL 各层 API 详细列表

基于代码分析，逐层列出对外提供的 API 及调用的下层 API。

---

## 一、Core 层 (L0) - kos_core.h

### 1.1 对外提供的 API

| API | 功能 |
|-----|------|
| **类型检查** | |
| `kos_check(ctx, term, type)` | 验证 term : type（结构式检查） |
| `kos_type_check(ctx, proof, prop)` | 验证 proof : prop，委托给 kos_check |
| `kos_check_via_kos(term_expr, type_expr, errmsg, errmsg_size)` | 通过 kos-core 校验 term : type |
| `kos_type_wellformed(type)` | 类型良构判定 |
| **Universe 系统** | |
| `kos_get_universe_info(type)` | 获取 Universe 层级信息 |
| `kos_universe_leq(u1, u2)` | Universe 层级比较 |
| `kos_universe_lift_to_logic(type)` | U_i : Type_{i+1} |
| `kos_prop_embed_to_data(prop)` | Prop ↪ U_1 |
| **归约** | |
| `kos_reduce(t)` | β/ι 规约 |
| **类型构建** | |
| `kos_mk_atomic(val, type)` | 原子项 |
| `kos_mk_prop(prop_name)` | 命题 Prop P |
| `kos_mk_val(val)` | 值类型 |
| `kos_mk_time(time_val)` | 时间类型 |
| `kos_mk_id(id_val)` | 标识符 |
| `kos_mk_universe_computational(level)` | U_i |
| `kos_mk_universe_logical(level)` | Type_i |
| `kos_mk_pair(data, proof)` | 对 <d, p> |
| `kos_mk_sigma(domain, body)` | Σ(x:A).B |
| `kos_mk_pi(domain, body)` | Π(x:A).B |
| `kos_mk_lambda(domain, body_term)` | λ 抽象 |
| `kos_mk_app(func, arg)` | 函数应用 |
| `kos_mk_sum(left_type, right_type)` | A + B |
| `kos_mk_inl(left_type, right_type, value)` | inl 注入 |
| `kos_mk_inr(left_type, right_type, value)` | inr 注入 |
| `kos_mk_case(sum_term, left_branch, right_branch)` | case 分析 |
| `kos_mk_split(pair_term, body_term)` | split 消除 |
| `kos_mk_gt(left, right)` | a > b : Prop |
| `kos_mk_ge(left, right)` | a >= b |
| `kos_mk_lt(left, right)` | a < b |
| `kos_mk_le(left, right)` | a <= b |
| `kos_mk_eq(left, right)` | a == b |
| **内存与序列化** | |
| `kos_term_copy(t)` | 深拷贝 |
| `kos_term_free(t)` | 释放 |
| `kos_term_serialize(t)` | 序列化为 JSON |
| `kos_term_deserialize(json_str)` | 从 JSON 反序列化 |
| `kos_serialized_free(s)` | 释放序列化数据 |
| `kos_term_save_to_file(t, filename)` | 保存到文件 |
| `kos_term_load_from_file(filename)` | 从文件加载 |
| `kos_knowledge_save(K, filename)` | 保存知识集 |
| `kos_knowledge_load(filename)` | 加载知识集 |

### 1.2 Core 层内部调用的 API

| 实现文件 | 调用的 API |
|----------|------------|
| **type_checker.c** | `kos_reduce`, `kos_type_wellformed`, `kos_check`（递归）, `kos_get_universe_info`, `kos_universe_leq`, `kos_substitute`, `kos_term_free` |
| **type_checker.c** (kos_check_via_kos) | `kos_core_bridge_available`, `kos_core_bridge_check_expr` |
| **reduction.c** | `kos_reduce`（递归）, `kos_term_copy`, `kos_term_free` |
| **substitution.c** | `kos_term_copy`, `kos_term_free`, `kos_substitute`（递归） |
| **universe.c** | `kos_mk_universe_computational`, `kos_mk_universe_logical`, `kos_get_universe_info`, `kos_term_copy` |
| **type_builder.c** | `kos_mk_atomic`（内部） |
| **storage.c** | `kos_term_serialize`, `kos_term_deserialize` |

---

## 二、Bridge 层 - kos_core_bridge.h

### 2.1 对外提供的 API

| API | 功能 | 对应 kos-core 命令 |
|-----|------|-------------------|
| **路径配置** | |
| `kos_core_bridge_set_path(path)` | 设置 kos-core 可执行路径 |
| `kos_core_bridge_get_path()` | 获取当前路径 |
| **模块校验** | |
| `kos_core_bridge_check_file(filepath, errmsg, errmsg_size)` | 校验 .kos 文件 | `kos-core check <file>` |
| `kos_core_bridge_check_source(kos_source, errmsg, errmsg_size)` | 校验 .kos 源码字符串 | 同上（临时文件） |
| **单 Term 校验** | |
| `kos_core_bridge_check_term(term_expr, errmsg, errmsg_size)` | 校验单个项 | `kos-core term "<expr>"` |
| **Term 获取** | |
| `kos_core_bridge_term_from_kos(term_expr, errmsg, errmsg_size)` | .kos → kos_term | `kos-core json-term "<expr>"` |
| **类型推理** | |
| `kos_core_bridge_infer_from_kos(term_expr, term_out, type_out, errmsg, errmsg_size)` | 推理 (term, type) | `kos-core infer-term "<expr>"` |
| **类型检查** | |
| `kos_core_bridge_check_expr(term_expr, type_expr, errmsg, errmsg_size)` | 检查 term : type | `kos-core check-term "<term>" "<type>"` |
| **可用性** | |
| `kos_core_bridge_available()` | 检测 kos-core 是否可用 | 调用 `kos-core term "Prop P"` |

### 2.2 Bridge 层调用的 API

| 实现文件 | 调用的 API |
|----------|------------|
| **kos_core_bridge.c** | `kos_term_deserialize`, `kos_term_free`（内部解析 JSON 后） |
| **kos_core_bridge_check_source** | `kos_core_bridge_check_file`（写入临时文件后） |

---

## 三、Kernel 层 (L1) - kos_kernel.h + kos_knowledge_base.h

### 3.1 对外提供的 API

| API | 功能 |
|-----|------|
| **状态管理** | |
| `kos_state_create(initial_K)` | 创建 σ = 〈K, TS, P〉 |
| `kos_state_free(sigma)` | 释放状态 |
| **事件队列** | |
| `kos_queue_create()` | 创建空队列 |
| `kos_queue_free(queue)` | 释放队列 |
| `kos_queue_enqueue(queue, event_pair)` | 入队 |
| `kos_queue_dequeue(queue)` | 出队 |
| `kos_queue_peek(queue)` | 查看队首 |
| `kos_queue_is_empty(queue)` | 是否为空 |
| `kos_queue_size(queue)` | 队列大小 |
| **演化调度** | |
| `kos_step(sigma)` | 从队列取事件执行一步 |
| `kos_kernel_step(sigma, event_pair)` | 对给定事件对执行一步 |
| `kos_evolution_cycle(sigma)` | 完整演化循环 |
| **验证** | |
| `kos_verify_precondition(event_pair, sigma)` | 前置条件 Pre(e) |
| `kos_verify_postcondition(event_pair, old_sigma, new_sigma)` | 后置条件 Post(e) |
| **知识集** | |
| `kos_update_knowledge(K, new_fact)` | K' = Σ(new_fact, K) |
| **状态镜像** | |
| `kos_state_get_K(sigma)` | 获取知识集 K |
| `kos_state_get_TS(sigma)` | 获取逻辑时钟 |
| `kos_state_get_queue_size(sigma)` | 获取队列大小 |
| `kos_state_is_empty(sigma)` | 是否为空 |
| **知识库** | |
| `kos_state_set_kb(sigma, kb)` | 设置知识库 |
| `kos_state_get_kb(sigma)` | 获取知识库 |
| **知识库 (kos_knowledge_base.h)** | |
| `kos_kb_create()` | 创建空知识库 |
| `kos_kb_free(kb)` | 释放知识库 |
| `kos_kb_add_item(kb, id, t, A, logical_ts, source)` | 添加知识项 |
| `kos_kb_add_dependency(kb, from_id, to_id)` | 添加依赖边 |
| `kos_kb_add_dependency_typed(kb, from_id, to_id, dep_type)` | 添加带类型依赖边 |
| `kos_kb_infer_dependencies(kb)` | 推断依赖 |
| `kos_kb_find(kb, id)` | 查找项 |
| `kos_kb_export_dependency_graph_json(kb)` | 导出依赖图 JSON |
| `kos_kb_export_visualization_html(kb)` | 导出可视化 HTML |
| `kos_export_counterfactual_demo_html(kb1, kb2, cf1, cf2)` | 反事实演示 HTML |
| `kos_kb_to_sigma_chain(kb)` | 知识库 → Σ 链 |
| `kos_kb_merge_from_sigma_chain(kb, chain, ts, source)` | Σ 链 → 知识库 |

### 3.2 Kernel 层调用的 API

| 实现文件 | 调用的 API |
|----------|------------|
| **state_step.c** | `kos_term_copy`, `kos_term_free`, `kos_queue_create`, `kos_queue_free`, `kos_queue_enqueue`, `kos_queue_dequeue`, `kos_queue_peek`, `kos_queue_is_empty`, `kos_type_check`, `kos_reduce`, `kos_update_knowledge`, `kos_mk_pair`, `kos_kb_free`, `kos_kb_add_item` |
| **event_op_registry.c** | `kos_verify_precondition`, `kos_reduce`, `kos_term_copy`, `kos_term_free`, `kos_update_knowledge`, `kos_queue_dequeue`, `kos_queue_peek`, `kos_queue_is_empty` |
| **knowledge_base.c** | `kos_term_free`, `kos_term_copy`, `kos_mk_pair` |

---

## 四、Runtime 层 (L2) - kos_runtime.h

### 4.1 对外提供的 API

| API | 功能 |
|-----|------|
| **精化引擎** | |
| `kos_elab(raw_signal, ontology)` | 信号 → 事件对 |
| `kos_elab_ex(raw_signal, ontology, event_out, errmsg, errmsg_size)` | 带错误输出的精化 |
| `kos_elab_from_kos(raw_signal)` | .kos 字符串 → 事件对 |
| `kos_elab_batch(signals, count, ontology, success_count)` | 批量精化 |
| `kos_elab_record(event_pair, original_signal, logical_clock)` | 记录精化轨迹 |
| `kos_elab_record_free(record)` | 释放精化记录 |
| **信号处理** | |
| `kos_runtime_process_signal(signal, ontology, sigma, result)` | 精化 + 溯源 |
| `kos_signal_process_result_free(result)` | 释放结果 |
| `kos_runtime_register_traceability_handler(handler, free_func)` | 注册溯源处理器 |
| **物化** | |
| `kos_materialize(sigma, backend)` | 状态物化到存储 |
| `kos_storage_create(type, config)` | 创建存储后端 |
| `kos_storage_free(backend)` | 释放存储后端 |
| `kos_materialize_fact(backend, fact, logical_clock)` | 物化单个事实 |
| **信号缓冲区** | |
| `kos_signal_buffer_create(capacity)` | 创建缓冲区 |
| `kos_signal_buffer_free(buffer)` | 释放缓冲区 |
| `kos_signal_buffer_add(buffer, signal)` | 添加信号 |
| `kos_signal_buffer_take(buffer)` | 取出信号 |
| `kos_signal_buffer_is_empty(buffer)` | 是否为空 |
| `kos_signal_buffer_size(buffer)` | 缓冲区大小 |
| **系统初始化** | |
| `kos_runtime_init(initial_ontology)` | 初始化运行时 |
| `kos_runtime_free(sigma)` | 释放运行时 |
| `kos_capture_physical_signal(source)` | 捕获物理信号 |
| `kos_signal_source_create(type, handle, name)` | 创建信号源 |
| `kos_signal_source_free(source)` | 释放信号源 |
| **数据库** | |
| `kos_db_init(db_path)` | 初始化数据库 |
| `kos_db_close()` | 关闭数据库 |
| **轨迹重放** | |
| `kos_replay_elaboration_trajectory(records, count, sigma, backend)` | 重放精化轨迹 |
| `kos_save_elaboration_trajectory(records, count, filename)` | 保存轨迹 |
| `kos_load_elaboration_trajectory(filename, count)` | 加载轨迹 |

### 4.2 Runtime 层调用的 API

| 实现文件 | 调用的 API |
|----------|------------|
| **elab.c** | `kos_core_bridge_available`, `kos_elab_from_kos`, `kos_core_bridge_term_from_kos`, `kos_mk_prop`, `kos_mk_pair`, `kos_term_free`, `kos_elab`, `kos_term_copy` |
| **signal_process.c** | `kos_elab_ex`, `kos_term_free` |
| **replay.c** | `kos_queue_enqueue`, `kos_step`, `kos_materialize`, `kos_term_serialize`, `kos_serialized_free`, `kos_elab_record_free`, `kos_term_deserialize` |
| **storage_manager.c** | `kos_term_copy`, `kos_term_free` |
| **system_init.c** | `kos_state_create`, `kos_state_free` |

---

## 五、调用关系汇总表

| 调用方 | 被调用的下层/同层 API |
|--------|------------------------|
| **Core (type_checker)** | kos_reduce, kos_check, kos_type_wellformed, kos_get_universe_info, kos_universe_leq, kos_substitute, kos_term_free, kos_core_bridge_available, kos_core_bridge_check_expr |
| **Bridge** | kos_term_deserialize, kos_term_free |
| **Kernel (state_step)** | kos_term_copy, kos_term_free, kos_queue_*, kos_type_check, kos_reduce, kos_update_knowledge, kos_mk_pair, kos_kb_*, kos_queue_* |
| **Runtime (elab)** | kos_core_bridge_available, kos_core_bridge_term_from_kos, kos_mk_prop, kos_mk_pair, kos_term_free, kos_term_copy |
| **Runtime (replay)** | kos_queue_enqueue, kos_step, kos_materialize, kos_term_serialize, kos_serialized_free, kos_term_deserialize |
| **Runtime (system_init)** | kos_state_create, kos_state_free |

---

## 六、跨层依赖图（API 视角）

```
Application / Ontology / Domain
    │
    ├── kos_core_bridge_* (check_file, check_source, check_term, term_from_kos, infer_from_kos, check_expr, available)
    ├── kos_elab / kos_elab_ex / kos_elab_from_kos
    ├── kos_runtime_process_signal
    ├── kos_state_create / kos_state_free
    ├── kos_queue_enqueue / kos_kernel_step / kos_step
    └── kos_kb_*
         │
         ▼
Runtime (elab.c)
    │
    ├── kos_core_bridge_available
    ├── kos_core_bridge_term_from_kos
    ├── kos_mk_prop, kos_mk_pair
    └── kos_term_copy, kos_term_free
         │
         ▼
Kernel (state_step.c)
    │
    ├── kos_type_check  ──────► Core (type_checker)
    ├── kos_reduce      ──────► Core (reduction)
    ├── kos_update_knowledge
    ├── kos_mk_pair      ──────► Core (type_builder)
    ├── kos_term_copy, kos_term_free
    └── kos_kb_add_item  ──────► Knowledge Base
         │
         ▼
Core (type_checker)
    │
    ├── kos_check (递归)
    ├── kos_reduce
    ├── kos_type_wellformed
    ├── kos_get_universe_info, kos_universe_leq
    └── kos_check_via_kos ────► kos_core_bridge_check_expr
         │
         ▼
Bridge (kos_core_bridge.c)
    │
    └── 子进程调用 kos-core (Haskell)
```
