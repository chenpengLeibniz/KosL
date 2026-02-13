# Core 层 kos-core 接口改造总结

## 改造目标
将系统中涉及 Core 层的调用改造为调用新的 kos-core 接口，实现**非法类型不能创建**和**信号自动转化为事件**。

---

## 一、改造范围

### 1. 本体初始化（ontology_setup）

| 文件 | 改造内容 |
|------|----------|
| `src/domain/manufacturing/ontology_setup.c` | 优先使用 `kos_ontology_add_type_from_kos`，kos-core 不可用时回退到 `kos_mk_*` + `kos_ontology_add_type_definition` |
| `src/domain/finance/ontology_setup.c` | 同上 |

**改造要点：**
- 基础类型（BatchID、Machine、Time 等）：`add_type(ontology, "BatchID", "Prop BatchID", kos_mk_prop("BatchID"), ...)`
- 复合类型（FailEvt、ProcStep、Anomaly 等）：kos-core 可用时使用 .kos 表达式，不可用时用 `kos_mk_sigma`/`kos_mk_pi` 回退
- 原子类型统一使用 `Prop X`（良构），不再使用 `kos_mk_id`/`kos_mk_time` 作为类型（非良构）

### 2. 扩展类型生成（ontology_extended_generated）

| 文件 | 改造内容 |
|------|----------|
| `src/domain/manufacturing/ontology_extended_generated.c` | `add_basic_type` 优先 `kos_ontology_add_type_from_kos("Prop X")`，回退用 `kos_mk_prop` |

### 3. 本体反序列化（ontology_manager）

| 文件 | 改造内容 |
|------|----------|
| `src/core/ontology_manager.c` | `kos_ontology_deserialize` 中仅当 `kos_type_wellformed(type_def)` 时才添加类型 |
| `src/core/ontology_manager.c` | `kos_ontology_update_type_definition` 增加 `kos_type_wellformed(new_type_def)` 检查 |

### 4. 运行时更新（runtime_update）

| 文件 | 改造内容 |
|------|----------|
| `include/kos_ontology_version.h` | 新增 `kos_ontology_transaction_add_update_from_kos` 声明 |
| `src/ontology/runtime_update.c` | 实现 `kos_ontology_transaction_add_update_from_kos`，经 kos-core 校验后加入事务 |

### 5. 信号精化（elab）

| 文件 | 改造内容 |
|------|----------|
| `src/runtime/elab.c` | `kos_elab` 入口处：若信号像 .kos 表达式（以 "Prop "、"Sigma"、"Pi("、"<" 或 "{" 开头），优先调用 `kos_elab_from_kos` |

---

## 二、新增/增强接口

| 接口 | 说明 |
|------|------|
| `kos_ontology_add_type_from_kos` | 从 .kos 源添加类型（经 kos-core 校验） |
| `kos_core_bridge_term_from_kos` | 从 .kos 表达式获取经校验的 kos_term |
| `kos_elab_from_kos` | 信号为 .kos 时自动转化为事件对 |
| `kos_ontology_transaction_add_update_from_kos` | 事务中从 .kos 添加更新操作 |

---

## 三、调用路径汇总

```
本体初始化
  manufacturing_ontology_init / finance_ontology_init
    -> add_type(ontology, name, kos_expr, fallback)
       -> kos_core_bridge_available() ? kos_ontology_add_type_from_kos : kos_ontology_add_type_definition

本体反序列化
  kos_ontology_deserialize
    -> kos_term_deserialize(type_def_value)
    -> kos_type_wellformed(type_def) ? kos_ontology_add_type_definition : 跳过

本体更新
  kos_ontology_update_type_definition
    -> kos_type_wellformed(new_type_def) ? 更新 : 返回 -1

事务更新
  kos_ontology_transaction_add_update_from_kos (新增)
    -> kos_core_bridge_term_from_kos(kos_expr)
    -> kos_ontology_transaction_add_update(transaction, op, name, term, ctx)

信号精化
  kos_elab(raw_signal, ontology)
    -> 若信号像 .kos：kos_elab_from_kos(raw_signal)
    -> 否则：parse_signal_to_event + construct_proof_from_ontology
```

---

## 四、兼容性

- **kos-core 可用时**：优先使用 kos-core 校验，非法类型不能创建
- **kos-core 不可用时**：回退到 C 层 `kos_mk_*` + `kos_type_wellformed` 检查
- **原子类型**：统一使用 `Prop X`（良构），不再使用 KOS_ID/KOS_TIME 作为类型
