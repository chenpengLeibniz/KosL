# Core 层宪法约束（KOS-TL）

本文档说明 kos-web 中**所有对象创建必须通过 Core 层（kos-core）宪法约束**的落地方式。

## 约束原则

1. **类型必须在 Γ 中**：只有 Core 加载后出现在类型环境 Γ 中的类型，才允许被用来构造对象（入队、小步、加入知识集 K）。
2. **可选 kos-core check-term**：若配置了 `KOS_CORE_PATH`，在向 K 追加项前会调用 `kos-core check-term` 校验「项 ⊢ 类型」；未通过则拒绝创建。

## 实现要点

| 环节 | 实现 |
|------|------|
| **入队 (enqueue_signals)** | `validate_and_elaborate` 仅当精化后的事件类型在 **Γ** 中时才返回成功；Γ 为空时一律拒绝入队。 |
| **小步 (kernel_step)** | 先检查事件类型 ∈ `_known_event_types()`（= VALID_EVENT_TYPES ∩ Γ）；再在可用时调用 `kos_core_check_term(term_expr, type_expr)`；任一不通过则返回错误，不写入 K。 |
| **RootCauseReport** | 仅当类型 `RootCauseReport` 在 Γ 中定义时，才将根因报告实例追加到 K 与 materialized。 |
| **get_valid_event_types()** | 返回 `_known_event_types()`，即仅 Γ 中且属于内核支持的事件类型名；Γ 为空时返回空列表。 |

## Γ 的来源与校验

- **Γ 来源**：仅由 Core 加载（粘贴 KOS、上传 .kos、单条类型定义）注入；`sync_gamma_from_core()` 将 Core 的 types/predicates/constructors 同步到 Kernel 的 `state.gamma`。
- **Core 加载时的宪法**：调用 `load_from_kos(..., use_kos_core=True)` 时，会先经 `kos_core_check_file` 校验再解析入库；默认 `use_kos_core=False`，由上层按需开启。

## 配置

- **KOS_CORE_PATH**：指向 kos-core 可执行文件。未设置时不做 check-term，仅执行「类型在 Γ 中」的约束。
- 事件对到 KOS 项/类型的序列化（供 check-term 使用）见 `kernel_engine._event_pair_to_kos_expr`；若与当前 .kos 的构造函数签名不一致，可在此处调整或暂时依赖「类型在 Γ」约束。

## 小结

- **已满足**：入队、小步、RootCauseReport 均以 Γ 为门控；Γ 为空时不能创建任何对象。
- **可选加强**：配置 kos-core 后，对象创建前会经 check-term 校验，满足「项满足类型」的宪法要求。
