# Core Layer Constitution (KOS-TL)

This document describes how **all object creation must pass the Core layer (kos-core) constitution** in kos-web.

## Principles

1. **Types must be in Γ**: Only types that appear in the type environment Γ after Core load may be used to construct objects (enqueue, small-step, add to knowledge set K).
2. **Optional kos-core check-term**: When `KOS_CORE_PATH` is set, before appending to K the system calls `kos-core check-term` to verify that the term inhabits the type; creation is rejected if it fails.

## Implementation

| Stage | Behavior |
|-------|----------|
| **Enqueue (enqueue_signals)** | `validate_and_elaborate` succeeds only when the elaborated event type is in **Γ**; when Γ is empty, all signals are rejected. |
| **Small-step (kernel_step)** | First check event type ∈ `_known_event_types()` (= VALID_EVENT_TYPES ∩ Γ); then, when available, call `kos_core_check_term(term_expr, type_expr)`; if either fails, return error and do not write to K. |
| **RootCauseReport** | The root-cause report instance is appended to K and materialized only when the type `RootCauseReport` is defined in Γ. |
| **get_valid_event_types()** | Returns `_known_event_types()`, i.e. only those type names that are in Γ and are supported event types; when Γ is empty, returns an empty list. |

## Source and Validation of Γ

- **Γ source**: Injected only via Core load (paste KOS, upload .kos, or single type definition). `sync_gamma_from_core()` copies Core’s types/predicates/constructors into Kernel’s `state.gamma`.
- **Constitution at Core load**: When `load_from_kos(..., use_kos_core=True)` is used, content is validated with `kos_core_check_file` before parsing; default is `use_kos_core=False`, to be enabled by the caller if desired.

## Configuration

- **KOS_CORE_PATH**: Path to the kos-core executable. When unset, check-term is skipped; only the “type in Γ” rule is enforced.
- Event-pair to KOS term/type serialization for check-term is in `kernel_engine._event_pair_to_kos_expr`; adjust there if constructor signatures in your .kos differ, or rely on “type in Γ” only.

## Summary

- **Enforced**: Enqueue, small-step, and RootCauseReport are all gated by Γ; no object creation when Γ is empty.
- **Optional**: With kos-core configured, object creation is also validated by check-term so that terms satisfy their types.
