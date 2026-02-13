# Six Core Scenarios and Runtime Signal Mapping

This document describes which **Runtime** signals (signal kinds) each of the **six core scenarios** in kos-web should build or rely on for correct demo and reasoning, and explains **constitution (Γ)**, **auto-evolution**, and **replay**.

---

## Constitution and Γ (Required Reading)

- **Object creation must pass the Core layer (Γ)**: Signals can only be enqueued and small-steps can only write to the knowledge set K when the event type is defined in **Γ**. Γ is populated by **Core Load** (paste KOS, import .kos, or single type definition); after load it is synced to the Kernel.
- **When Γ is empty**: Runtime “Apply” cannot enqueue any signal; the Kernel cannot create any object. Before use, load KOS definitions that include event types (e.g. ProcStep, Anomaly, FailEvt, Sensor, QcFailure) in **Core Load**.
- See [CORE_CONSTITUTION_EN.md](CORE_CONSTITUTION_EN.md).

---

## Auto-Evolution and Replay

- **Auto evolve** (Runtime page “Auto evolve” block): The user sets a **sequence length** (e.g. 5–50). The system generates that many signals, enqueues those that elaborate to event types in Γ, and runs small-steps until the queue is empty or a step limit is reached. Results can be **saved as log** for replay and compliance audit; links to **Kernel** or **Scenarios (audit/compliance)** are provided.
- **Replay**: Under “Log & Replay”, choose **Replay** (only re-enqueue signals from a log) or **Replay & evolve** (enqueue then run small-steps until queue empty). After replay, inspect the queue and trace in Kernel, or run **Scenarios → Audit / Compliance** for compliance audit.

---

## Signal Kind Quick Reference (Runtime → Kernel)

| signal kind   | Event type after elaboration | Description |
|---------------|------------------------------|-------------|
| `proc_step`   | ProcStep                     | Production step: batch on machine (batch_id, machine, start_time, end_time) |
| `anomaly`     | Anomaly                      | Device anomaly: temp, voltage, etc. (machine, param, value, timestamp) |
| `fail_evt`    | FailEvt                      | Quality failure event (batch_id, error_code, timestamp) |
| `sensor`      | Sensor                       | Sensor reading (name, value, unit, ts) |
| `qc_failure`  | QcFailure                    | QC failure (batch, defect, severity, ts) |

---

## Scenario 1: Root Cause

- **Meaning**: Given a failure event, build RootCauseReport from proof (FailEvt + Anomaly + CausalProof).
- **Runtime should build**:
  - **At least one `fail_evt`**: Consuming it in Kernel triggers `scenario_root_cause()`.
  - **Full causal chain**: In time order: **proc_step** (e.g. 08:00–09:30 batch on M_03), **anomaly** (e.g. 08:15 M_03 voltage), **fail_evt** (e.g. 10:00 HARD_ERR). Then the trace has ProcStep, Anomaly, FailEvt and root-cause proof aligns with the timeline.
- **Flow**: Runtime generate → Apply (enqueue) → Kernel step (or run until queue empty).

---

## Scenario 2: Counterfactual

- **Meaning**: Compare whether RootCauseReport is provable in “factual ctx” (with anomaly) vs “counterfactual ctx” (no anomaly) to see if anomaly is causally necessary.
- **App env**: **Counterfactual** scenario page; run prove on two .kos contexts.
- **Runtime**: No direct consumption; scenario uses two pre-set .kos files. For extended demo you can build two traces (with vs without **anomaly**) and compare.

---

## Scenario 3: Compliance Check

- **Meaning**: Check a single signal for compliance before ingest (current: kind in allowed list).
- **App env**: **Compliance** scenario page; run check on a generated signal (compliant: proc_step / anomaly / fail_evt; non-compliant: sensor / qc_failure, etc.).
- **Runtime**: One **proc_step**, **anomaly**, or **fail_evt** for compliant; or another kind for non-compliant demo.

---

## Scenario 4: Audit Trail

- **Meaning**: Export current Kernel trace as audit JSON (step_index, ts_after, event_label, event_pair per step).
- **App env**: **Scenarios → Audit** (`/audit`); run audit export and download JSON.
- **Runtime**: Any enqueueable mix of **proc_step**, **anomaly**, **fail_evt**, **sensor**, **qc_failure**; Apply, run steps in Kernel, then run Audit to get non-empty audit.trace. Suggest 2–3 steps of different types.

---

## Scenario 5: Evolve Idle

- **Meaning**: Take up to 50 signals from the Runtime simulator and ingest until queue is empty; report step count.
- **App env**: **Scenarios → Evolve idle** (`/governance`).
- **Runtime**: Prepare mixed signals (batch or from history); Apply to enqueue, then either run “Evolve idle” in scenario (consumes simulator buffer) or run step_drain in Kernel for strict consistency.

---

## Scenario 6: Verify AI (Type Check)

- **Meaning**: Check whether a term (term_expr) satisfies a type (type_expr) via kos-core check-term.
- **App env**: **Scenarios → AI governance** (`/ai-governance`); input term/type and run check.
- **Runtime**: None; input is only term_expr and type_expr strings.

---

## Summary Table

| Scenario        | Runtime signals to build | Notes |
|-----------------|--------------------------|--------|
| 1 Root cause    | **fail_evt** (required); full chain: **proc_step** → **anomaly** → **fail_evt** | Root cause runs when consuming fail_evt; chain needs step and anomaly first |
| 2 Counterfactual | None (or two traces: with/without **anomaly**) | Mainly pre-set .kos prove comparison |
| 3 Compliance     | One **proc_step** / **anomaly** / **fail_evt** (compliant) or other kind (non-compliant) | Scenario page can generate proc_step |
| 4 Audit          | Any **proc_step**, **anomaly**, **fail_evt**, **sensor**, **qc_failure** (several) | Enqueue, run steps, then run audit export |
| 5 Evolve idle   | Mixed **proc_step**, **anomaly**, **fail_evt**, **sensor**, **qc_failure** | Enqueue then drain or use scenario’s evolve |
| 6 AI type check  | None | term/type expressions only |

Signals are produced in Runtime by “Generate”, “From history”, or “Auto evolve”; they are enqueued by “Apply” **only when the elaborated event type is in Γ**. Auto-evolution and replay support saving sequences as logs and using them for compliance audit.
