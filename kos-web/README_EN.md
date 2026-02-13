# KOS-TL Application Demo (kos-web)

A web demo built on the **Minimal Trusted KOS Kernel (MTK)** and the three-layer architecture (Core / Kernel / Runtime): Core type and predicate loading, Runtime signal simulation, Kernel event-driven small-step evolution and **Event Log (Trace)**, plus interactive knowledge graph and trace views.

## Relation to Overall Design

This repo follows the **Deterministic Event-Sourced Knowledge Kernel** design in [New_Design_Plan.md](../New_Design_Plan.md):

- **State σ** evolves from events; all state changes are committed in the Kernel after **type checking**.
- **Trace** is the **Event Log**: an immutable append-only event sequence; supports deterministic replay (same Trace + same initial σ ⇒ same final σ).
- **State Hash** is computed from the Trace for consistency verification; recovery from crash by replaying the Event Log.

kos-web demonstrates these in the browser: Core types/predicates, Runtime signal generation, Kernel “ingest and evolve” corresponding to **elab → Verify(Pre) → Reduce → Update(K,TS) → append Event Log**, and the Trace view. See [MTK and Kernel/Runtime alignment](../docs/MTK_KERNEL_RUNTIME_ALIGNMENT.md).

## Tech Stack

### Backend
- **Framework**: FastAPI (Python)
- **No DB**: single-process in-memory state for the demo (extensible to PostgreSQL/Redis)

### Frontend
- **Framework**: React 18
- **Build**: Vite
- **Routing**: React Router
- **Styling**: plain CSS
- **Knowledge graph**: vis-network (CDN)

### Infrastructure
- **Containers**: Docker & Docker Compose (optional)

## Features vs Three Layers / MTK

| Layer / MTK | Features | Implementation |
|--------------|----------|----------------|
| **L0: Core** | Types and predicates only via **Core Load**; no preload | Core page shows current loaded content; **Core Load** supports KOS text and **Import .kos** (`POST /api/core/load`, `POST /api/core/load_file`); after load, Γ is synced to Kernel. **Constitution**: only types defined in Γ may participate in object creation; see [docs/CORE_CONSTITUTION_EN.md](docs/CORE_CONSTITUTION_EN.md) |
| **L2: Runtime** | Signal simulation, **Apply**, **Auto evolve**, log save & replay | Single/batch/random generation (sensor, QC failure, etc.); **Apply** enqueues signals that elaborate to event types in Γ; **Auto evolve** generates a sequence of given length, enqueues and runs small-steps until queue empty; logs can be saved and replayed, including “Replay & evolve”; `/api/runtime/apply`, `/api/runtime/auto_evolve`, `/api/runtime/log/save`, `/api/runtime/log/replay` |
| **L1: Kernel** | Γ, σ, event queue P, small-step, auto scenarios, materialize | Shows Γ (synced from Core), σ (K), queue P; **Step once** takes one from P, elaborates and evolves—writes to K only when type is in Γ and (optionally) passes kos-core check-term; auto root-cause/audit by event type; results materialized to Runtime; `/api/kernel/step`, `/api/kernel/state`, `/api/kernel/trace` |
| **Visualization** | Knowledge graph, evolution trajectory | `/api/graph?step=`, step and per-step scenario results |

## Quick Start

### 1. Backend (Python 3.10+)

```bash
cd kos-web/backend
python -m venv .venv
# Windows: .venv\Scripts\activate
# Linux/macOS: source .venv/bin/activate
pip install -r requirements.txt
uvicorn app.main:app --reload --host 0.0.0.0 --port 8000
```

API docs: <http://127.0.0.1:8000/docs>

### 2. Frontend

```bash
cd kos-web/frontend
npm install
npm run dev
```

Frontend: <http://127.0.0.1:5173>; Vite proxies `/api` to backend port 8000.

**Using port 80 (e.g. Docker or host Nginx):**

1. **Docker**: frontend image nginx is configured for SPA fallback; rebuild after changing `nginx.conf`: `docker compose build frontend --no-cache && docker compose up -d`.
2. **Host Nginx**: configure `try_files $uri $uri/ /index.html` and proxy `/api/` to the backend.
3. **Backend**: Kernel page calls `/api/kernel/state`; ensure `/api` is proxied to the backend when using port 80.

### 3. Usage Flow

1. **Core (first)**: In **Core Load**, paste KOS types/predicates or import a .kos file, then click “Load to Core”. When Γ is empty, Runtime cannot enqueue and Kernel cannot create objects; after load, types are synced to Kernel’s Γ.
2. **Runtime**: Generate signals (random / proc_step / anomaly / fail_evt / batch), or use **Auto evolve** (set sequence length, one-click generate and evolve). Click **Apply** to enqueue signals that elaborate to event types in Γ; you can **Save as log**, **Replay**, or **Replay & evolve** (enqueue then run small-steps automatically).
3. **Kernel**: Click “Step once” or “Run until queue empty”—writes to K and appends to Trace only when event type is in Γ (and optionally passes kos-core check-term); auto root-cause/audit by event type.
4. **Knowledge graph**: Use the step slider to view Γ and σ at that step.
5. **Trace**: View Event Log (Trace) step list and each ⟨e,p⟩.
6. **Scenarios**: Six core scenarios (root cause, counterfactual, compliance, audit, evolve idle, AI governance); after evolve or replay, use **Scenarios → Audit / Compliance** for compliance audit.

### Enabling kos-core (full scenario support)

Scenarios use simulated results by default. To call real kos-core for proofs and type checking, set `KOS_CORE_PATH` to the kos-core executable:

```bash
# Build: cd kos-core && stack build
# Executable is typically at .stack-work/dist/.../build/kos-core/kos-core(.exe)

# Linux/macOS
export KOS_CORE_PATH=/path/to/KosL/kos-core/.stack-work/dist/.../build/kos-core/kos-core

# Windows PowerShell
$env:KOS_CORE_PATH = "D:\path\to\kos-core.exe"
```

## Directory Structure

```
kos-web/
├── README.md          # Chinese
├── README_EN.md       # English (this file)
├── backend/
│   └── app/
│       ├── main.py
│       ├── core_loader.py
│       ├── sensor_simulator.py
│       ├── kernel_engine.py
│       ├── kos_core_bridge.py
│       └── ...
└── frontend/
    └── src/
        ├── api.js
        └── pages/
            ├── Core.jsx
            ├── Runtime.jsx
            ├── Kernel.jsx
            └── ...
```

## Docker

From **kos-web** directory:

- Start: `docker compose up -d`
- Build and start: `docker compose up -d --build`
- Logs: `docker compose logs -f`

- **Frontend**: <http://localhost>
- **API docs**: <http://localhost:8000/docs>

## Documentation

- **Core constitution (ZH)**: [docs/CORE_CONSTITUTION.md](docs/CORE_CONSTITUTION.md)
- **Core constitution (EN)**: [docs/CORE_CONSTITUTION_EN.md](docs/CORE_CONSTITUTION_EN.md) — object creation must satisfy Γ and optional kos-core check-term
- **Scenarios & Runtime signals (ZH)**: [docs/SCENARIOS_RUNTIME_SIGNALS.md](docs/SCENARIOS_RUNTIME_SIGNALS.md)
- **Scenarios & Runtime signals (EN)**: [docs/SCENARIOS_RUNTIME_SIGNALS_EN.md](docs/SCENARIOS_RUNTIME_SIGNALS_EN.md) — six scenarios, signal mapping, auto-evolve and replay

## References

- **Design**: [New_Design_Plan.md](../New_Design_Plan.md)
- **Kernel/Runtime alignment**: [docs/MTK_KERNEL_RUNTIME_ALIGNMENT.md](../docs/MTK_KERNEL_RUNTIME_ALIGNMENT.md)
- **Root repo**: [README.md](../README.md)
