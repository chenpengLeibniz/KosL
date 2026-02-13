# KOS-TL Documentation Center (English)

This directory serves as the master index for all KOS-TL (Knowledge Operation System - Type Logic) project documentation. Documents are organized by topic for quick navigation.

---

## 1. System Architecture & Core APIs

| Document | Description |
|----------|-------------|
| [CORE_API.md](CORE_API.md) | L0 Core Layer API Reference: type checking, Universe levels, reduction, type builder, storage |
| [KERNEL_API.md](KERNEL_API.md) | L1 Kernel Layer API Reference: state triple, event queue, migration operator, pre/post conditions |
| [KERNEL_API_QUICK_REFERENCE.md](KERNEL_API_QUICK_REFERENCE.md) | Kernel Layer API Quick Reference |
| [RUNTIME_API.md](RUNTIME_API.md) | L2 Runtime Layer API Reference: elab operator, physical storage, scheduler relay, trajectory replay |

---

## 2. Haskell kos-core Integration

| Document | Description |
|----------|-------------|
| [KOS_CORE_HASKELL.md](KOS_CORE_HASKELL.md) | Haskell kos-core implementation notes |
| [KOS_CORE_INTEGRATION.md](KOS_CORE_INTEGRATION.md) | kos-core integration with C runtime |
| [KOS_CORE_KOS_TEX_COMPARISON.md](KOS_CORE_KOS_TEX_COMPARISON.md) | kos-core vs Kos.tex formal specification comparison |

---

## 3. Syntax & Design Principles

| Document | Description |
|----------|-------------|
| [KOS_SYNTAX.md](KOS_SYNTAX.md) | KOS expression syntax specification |
| [KOS_ADVANTAGES.md](KOS_ADVANTAGES.md) | KOS architecture advantages and design philosophy |

---

## 4. Formal Verification & Implementation

| Document | Description |
|----------|-------------|
| [CORE_LAYER_KOS_CORE_REFACTORING.md](CORE_LAYER_KOS_CORE_REFACTORING.md) | Core layer kos-core refactoring: illegal type prevention, signal→event auto-conversion |
| [ILLEGAL_TYPE_AND_SIGNAL_EVENT_IMPLEMENTATION.md](ILLEGAL_TYPE_AND_SIGNAL_EVENT_IMPLEMENTATION.md) | Illegal type prevention and signal-event conversion implementation details |
| [RUNTIME_SIGNAL_PROCESS_AND_TRACEABILITY.md](RUNTIME_SIGNAL_PROCESS_AND_TRACEABILITY.md) | Runtime signal processing and automatic traceability |
| [UNQUALIFIED_PRODUCT_SIGNAL_FLOW.md](UNQUALIFIED_PRODUCT_SIGNAL_FLOW.md) | Unqualified product signal flow demo (Batch_26_01-00001) |
| [KOS_CORE_TYPE_INFERENCE.md](KOS_CORE_TYPE_INFERENCE.md) | kos-core type inference integration |

---

## 5. Traceability & Quality

| Document | Description |
|----------|-------------|
| [CAUSAL_TRACEABILITY.md](CAUSAL_TRACEABILITY.md) | Causal traceability design |
| [QUALITY_TRACEABILITY_DEMO.md](QUALITY_TRACEABILITY_DEMO.md) | Quality traceability demo |

---

## 6. Architecture Analysis & Recommendations

| Document | Description |
|----------|-------------|
| [CORE_INDEPENDENCE_RECOMMENDATION.md](CORE_INDEPENDENCE_RECOMMENDATION.md) | Core layer independence recommendations |
| [CORE_RUNTIME_RIGOROUS_ANALYSIS.md](CORE_RUNTIME_RIGOROUS_ANALYSIS.md) | Core and Runtime rigor analysis |
| [MULTI_ONTOLOGY_STORAGE_REASONING_VIZ.md](MULTI_ONTOLOGY_STORAGE_REASONING_VIZ.md) | Multi-ontology storage, reasoning, and visualization |
| [PALANTIR_ALIGNMENT_ROADMAP.md](PALANTIR_ALIGNMENT_ROADMAP.md) | Palantir alignment roadmap |
| [KERNEL_IMPLEMENTATION_SUMMARY.md](KERNEL_IMPLEMENTATION_SUMMARY.md) | Kernel layer implementation summary |

---

## 7. Phase Enhancements & Feature Modules

| Document | Description |
|----------|-------------|
| [PHASE2_ENHANCEMENT_SUMMARY.md](PHASE2_ENHANCEMENT_SUMMARY.md) | Phase 2 enhancement summary |
| [PHASE3_PHASE4_ENHANCEMENT_SUMMARY.md](PHASE3_PHASE4_ENHANCEMENT_SUMMARY.md) | Phase 3/4 enhancement summary |
| [API_LAYER_PHASE5_SUMMARY.md](API_LAYER_PHASE5_SUMMARY.md) | API layer Phase 5 summary |
| [QUERY_ENGINE_PHASE1_SUMMARY.md](QUERY_ENGINE_PHASE1_SUMMARY.md) | Query engine Phase 1 summary |
| [PERFORMANCE_OPTIMIZATION_PHASE7_SUMMARY.md](PERFORMANCE_OPTIMIZATION_PHASE7_SUMMARY.md) | Performance optimization Phase 7 summary |
| [STREAM_PROCESSING_PHASE4_SUMMARY.md](STREAM_PROCESSING_PHASE4_SUMMARY.md) | Stream processing Phase 4 summary |
| [VISUALIZATION_ENHANCEMENT_SUMMARY.md](VISUALIZATION_ENHANCEMENT_SUMMARY.md) | Visualization enhancement summary |

---

## 8. Development & Operations

| Document | Description |
|----------|-------------|
| [CREATE_REPO_STEPS.md](CREATE_REPO_STEPS.md) | Repository creation steps |
| [decisions.md](decisions.md) | Architecture Decision Records (ADR) |
| [GITHUB_2FA_SETUP.md](GITHUB_2FA_SETUP.md) | GitHub two-factor authentication setup |
| [GITHUB_2FA_QUICK_START.md](GITHUB_2FA_QUICK_START.md) | GitHub 2FA quick start |
| [GITHUB_PAT_QUICK_GUIDE.md](GITHUB_PAT_QUICK_GUIDE.md) | GitHub PAT quick guide |
| [GITHUB_SYNC_SETUP.md](GITHUB_SYNC_SETUP.md) | GitHub sync configuration |
| [GITHUB_ACCOUNT_STATUS_CHECK.md](GITHUB_ACCOUNT_STATUS_CHECK.md) | GitHub account status check |
| [GITHUB_API_404_EXPLANATION.md](GITHUB_API_404_EXPLANATION.md) | GitHub API 404 explanation |
| [GITHUB_REPO_404_TROUBLESHOOTING.md](GITHUB_REPO_404_TROUBLESHOOTING.md) | GitHub repo 404 troubleshooting |
| [FIX_GITHUB_ACCOUNT_404.md](FIX_GITHUB_ACCOUNT_404.md) | Fix GitHub account 404 |

---

## Quick Navigation

- **Getting Started**: Read [KOS_ADVANTAGES.md](KOS_ADVANTAGES.md), [KOS_SYNTAX.md](KOS_SYNTAX.md)
- **API Development**: See [CORE_API.md](CORE_API.md), [KERNEL_API.md](KERNEL_API.md), [RUNTIME_API.md](RUNTIME_API.md)
- **Formal Integration**: See [KOS_CORE_INTEGRATION.md](KOS_CORE_INTEGRATION.md), [RUNTIME_SIGNAL_PROCESS_AND_TRACEABILITY.md](RUNTIME_SIGNAL_PROCESS_AND_TRACEABILITY.md)
- **Chinese Index**: See [README_CN.md](README_CN.md)
