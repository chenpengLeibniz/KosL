# KOS-TL 文档中心（中文）

本目录为 KOS-TL (Knowledge Operation System - Type Logic) 项目的完整文档索引。文档按主题分类，便于快速定位。

---

## 一、系统架构与核心 API

| 文档 | 描述 |
|------|------|
| [CORE_API.md](CORE_API.md) | L0 Core 层 API 参考：类型检查、Universe 层级、归约、类型构建器、存储 |
| [KERNEL_API.md](KERNEL_API.md) | L1 Kernel 层 API 参考：状态三元组、事件队列、迁移算子、前置/后置条件 |
| [KERNEL_API_QUICK_REFERENCE.md](KERNEL_API_QUICK_REFERENCE.md) | Kernel 层 API 快速参考 |
| [RUNTIME_API.md](RUNTIME_API.md) | L2 Runtime 层 API 参考：elab 算子、物理存储、调度中继、轨迹重放 |

---

## 二、Haskell kos-core 集成

| 文档 | 描述 |
|------|------|
| [KOS_CORE_HASKELL.md](KOS_CORE_HASKELL.md) | Haskell kos-core 实现说明 |
| [KOS_CORE_INTEGRATION.md](KOS_CORE_INTEGRATION.md) | kos-core 与 C 运行时集成方案 |
| [KOS_CORE_KOS_TEX_COMPARISON.md](KOS_CORE_KOS_TEX_COMPARISON.md) | kos-core 与 Kos.tex 形式化规范对比 |

---

## 三、语法与设计原则

| 文档 | 描述 |
|------|------|
| [KOS_SYNTAX.md](KOS_SYNTAX.md) | KOS 表达式语法规范 |
| [KOS_ADVANTAGES.md](KOS_ADVANTAGES.md) | KOS 架构优势与设计理念 |

---

## 四、形式化验证与实现

| 文档 | 描述 |
|------|------|
| [CORE_LAYER_KOS_CORE_REFACTORING.md](CORE_LAYER_KOS_CORE_REFACTORING.md) | Core 层 kos-core 重构：非法类型预防、信号→事件自动转换 |
| [ILLEGAL_TYPE_AND_SIGNAL_EVENT_IMPLEMENTATION.md](ILLEGAL_TYPE_AND_SIGNAL_EVENT_IMPLEMENTATION.md) | 非法类型预防与信号事件转换实现细节 |
| [RUNTIME_SIGNAL_PROCESS_AND_TRACEABILITY.md](RUNTIME_SIGNAL_PROCESS_AND_TRACEABILITY.md) | Runtime 信号处理与自动可追溯性 |
| [UNQUALIFIED_PRODUCT_SIGNAL_FLOW.md](UNQUALIFIED_PRODUCT_SIGNAL_FLOW.md) | 不合格产品信号运行过程演示（Batch_26_01-00001） |
| [KOS_CORE_TYPE_INFERENCE.md](KOS_CORE_TYPE_INFERENCE.md) | kos-core 类型推理集成 |
| [VALUE_DEPENDENT_PREDICATES.md](VALUE_DEPENDENT_PREDICATES.md) | 值依赖谓词纳入类型系统（gt/ge/lt/le/eq） |

---

## 五、可追溯性与质量

| 文档 | 描述 |
|------|------|
| [CAUSAL_TRACEABILITY.md](CAUSAL_TRACEABILITY.md) | 因果可追溯性设计 |
| [QUALITY_TRACEABILITY_DEMO.md](QUALITY_TRACEABILITY_DEMO.md) | 质量可追溯性演示 |

---

## 六、架构分析与建议

| 文档 | 描述 |
|------|------|
| [CORE_INDEPENDENCE_RECOMMENDATION.md](CORE_INDEPENDENCE_RECOMMENDATION.md) | Core 层独立性建议 |
| [CORE_RUNTIME_RIGOROUS_ANALYSIS.md](CORE_RUNTIME_RIGOROUS_ANALYSIS.md) | Core 与 Runtime 严格性分析 |
| [MULTI_ONTOLOGY_STORAGE_REASONING_VIZ.md](MULTI_ONTOLOGY_STORAGE_REASONING_VIZ.md) | 多本体存储、推理与可视化 |
| [PALANTIR_ALIGNMENT_ROADMAP.md](PALANTIR_ALIGNMENT_ROADMAP.md) | Palantir 对齐路线图 |
| [KERNEL_IMPLEMENTATION_SUMMARY.md](KERNEL_IMPLEMENTATION_SUMMARY.md) | Kernel 层实现总结 |

---

## 七、阶段增强与功能模块

| 文档 | 描述 |
|------|------|
| [PHASE2_ENHANCEMENT_SUMMARY.md](PHASE2_ENHANCEMENT_SUMMARY.md) | Phase 2 增强总结 |
| [PHASE3_PHASE4_ENHANCEMENT_SUMMARY.md](PHASE3_PHASE4_ENHANCEMENT_SUMMARY.md) | Phase 3/4 增强总结 |
| [API_LAYER_PHASE5_SUMMARY.md](API_LAYER_PHASE5_SUMMARY.md) | API 层 Phase 5 总结 |
| [QUERY_ENGINE_PHASE1_SUMMARY.md](QUERY_ENGINE_PHASE1_SUMMARY.md) | 查询引擎 Phase 1 总结 |
| [PERFORMANCE_OPTIMIZATION_PHASE7_SUMMARY.md](PERFORMANCE_OPTIMIZATION_PHASE7_SUMMARY.md) | 性能优化 Phase 7 总结 |
| [STREAM_PROCESSING_PHASE4_SUMMARY.md](STREAM_PROCESSING_PHASE4_SUMMARY.md) | 流处理 Phase 4 总结 |
| [VISUALIZATION_ENHANCEMENT_SUMMARY.md](VISUALIZATION_ENHANCEMENT_SUMMARY.md) | 可视化增强总结 |

---

## 八、开发与运维

| 文档 | 描述 |
|------|------|
| [CREATE_REPO_STEPS.md](CREATE_REPO_STEPS.md) | 仓库创建步骤 |
| [decisions.md](decisions.md) | 架构决策记录 (ADR) |
| [GITHUB_2FA_SETUP.md](GITHUB_2FA_SETUP.md) | GitHub 双因素认证设置 |
| [GITHUB_2FA_QUICK_START.md](GITHUB_2FA_QUICK_START.md) | GitHub 2FA 快速入门 |
| [GITHUB_PAT_QUICK_GUIDE.md](GITHUB_PAT_QUICK_GUIDE.md) | GitHub PAT 快速指南 |
| [GITHUB_SYNC_SETUP.md](GITHUB_SYNC_SETUP.md) | GitHub 同步配置 |
| [GITHUB_ACCOUNT_STATUS_CHECK.md](GITHUB_ACCOUNT_STATUS_CHECK.md) | GitHub 账户状态检查 |
| [GITHUB_API_404_EXPLANATION.md](GITHUB_API_404_EXPLANATION.md) | GitHub API 404 说明 |
| [GITHUB_REPO_404_TROUBLESHOOTING.md](GITHUB_REPO_404_TROUBLESHOOTING.md) | GitHub 仓库 404 故障排查 |
| [FIX_GITHUB_ACCOUNT_404.md](FIX_GITHUB_ACCOUNT_404.md) | 修复 GitHub 账户 404 |

---

## 快速导航

- **新手入门**：先读 [KOS_ADVANTAGES.md](KOS_ADVANTAGES.md)、[KOS_SYNTAX.md](KOS_SYNTAX.md)
- **API 开发**：参考 [CORE_API.md](CORE_API.md)、[KERNEL_API.md](KERNEL_API.md)、[RUNTIME_API.md](RUNTIME_API.md)
- **形式化集成**：参考 [KOS_CORE_INTEGRATION.md](KOS_CORE_INTEGRATION.md)、[RUNTIME_SIGNAL_PROCESS_AND_TRACEABILITY.md](RUNTIME_SIGNAL_PROCESS_AND_TRACEABILITY.md)
- **英文版索引**：见 [README_EN.md](README_EN.md)
