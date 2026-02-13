// include/kos_causal_trace.h
// 因果关系追溯：因果事件索引、证据搜索、因果链构建

#ifndef KOS_CAUSAL_TRACE_H
#define KOS_CAUSAL_TRACE_H

#include "kos_manufacturing.h"
#include "kos_kernel.h"
#include <stdbool.h>
#include <stddef.h>

// ========== 因果事件索引 ==========
// 用于高效搜索因果链：按批次、机器、时间索引 FailEvt / ProcStep / Anomaly

typedef struct kos_causal_index kos_causal_index_t;

kos_causal_index_t* kos_causal_index_create(void);
void kos_causal_index_free(kos_causal_index_t* idx);

// 从知识集 K 构建索引（解析 K 中的 FailEvt/ProcStep/Anomaly 字符串表示）
int kos_causal_index_build_from_K(kos_causal_index_t* idx, const kos_term* K);

// 从知识库 KB 构建索引（解析 KB 项中的 ProcStep/Anomaly/FailEvt 格式）
// 补齐路线：见 docs/KOS_TL_APPLICATION_VALUE_ANALYSIS.md Phase 1.2
int kos_causal_index_build_from_kb(kos_causal_index_t* idx, const void* kb);

// 从 KB/K 构建索引时排除指定异常（用于反事实推理：假设该异常未发生）
int kos_causal_index_build_from_kb_excluding_anomaly(kos_causal_index_t* idx,
                                                      const void* kb,
                                                      const Anomaly* exclude);
int kos_causal_index_build_from_K_excluding_anomaly(kos_causal_index_t* idx,
                                                      const kos_term* K,
                                                      const Anomaly* exclude);

// 显式注册事件（用于外部数据源或增量更新）
int kos_causal_index_add_fail_evt(kos_causal_index_t* idx, FailEvt evt);
int kos_causal_index_add_proc_step(kos_causal_index_t* idx, ProcStep step);
int kos_causal_index_add_anomaly(kos_causal_index_t* idx, Anomaly anomaly);

// 释放根因报告（含内部 kos_term*）
void kos_root_cause_report_free(RootCauseReport* report);

// ========== 因果证据搜索 ==========

// 因果证据链（Anomaly -> ProcStep -> FailEvt）
typedef struct {
    Anomaly anomaly;
    ProcStep process_step;
    FailEvt failure;
    bool valid;  /* 已通过因果有效性验证 */
} kos_causal_chain_t;

// 搜索因果证据：给定失败事件，从索引中查找满足因果约束的 (Anomaly, ProcStep) 对
// 返回因果链数组，*out_count 为数量，调用方负责 free 数组
kos_causal_chain_t* kos_causal_search_evidence(kos_causal_index_t* idx,
                                                FailEvt failure,
                                                size_t* out_count);

// 释放因果链数组
void kos_causal_chains_free(kos_causal_chain_t* chains, size_t count);

#endif /* KOS_CAUSAL_TRACE_H */
