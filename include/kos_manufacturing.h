// include/kos_manufacturing.h
// Manufacturing Domain: 制造业知识库体系
// 质量异常追溯系统

#ifndef KOS_MANUFACTURING_H
#define KOS_MANUFACTURING_H

#include "kos_core.h"
#include "kos_kernel.h"
#include "kos_ontology.h"
#include <stdint.h>

// ========== 基础原子类型 ==========

// BatchID: 批次标识
typedef struct {
    char batch_id[64];  // 批次ID，如 "Batch_202310-01"
} BatchID;

// Machine: 机器/设备标识
typedef struct {
    char machine_id[64];  // 设备ID，如 "HeatTreatment_03"
    char line_id[64];     // 生产线ID
} Machine;

// Time: 时间点（Unix时间戳或逻辑时钟）
typedef uint64_t Time;

// TimeRange: 时间区间
typedef struct {
    Time start;
    Time end;
} TimeRange;

// ErrorCode: 错误代码
typedef struct {
    char code[32];  // 错误代码，如 "HARD_ERR", "HARDNESS_ISSUE"
} ErrorCode;

// Param: 参数名称
typedef struct {
    char param_name[64];  // 参数名，如 "voltage", "temperature"
} Param;

// ParamValue: 参数值
typedef struct {
    Param param;
    double value;
} ParamValue;

// ========== 谓词类型 ==========

// InRoute: 批次是否允许在指定机器上处理
// InRoute(b: BatchID, m: Machine) : Prop
typedef struct {
    BatchID batch;
    Machine machine;
    bool is_valid;  // 是否在工艺路线中
} InRoute;

// Overlap: 时间点是否在时间区间内
// Overlap(t: Time, dur: TimeRange) : Prop
typedef struct {
    Time time_point;
    TimeRange duration;
    bool overlaps;
} Overlap;

// ========== 事件类型 ==========

// FailEvt: 失败事件类型
// FailEvt ≡ Σ(b: BatchID). Σ(err: ErrorCode). Σ(t: Time). Proof(t ∈ Shift_QA)
typedef struct {
    BatchID batch;
    ErrorCode error;
    Time time;
    kos_term* qa_shift_proof;  // 证明检查时间在QA班次内
} FailEvt;

// ProcStep: 工艺步骤类型
// ProcStep ≡ Σ(b: BatchID). Σ(m: Machine). Σ(dur: TimeRange). Proof(InRoute(b, m))
typedef struct {
    BatchID batch;
    Machine machine;
    TimeRange duration;
    kos_term* route_proof;  // 证明批次在工艺路线中
} ProcStep;

// Anomaly: 环境异常类型
// Anomaly ≡ Σ(m: Machine). Σ(p: Param). Σ(v: ParamValue). Σ(t: Time)
typedef struct {
    Machine machine;
    Param param;
    ParamValue value;
    Time time;
} Anomaly;

// WaterPressureAnomaly: 水压异常（用于双异常规则）
typedef struct {
    Machine machine;
    ParamValue pressure;
    Time time;
} WaterPressureAnomaly;

// ========== 因果证明类型 ==========

// CausalProof: 因果有效性约束
// CausalProof(a, f) ≡ Σ(e: ProcStep). Prop_causal(a, e, f)
typedef struct {
    Anomaly anomaly;        // 异常 a
    FailEvt failure;        // 失败 f
    ProcStep process_step;  // 工艺步骤 e
    kos_term* temporal_proof;  // 时间逻辑证明: a.t ∈ e.dur ∧ e.dur.end < f.t
    kos_term* spatial_proof;   // 空间逻辑证明: a.m = e.m
    kos_term* batch_proof;     // 批次一致性证明: e.b = f.b
} CausalProof;

// CausalProofDual: 双异常因果证明（修改后的规则）
// CausalProof(a, f) ≡ Σ(e: ProcStep). Σ(w: WaterPressureAnomaly). Prop_joint(a, w, e, f)
typedef struct {
    Anomaly anomaly;
    WaterPressureAnomaly water_anomaly;
    FailEvt failure;
    ProcStep process_step;
    kos_term* joint_proof;  // 联合证明
} CausalProofDual;

// RootCauseReport: 根因报告
// RootCauseReport ≡ Σ(f: FailEvt). Σ(a: Anomaly). CausalProof(a, f)
typedef struct {
    FailEvt failure;
    Anomaly anomaly;
    CausalProof causal_proof;
    kos_term* report_proof;  // 完整的证明链
} RootCauseReport;

// ========== 制造业领域接口 ==========

// 类型本体管理
TypeOntology* kos_manufacturing_ontology_init(void);
int kos_manufacturing_ontology_save(TypeOntology* ontology);

// 类型构建器（基于类型本体）
kos_term* kos_mk_batch_id(const char* batch_id);
kos_term* kos_mk_machine(const char* machine_id, const char* line_id);
kos_term* kos_mk_fail_event(BatchID batch, ErrorCode error, Time time);
kos_term* kos_mk_proc_step(BatchID batch, Machine machine, TimeRange duration);
kos_term* kos_mk_anomaly(Machine machine, Param param, ParamValue value, Time time);
kos_term* kos_mk_causal_proof(Anomaly anomaly, FailEvt failure, ProcStep step);
kos_term* kos_mk_root_cause_report(FailEvt failure, Anomaly anomaly, CausalProof proof);

// 类型本体CRUD操作（在 ontology_crud.c 中实现）
int kos_manufacturing_add_atomic_type(const char* name, const char* base_type);
AtomicTypeDef* kos_manufacturing_get_atomic_type(const char* name);
void kos_manufacturing_list_atomic_types(void);
int kos_manufacturing_remove_atomic_type(const char* name);

int kos_manufacturing_add_predicate_type(const char* name, const char** param_types, int param_count);
PredicateTypeDef* kos_manufacturing_get_predicate_type(const char* name);
void kos_manufacturing_list_predicate_types(void);

int kos_manufacturing_add_event_type(const char* name, const char** field_names, 
                                    const char** field_types, int field_count);
EventTypeDef* kos_manufacturing_get_event_type(const char* name);
void kos_manufacturing_list_event_types(void);
int kos_manufacturing_remove_event_type(const char* name);

// 谓词验证
bool kos_check_in_route(BatchID batch, Machine machine, kos_term* K);
bool kos_check_overlap(Time time_point, TimeRange duration);
bool kos_check_causal_validity(CausalProof* proof);

// 追溯分析
RootCauseReport* kos_analyze_quality_traceability(FailEvt failure, kos_state_t* sigma);
kos_term* kos_search_causal_evidence(FailEvt failure, kos_state_t* sigma);

// 反事实推理
bool kos_counterfactual_test(Anomaly anomaly, FailEvt failure, kos_state_t* sigma);

#endif

