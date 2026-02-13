"""
知识图谱数据：与 monograph kos-tl-knowledge-graph.html 中 nodes/edges 结构兼容。
step 参数表示轨迹步，用于控制“当前可见”的项节点（σ 中随演化步出现的项）。
"""
from typing import Any

# 节点：id, label, group, title, description
NODES = [
    {"id": "BatchID", "label": "BatchID", "group": "atom", "title": "批次标识符类型", "description": "基础原子类型"},
    {"id": "Machine", "label": "Machine", "group": "atom", "title": "机器/设备类型", "description": "基础原子类型"},
    {"id": "Time", "label": "Time", "group": "atom", "title": "时间类型", "description": "Unix时间戳或逻辑时钟"},
    {"id": "ErrorCode", "label": "ErrorCode", "group": "atom", "title": "错误代码类型", "description": "基础原子类型"},
    {"id": "Param", "label": "Param", "group": "atom", "title": "参数类型", "description": "基础原子类型"},
    {"id": "Val", "label": "Val", "group": "atom", "title": "值类型", "description": "基础原子类型"},
    {"id": "Voucher", "label": "Voucher", "group": "atom", "title": "凭证类型", "description": "基础原子类型"},
    {"id": "Action", "label": "Action", "group": "atom", "title": "动作类型", "description": "基础原子类型"},
    {"id": "FailEvt", "label": "FailEvt", "group": "composite", "title": "失效事件类型", "description": "Σ(b:BatchID).Σ(err:ErrorCode).Σ(t:Time).Proof(t∈Shift_QA)"},
    {"id": "ProcStep", "label": "ProcStep", "group": "composite", "title": "生产过程类型", "description": "Σ(b:BatchID).Σ(m:Machine).Σ(dur:Time×Time).Proof(InRoute(b,m))"},
    {"id": "Anomaly", "label": "Anomaly", "group": "composite", "title": "环境异常类型", "description": "Σ(m:Machine).Σ(p:Param).Σ(v:Val).Σ(t:Time)"},
    {"id": "CausalProof", "label": "CausalProof", "group": "composite", "title": "因果证明类型", "description": "Σ(e:ProcStep).Prop_causal(a,e,f)"},
    {"id": "RootCauseReport", "label": "RootCauseReport", "group": "composite", "title": "根因报告", "description": "Σ(f:FailEvt).Σ(a:Anomaly).CausalProof(a,f)"},
    {"id": "InRoute", "label": "InRoute", "group": "predicate", "title": "路径谓词", "description": "批次b是否允许在机器m上加工"},
    {"id": "TimeOK", "label": "TimeOK", "group": "predicate", "title": "时序一致性", "description": "(a.t∈e.dur)∧(e.dur.end<f.t)"},
    {"id": "SpaceOK", "label": "SpaceOK", "group": "predicate", "title": "空间一致性", "description": "a.m=e.m"},
    {"id": "BatchOK", "label": "BatchOK", "group": "predicate", "title": "批次一致性", "description": "e.b=f.b"},
    {"id": "PropCausal", "label": "Prop_causal", "group": "predicate", "title": "复合因果谓词", "description": "组合TimeOK、SpaceOK、BatchOK"},
    {"id": "mkFailure", "label": "mkFailure", "group": "constructor", "title": "失效事件构造函数", "description": "构造FailEvt类型的项"},
    {"id": "mkStep", "label": "mkStep", "group": "constructor", "title": "生产过程步骤构造函数", "description": "构造ProcStep类型的项"},
    {"id": "mkAnomaly", "label": "mkAnomaly", "group": "constructor", "title": "异常构造函数", "description": "构造Anomaly类型的项"},
    {"id": "mkCausalProof", "label": "mkCausalProof", "group": "constructor", "title": "因果证明构造函数", "description": "Π(a:Anomaly).Π(f:FailEvt).Π(e:ProcStep).TimeOK→SpaceOK→BatchOK→CausalProof"},
    {"id": "f0", "label": "f₀", "group": "term", "title": "失效事件实例", "description": "mkFailure(Batch_202310-01, Hardness_Issue, 10:00, π_QA)"},
    {"id": "eproc", "label": "e_proc", "group": "term", "title": "生产记录项", "description": "mkStep(Batch_202310-01, M_03, ⟨08:00,09:30⟩, π_route)"},
    {"id": "avolt", "label": "a_volt", "group": "term", "title": "电压异常实例", "description": "mkAnomaly(M_03, Voltage_Drop, 08:15, π_iot)"},
    {"id": "prfcausal", "label": "prf_causal", "group": "term", "title": "因果证明项", "description": "自动校验时序约束的证明项"},
    {"id": "ptime", "label": "p_time", "group": "term", "title": "时序一致性证明项", "description": "(a.t∈e.dur)∧(e.dur.end<f.t)"},
    {"id": "pspace", "label": "p_space", "group": "term", "title": "空间一致性证明项", "description": "a.m=e.m"},
    {"id": "pok", "label": "p_ok", "group": "term", "title": "InRoute 证明项", "description": "Proof(InRoute(Batch_202310-01, M_03))"},
    {"id": "pbatch", "label": "p_batch", "group": "term", "title": "批次一致性证明项", "description": "e.b=f.b 的证明"},
]

# 项节点在哪个轨迹步出现（与 HTML TERM_AT_STEP 一致）
TERM_AT_STEP = {
    "eproc": 1, "pok": 1,
    "avolt": 2, "f0": 2, "prfcausal": 2, "ptime": 2, "pspace": 2, "pbatch": 2,
}
# Γ 节点默认步 0
GAMMA_AT_STEP = {n["id"]: 0 for n in NODES if n["id"] not in TERM_AT_STEP}
GAMMA_AT_STEP.update({"FailEvt": 1, "Anomaly": 1, "ErrorCode": 1, "CausalProof": 2, "TimeOK": 2, "SpaceOK": 2, "BatchOK": 2, "PropCausal": 2, "RootCauseReport": 2})

EDGES_RAW = [
    {"from": "FailEvt", "to": "BatchID", "color": "#2196F3", "title": "Σ类型依赖"},
    {"from": "FailEvt", "to": "ErrorCode", "color": "#2196F3", "title": "Σ类型依赖"},
    {"from": "FailEvt", "to": "Time", "color": "#2196F3", "title": "Σ类型依赖"},
    {"from": "ProcStep", "to": "BatchID", "color": "#2196F3", "title": "Σ类型依赖"},
    {"from": "ProcStep", "to": "Machine", "color": "#2196F3", "title": "Σ类型依赖"},
    {"from": "ProcStep", "to": "Time", "color": "#2196F3", "title": "Σ类型依赖"},
    {"from": "Anomaly", "to": "Machine", "color": "#2196F3", "title": "Σ类型依赖"},
    {"from": "Anomaly", "to": "Param", "color": "#2196F3", "title": "Σ类型依赖"},
    {"from": "Anomaly", "to": "Val", "color": "#2196F3", "title": "Σ类型依赖"},
    {"from": "Anomaly", "to": "Time", "color": "#2196F3", "title": "Σ类型依赖"},
    {"from": "CausalProof", "to": "ProcStep", "color": "#2196F3", "title": "Σ类型依赖"},
    {"from": "RootCauseReport", "to": "FailEvt", "color": "#2196F3", "title": "Σ类型依赖"},
    {"from": "RootCauseReport", "to": "Anomaly", "color": "#2196F3", "title": "Σ类型依赖"},
    {"from": "RootCauseReport", "to": "CausalProof", "color": "#2196F3", "title": "Σ类型依赖"},
    {"from": "mkCausalProof", "to": "Anomaly", "color": "#4CAF50", "dashes": True, "title": "Π类型依赖"},
    {"from": "mkCausalProof", "to": "FailEvt", "color": "#4CAF50", "dashes": True, "title": "Π类型依赖"},
    {"from": "mkCausalProof", "to": "ProcStep", "color": "#4CAF50", "dashes": True, "title": "Π类型依赖"},
    {"from": "mkCausalProof", "to": "CausalProof", "color": "#4CAF50", "dashes": True, "title": "Π类型依赖"},
    {"from": "ProcStep", "to": "InRoute", "color": "#FF9800", "title": "谓词依赖"},
    {"from": "CausalProof", "to": "PropCausal", "color": "#FF9800", "title": "谓词依赖"},
    {"from": "PropCausal", "to": "TimeOK", "color": "#FF9800", "title": "谓词依赖"},
    {"from": "PropCausal", "to": "SpaceOK", "color": "#FF9800", "title": "谓词依赖"},
    {"from": "PropCausal", "to": "BatchOK", "color": "#FF9800", "title": "谓词依赖"},
    {"from": "f0", "to": "FailEvt", "color": "#F44336", "title": "实例化关系"},
    {"from": "eproc", "to": "ProcStep", "color": "#F44336", "title": "实例化关系"},
    {"from": "avolt", "to": "Anomaly", "color": "#F44336", "title": "实例化关系"},
    {"from": "prfcausal", "to": "CausalProof", "color": "#F44336", "title": "实例化关系"},
    {"from": "ptime", "to": "TimeOK", "color": "#F44336", "title": "实例化关系"},
    {"from": "pspace", "to": "SpaceOK", "color": "#F44336", "title": "实例化关系"},
    {"from": "pok", "to": "InRoute", "color": "#F44336", "title": "实例化关系"},
    {"from": "pbatch", "to": "BatchOK", "color": "#F44336", "title": "实例化关系"},
    {"from": "mkFailure", "to": "FailEvt", "color": "#F44336", "title": "构造函数映射"},
    {"from": "mkStep", "to": "ProcStep", "color": "#F44336", "title": "构造函数映射"},
    {"from": "mkAnomaly", "to": "Anomaly", "color": "#F44336", "title": "构造函数映射"},
    {"from": "mkCausalProof", "to": "CausalProof", "color": "#F44336", "title": "构造函数映射"},
]


def _step_for_node(node_id: str) -> int:
    if node_id in TERM_AT_STEP:
        return TERM_AT_STEP[node_id]
    return GAMMA_AT_STEP.get(node_id, 0)


def get_visible_node_ids(step: int) -> set[str]:
    visible = set()
    for n in NODES:
        nid = n["id"]
        need = _step_for_node(nid)
        if need <= step:
            visible.add(nid)
    return visible


def get_visible_edges(step: int) -> list[dict]:
    visible = get_visible_node_ids(step)
    return [e for e in EDGES_RAW if e["from"] in visible and e["to"] in visible]


def get_graph_data(step: int | None = None) -> dict[str, Any]:
    """返回 nodes 与 edges；step 为当前轨迹步（0-based），用于过滤可见节点与边。"""
    if step is None:
        from app.kernel_engine import get_state
        step = len(get_state().trace)
    step = max(0, step)
    visible_ids = get_visible_node_ids(step)
    nodes_out = [n for n in NODES if n["id"] in visible_ids]
    edges_out = get_visible_edges(step)
    return {"nodes": nodes_out, "edges": edges_out, "step": step}
