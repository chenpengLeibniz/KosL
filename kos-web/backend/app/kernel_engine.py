"""
Kernel 层：Γ、σ、事件队列 P；小步演化；按事件类型自动推演场景（根因/审计等）；物化到 Runtime。
与 Core 同步：Core 加载后 Γ 从 core_loader 同步；σ 随小步更新。

宪法约束（KOS-TL）：
- 所有对象创建必须通过 Core 层（Γ）的宪法约束。
- (1) 类型必须在 Γ 中定义，否则不得入队、不得执行小步、不得加入 K。
- (2) 若配置了 kos-core（KOS_CORE_PATH），创建前可选调用 check-term 校验项满足类型。
"""
from dataclasses import dataclass, field
from typing import Any

from app.core_loader import get_gamma_for_kernel
from app.sensor_simulator import RawSignal, SignalKind


@dataclass
class EventPair:
    """事件对 ⟨e, p⟩。"""
    event_type: str
    data: dict
    proof_label: str = "π"


def elaborate_signal(s: RawSignal) -> EventPair | None:
    """精化：原始信号 → 事件对。"""
    if s.kind == SignalKind.PROC_STEP:
        return EventPair("ProcStep", {
            "b": s.batch_id, "m": s.machine,
            "dur": [s.start_time, s.end_time],
            "proof": "π_route"
        }, "π_route")
    if s.kind == SignalKind.ANOMALY:
        return EventPair("Anomaly", {
            "m": s.machine, "p": s.param, "v": s.value, "t": s.timestamp,
            "proof": "π_iot"
        }, "π_iot")
    if s.kind == SignalKind.FAIL_EVT:
        return EventPair("FailEvt", {
            "b": s.batch_id, "err": s.error_code, "t": s.timestamp,
            "proof": "π_QA"
        }, "π_QA")
    if s.kind == SignalKind.SENSOR:
        return EventPair("Sensor", {"name": s.extra.get("name"), "value": s.extra.get("value"), "unit": s.extra.get("unit"), "ts": s.timestamp}, "π_sensor")
    if s.kind == SignalKind.QC_FAILURE:
        return EventPair("QcFailure", {
            "batch": s.extra.get("batch"), "defect": s.extra.get("defect"), "severity": s.extra.get("severity"), "ts": s.timestamp
        }, "π_qc")
    return None


@dataclass
class TraceStep:
    step_index: int
    ts_after: int
    event_pair: dict
    event_label: str = ""
    scenario_result: dict | None = None  # 自动场景结果（根因/审计等）


@dataclass
class KernelState:
    """σ = (K, TS, P, Γ)；P 为待处理信号队列。K 中除事件类型项（FailEvt、ProcStep 等）外，根因溯源成功时会追加 RootCauseReport 类型实例。"""
    K: list[dict] = field(default_factory=list)
    TS: int = 0
    P: list[dict] = field(default_factory=list)  # 事件队列：原始 signal dict
    gamma: dict = field(default_factory=dict)   # Γ，从 Core 同步
    trace: list[TraceStep] = field(default_factory=list)
    materialized: list[dict] = field(default_factory=list)  # 已物化到 Runtime 的项

    def to_dict(self) -> dict:
        qlen = len(self.P)
        return {
            "K": self.K,
            "TS": self.TS,
            "queue_length": qlen,
            "queue_preview": self.P[:20],
            "sigma": {"K_len": len(self.K), "TS": self.TS, "P_len": qlen},
            "gamma": self.gamma,
            "trace_length": len(self.trace),
            "trace": [
                {
                    "step_index": s.step_index,
                    "ts_after": s.ts_after,
                    "event_pair": s.event_pair,
                    "event_label": s.event_label,
                    "scenario_result": s.scenario_result,
                }
                for s in self.trace
            ],
            "materialized": self.materialized,
        }


_state: KernelState | None = None


def get_state() -> KernelState:
    global _state
    if _state is None:
        _state = KernelState()
        _state.gamma = get_gamma_for_kernel()
    return _state


def reset_state() -> KernelState:
    global _state
    _state = KernelState()
    _state.gamma = get_gamma_for_kernel()
    return _state


def sync_gamma_from_core() -> None:
    """从 Core 层同步 Γ 到当前 Kernel 状态（Core 加载后调用）。"""
    if _state is not None:
        _state.gamma = get_gamma_for_kernel()


# 有效事件类型（与 elaborate_signal 及 kernel_step 一致；仅此类可入队）
VALID_EVENT_TYPES = frozenset({"FailEvt", "ProcStep", "Anomaly", "Sensor", "QcFailure"})


def get_valid_event_types() -> list[str]:
    """返回当前可入队的事件类型列表（仅包含 Γ 中已定义的类型；Γ 为空则返回空，符合 KOS-TL）。"""
    return list(_known_event_types())


def validate_and_elaborate(signal: dict) -> tuple[bool, EventPair | None, str]:
    """
    校验信号是否能精化为有效事件对；若能则返回 (True, event_pair, "")，否则 (False, None, reason)。
    宪法约束：仅当精化成功且事件类型在 Γ 中定义时才返回 True（与 kernel_step 一致，入队即承诺可创建对象）。

    会出现无法精化的情况包括：
    1) kind 不在 (proc_step, anomaly, fail_evt, sensor, qc_failure) → 未知信号类型；
    2) elaborate_signal 返回 None；
    3) 精化得到的事件类型不在 Γ 中（或 Γ 为空）。
    """
    kind = signal.get("kind")
    if kind == "proc_step":
        s = RawSignal(
            SignalKind.PROC_STEP,
            batch_id=signal.get("batch_id"),
            machine=signal.get("machine"),
            start_time=signal.get("start_time"),
            end_time=signal.get("end_time"),
            timestamp=signal.get("timestamp"),
        )
    elif kind == "anomaly":
        s = RawSignal(
            SignalKind.ANOMALY,
            machine=signal.get("machine"),
            param=signal.get("param"),
            value=signal.get("value"),
            timestamp=signal.get("timestamp"),
        )
    elif kind == "fail_evt":
        s = RawSignal(
            SignalKind.FAIL_EVT,
            batch_id=signal.get("batch_id"),
            error_code=signal.get("error_code"),
            timestamp=signal.get("timestamp"),
        )
    elif kind == "sensor":
        ts = signal.get("ts") or signal.get("timestamp") or 0
        if isinstance(ts, (int, float)):
            pass
        else:
            ts = 0
        s = RawSignal(
            SignalKind.SENSOR,
            timestamp=ts,
            extra={
                "name": signal.get("name"),
                "value": signal.get("value"),
                "unit": signal.get("unit"),
                "ts": ts,
            },
        )
    elif kind == "qc_failure":
        ts = signal.get("ts") or signal.get("timestamp") or 0
        if not isinstance(ts, (int, float)):
            ts = 0
        s = RawSignal(
            SignalKind.QC_FAILURE,
            timestamp=ts,
            extra={
                "batch": signal.get("batch"),
                "defect": signal.get("defect"),
                "severity": signal.get("severity"),
                "ts": ts,
            },
        )
    else:
        return False, None, f"未知信号类型: {kind}，仅支持事件类型 {sorted(VALID_EVENT_TYPES)}"
    ep = elaborate_signal(s)
    if ep is None:
        return False, None, f"精化失败: kind={kind} 无法构造事件对"
    known = _known_event_types()
    if ep.event_type not in known:
        if not _gamma_type_names():
            return False, None, "Core 层（Γ）为空，无法入队；请先在 Core 加载 中加载类型与谓词。"
        return False, None, f"事件类型 {ep.event_type} 不在 Γ 中，请在 Core 加载 中定义后重试"
    return True, ep, ""


def enqueue_signals(signals: list[dict]) -> tuple[int, list[dict]]:
    """
    仅将能精化为有效事件类型的信号加入事件队列 P。
    返回 (入队数量, 被拒绝列表 [{signal, reason}]).
    """
    state = get_state()
    enqueued: list[dict] = []
    rejected: list[dict] = []
    for sig in signals:
        ok, _, err = validate_and_elaborate(sig)
        if ok:
            enqueued.append(sig)
        else:
            rejected.append({"signal": sig, "reason": err})
    state.P.extend(enqueued)
    return len(enqueued), rejected


def _gamma_type_names() -> set[str]:
    """Γ 中已定义的类型名集合。"""
    state = get_state()
    return {t.get("name") or t.get("id") for t in (state.gamma.get("types") or []) if t.get("name") or t.get("id")}


def _type_in_gamma(type_name: str) -> bool:
    """类型是否在 Γ 中定义（宪法约束：仅此类才可构造对象）。"""
    return type_name in _gamma_type_names()


def _event_pair_to_kos_expr(ep: EventPair) -> tuple[str, str] | None:
    """将事件对序列化为 kos-core check-term 所需的 (term_expr, type_expr)。无法序列化时返回 None（跳过 check-term）。"""
    d = ep.data
    e = ep.event_type
    type_expr = e
    try:
        if e == "ProcStep":
            b = d.get("b") or ""
            m = d.get("m") or ""
            dur = d.get("dur") or [0, 0]
            term_expr = f'mkProcStep "{b}" "{m}" ({dur[0]}, {dur[1]})'
            return term_expr, type_expr
        if e == "Anomaly":
            m = d.get("m") or ""
            p = d.get("p") or ""
            v = d.get("v", 0)
            t = d.get("t", 0)
            term_expr = f'mkAnomaly "{m}" "{p}" {v} {t}'
            return term_expr, type_expr
        if e == "FailEvt":
            b = d.get("b") or ""
            err = d.get("err") or ""
            t = d.get("t", 0)
            term_expr = f'mkFailEvt "{b}" "{err}" {t}'
            return term_expr, type_expr
        if e == "Sensor":
            name = d.get("name") or ""
            value = d.get("value", 0)
            unit = d.get("unit") or ""
            ts = d.get("ts", 0)
            term_expr = f'mkSensor "{name}" {value} "{unit}" {ts}'
            return term_expr, type_expr
        if e == "QcFailure":
            batch = d.get("batch") or ""
            defect = d.get("defect") or ""
            sev = d.get("severity", 0)
            ts = d.get("ts", 0)
            term_expr = f'mkQcFailure "{batch}" "{defect}" {sev} {ts}'
            return term_expr, type_expr
    except Exception:
        pass
    return None


def _kos_core_validate_term(ep: EventPair) -> tuple[bool, str]:
    """若配置了 kos-core，用 check-term 校验事件项是否满足类型；未配置或无法序列化时返回 (True, "")。"""
    try:
        from app.kos_core_bridge import kos_core_check_term
        pair = _event_pair_to_kos_expr(ep)
        if pair is None:
            return True, ""
        ok, err = kos_core_check_term(pair[0], pair[1])
        if ok:
            return True, ""
        return False, err or "check-term 失败"
    except Exception:
        return True, ""


def _known_event_types() -> set[str]:
    """当前可接受的事件类型：仅当类型在 Γ 中定义时才可创建对象（KOS-TL：Γ 为空则无可构造类型）。"""
    return set(VALID_EVENT_TYPES) & _gamma_type_names()


def kernel_step(event_pair: EventPair) -> tuple[bool, str]:
    """执行一小步演化；返回 (成功?, 错误信息)。仅当类型在 Γ 中且通过 kos-core 宪法校验（若已配置）时才创建对象。"""
    state = get_state()
    e = event_pair.event_type
    data = event_pair.data
    known = _known_event_types()
    if e not in known:
        gamma_types = state.gamma.get("types") or []
        if not gamma_types:
            return False, "Core 层（Γ）为空，无法创建对象；请先在 Core 加载 中加载类型与谓词。"
        return False, f"事件类型 {e} 不在 Γ 中（请在 Core 加载 中定义该类型后重试）"
    ok, err = _kos_core_validate_term(event_pair)
    if not ok:
        return False, f"对象未通过 kos-core 宪法约束（check-term）：{err}"
    new_item = {"type": e, "data": data, "proof": event_pair.proof_label}
    state.K.append(new_item)
    state.TS += 1
    label = f"⟨ {e}, {event_pair.proof_label} ⟩"
    step = TraceStep(
        step_index=len(state.trace),
        ts_after=state.TS,
        event_pair={"event_type": e, "data": data, "proof": event_pair.proof_label},
        event_label=label,
        scenario_result=None,
    )
    state.trace.append(step)
    # 物化到 Runtime
    state.materialized.append({"ts": state.TS, "item": new_item})
    return True, ""


def _run_auto_scenario(event_type: str, step: TraceStep) -> dict | None:
    """按事件类型自动推演场景；结果写入 step.scenario_result。前端依赖 result.auto 显示「根因追溯/审计」标签。"""
    from app.scenarios import scenario_root_cause, scenario_audit_trail
    if event_type == "FailEvt":
        try:
            r = scenario_root_cause()
            if isinstance(r, dict) and "auto" not in r:
                r = {**r, "auto": "root_cause"}
            return r
        except Exception as e:
            return {"auto": "root_cause", "ok": False, "message": "根因追溯未就绪或未配置 kos-core"}
    if event_type in ("ProcStep", "Anomaly", "FailEvt"):
        try:
            r = scenario_audit_trail()
            if isinstance(r, dict) and "auto" not in r:
                r = {**r, "auto": "audit"}
            return r
        except Exception:
            return {"auto": "audit", "ok": False, "message": "审计轨迹未就绪"}
    return None


def ingest_one() -> tuple[bool, str, dict | None]:
    """
    从队列 P 取一条信号，精化后执行小步；并按事件类型自动推演场景。
    返回 (成功?, 错误信息, 需新类型时的建议 { need_new_type: True, suggestion: "..." } )。
    """
    state = get_state()
    if not state.P:
        return False, "事件队列为空", None
    signal = state.P.pop(0)
    kind = signal.get("kind")
    if kind == "proc_step":
        s = RawSignal(SignalKind.PROC_STEP, batch_id=signal.get("batch_id"), machine=signal.get("machine"),
                      start_time=signal.get("start_time"), end_time=signal.get("end_time"), timestamp=signal.get("timestamp"))
    elif kind == "anomaly":
        s = RawSignal(SignalKind.ANOMALY, machine=signal.get("machine"), param=signal.get("param"),
                      value=signal.get("value"), timestamp=signal.get("timestamp"))
    elif kind == "fail_evt":
        s = RawSignal(SignalKind.FAIL_EVT, batch_id=signal.get("batch_id"), error_code=signal.get("error_code"), timestamp=signal.get("timestamp"))
    elif kind == "sensor":
        ts = signal.get("ts") or signal.get("timestamp") or 0
        s = RawSignal(SignalKind.SENSOR, timestamp=ts if isinstance(ts, (int, float)) else 0, extra={"name": signal.get("name"), "value": signal.get("value"), "unit": signal.get("unit"), "ts": ts})
    elif kind == "qc_failure":
        ts = signal.get("ts") or signal.get("timestamp") or 0
        s = RawSignal(SignalKind.QC_FAILURE, timestamp=ts if isinstance(ts, (int, float)) else 0, extra={"batch": signal.get("batch"), "defect": signal.get("defect"), "severity": signal.get("severity"), "ts": ts})
    else:
        return False, f"未知信号类型: {kind}", {"need_new_type": True, "suggestion": f"-- 可添加类型以支持 kind={kind}"}
    ep = elaborate_signal(s)
    if ep is None:
        return False, "精化失败，可能缺少对应类型", {"need_new_type": True, "suggestion": f"-- 信号 kind={kind}"}
    ok, err = kernel_step(ep)
    if not ok:
        need_new = {"need_new_type": True, "suggestion": err, "message": err}
        if "未知事件类型:" in err:
            import re
            m = re.search(r"未知事件类型:\s*(\w+)", err)
            if m:
                need_new["type_name"] = m.group(1)
        return False, err, need_new
    step = state.trace[-1]
    step.scenario_result = _run_auto_scenario(ep.event_type, step)
    # 根因溯源成功时，仅当 RootCauseReport 在 Γ 中定义时才加入 K（宪法约束）
    if ep.event_type == "FailEvt" and step.scenario_result and step.scenario_result.get("ok") and step.scenario_result.get("provable"):
        if _type_in_gamma("RootCauseReport"):
            root_item = {
                "type": "RootCauseReport",
                "data": {
                    "proof_term": step.scenario_result.get("proof_term"),
                    "summary": step.scenario_result.get("summary"),
                    "step_index": step.step_index,
                    "trigger_event": ep.event_type,
                },
                "proof": "π_root_cause",
            }
            state.K.append(root_item)
            state.materialized.append({"ts": state.TS, "item": root_item})
    return True, "", None


def ingest_signal(signal: dict) -> tuple[bool, str]:
    """单条信号校验后入队并立即消费该条（仅当为有效事件类型时）。"""
    ok, ep, err = validate_and_elaborate(signal)
    if not ok:
        return False, err
    enqueue_signals([signal])  # 已校验，必入队 1 条
    ok, msg, _ = ingest_one()
    return ok, msg
