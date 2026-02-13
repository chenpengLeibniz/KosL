"""
KOS-TL 应用展示系统 - 后端 API。
(1) Core：仅通过 Core 加载 注入类型与谓词；可选 kos-core 校验；加载后同步到 Kernel Γ
(2) Runtime：信号模拟（与 signal_simulate.c 对齐）、生效入队、实时录制、日志保存与重放
(3) Kernel：Γ、σ、事件队列 P；小步演化；按事件类型自动推演场景；物化
"""
import time
import uuid
from fastapi import FastAPI, HTTPException, UploadFile
from fastapi.middleware.cors import CORSMiddleware
from pydantic import BaseModel

from app.core_loader import (
    get_types,
    get_predicates,
    get_constructors,
    load_from_kos,
    load_from_kos_file_content,
    clear_dynamic,
)
from app.sensor_simulator import get_simulator, SignalKind
from app.kernel_engine import (
    get_state,
    reset_state,
    ingest_signal,
    ingest_one,
    enqueue_signals,
    sync_gamma_from_core,
    get_valid_event_types,
    validate_and_elaborate,
)
from app.scenarios import (
    scenario_root_cause,
    scenario_counterfactual,
    scenario_audit_trail,
    scenario_evolve_idle,
    scenario_verify_ai,
    scenario_compliance_check,
)

app = FastAPI(
    title="KOS-TL 应用展示 API",
    description="Core 类型/谓词（仅加载）、Runtime 信号模拟与生效、Kernel Γ/σ/队列与轨迹",
)
app.add_middleware(
    CORSMiddleware,
    allow_origins=["*"],
    allow_credentials=True,
    allow_methods=["*"],
    allow_headers=["*"],
)

# 运行时日志：保存的信号列表，用于重放；每项含 id, name, signals, created_at
_runtime_logs: list[dict] = []
# 实时录制：生效时追加到此缓冲，停止录制时保存为一条日志
_recording: bool = False
_recording_buffer: list[dict] = []


# ---------- Core（仅来自加载，加载后同步 Γ 到 Kernel）----------
@app.get("/api/core/types")
def api_core_types():
    return {"types": get_types()}


@app.get("/api/core/predicates")
def api_core_predicates():
    return {"predicates": get_predicates()}


@app.get("/api/core/constructors")
def api_core_constructors():
    return {"constructors": get_constructors()}


class LoadKosBody(BaseModel):
    kos_source: str


@app.post("/api/core/load")
def api_core_load(body: LoadKosBody):
    """从 KOS 格式文本加载类型或谓词到 Core 层；并同步到 Kernel 的 Γ。"""
    result = load_from_kos(body.kos_source)
    if not result.get("ok"):
        raise HTTPException(status_code=400, detail=result.get("error", "解析失败"))
    sync_gamma_from_core()
    return result


@app.post("/api/core/load_file")
async def api_core_load_file(file: UploadFile | None = None):
    """导入 .kos 文件：上传后解析并加载到 Core，同步到 Kernel Γ。表单字段名须为 file。"""
    if file is None or file.filename is None or file.filename.strip() == "":
        raise HTTPException(status_code=400, detail="请选择要上传的 .kos 文件")
    fn = file.filename.lower().strip()
    if not (fn.endswith(".kos") or fn.endswith(".kos.txt")):
        raise HTTPException(status_code=400, detail="请选择扩展名为 .kos 的文件")
    try:
        raw = await file.read()
        content = raw.decode("utf-8", errors="replace")
    except Exception as e:
        raise HTTPException(status_code=400, detail=f"读取文件失败: {e}")
    if not content.strip():
        raise HTTPException(status_code=400, detail="文件内容为空")
    result = load_from_kos_file_content(content, file.filename)
    if not result.get("ok"):
        raise HTTPException(status_code=400, detail=result.get("error", "解析失败"))
    sync_gamma_from_core()
    return result


@app.post("/api/core/clear_dynamic")
def api_core_clear_dynamic():
    clear_dynamic()
    return {"ok": True}


class AddTypeBody(BaseModel):
    name: str
    definition: str | None = None


@app.post("/api/core/add_type")
def api_core_add_type(body: AddTypeBody):
    """通过 Core 层创建新类型（如 Kernel 遇到未知事件类型时由用户确认后调用）。"""
    from app.core_loader import load_from_kos
    name = (body.name or "").strip()
    if not name:
        raise HTTPException(status_code=400, detail="类型名不能为空")
    definition = body.definition or f"def {name} : Type1 := Sigma(x : U0). Prop P."
    result = load_from_kos(definition.strip())
    if not result.get("ok"):
        raise HTTPException(status_code=400, detail=result.get("error", "解析失败"))
    sync_gamma_from_core()
    return {"ok": True, "added": result.get("added", {}), "type_name": name}


# ---------- Runtime：信号模拟（与 signal_simulate.c 对齐）、生效、日志 ----------
# Core 事件类型 → Runtime 可生成的 signal kind（与 kernel_engine.VALID_EVENT_TYPES 对应）
_EVENT_TYPE_TO_KIND = {
    "FailEvt": "fail_evt",
    "ProcStep": "proc_step",
    "Anomaly": "anomaly",
    "Sensor": "sensor",
    "QcFailure": "qc_failure",
}


def _get_creatable_types() -> list[dict]:
    """返回当前可创建的事件类型列表：来自 Core 的 mk_XXX 构造函数，映射到 Runtime 的 signal kind。"""
    from app.core_loader import get_constructors
    from app.kernel_engine import get_valid_event_types

    constructors = get_constructors()
    valid = set(get_valid_event_types())
    out: list[dict] = []
    for c in constructors:
        name = c.get("name") or c.get("id") or ""
        target = (c.get("target") or "").strip()
        if not target or not name.startswith("mk"):
            continue
        if target not in valid:
            continue
        kind = _EVENT_TYPE_TO_KIND.get(target)
        if not kind:
            continue
        out.append({
            "constructor": name,
            "target_type": target,
            "signal_kind": kind,
            "label": name,
        })
    if not out:
        for ev in get_valid_event_types():
            kind = _EVENT_TYPE_TO_KIND.get(ev)
            if kind:
                out.append({
                    "constructor": f"mk{ev}",
                    "target_type": ev,
                    "signal_kind": kind,
                    "label": ev,
                })
    return out


@app.get("/api/runtime/creatable_types")
def api_runtime_creatable_types():
    """返回与 Core 层类型对应的可创建类型列表（mk_XXX → signal_kind），供 Runtime 模拟器展开按钮。"""
    return {"creatable_types": _get_creatable_types()}


@app.get("/api/runtime/signals/latest")
def api_signals_latest(n: int = 10):
    return {"signals": get_simulator().get_latest(n)}


@app.post("/api/runtime/signals/generate")
def api_signals_generate(kind: str | None = None):
    """生成一条信号。kind: proc_step | anomaly | fail_evt | sensor | qc_failure | random"""
    sim = get_simulator()
    if kind == "proc_step":
        s = sim.generate_proc_step()
    elif kind == "anomaly":
        s = sim.generate_anomaly()
    elif kind == "fail_evt":
        s = sim.generate_fail_evt()
    elif kind == "sensor":
        s = sim.generate_sensor()
    elif kind == "qc_failure":
        s = sim.generate_qc_failure()
    else:
        s = sim.generate_random()
    return {"signal": s.to_dict()}


class GenerateBatchBody(BaseModel):
    count: int = 5
    kinds: list[str] | None = None


@app.post("/api/runtime/signals/generate_batch")
def api_signals_generate_batch(body: GenerateBatchBody):
    sim = get_simulator()
    kinds = body.kinds or ["proc_step", "anomaly", "fail_evt", "sensor", "qc_failure"]
    out = []
    for i in range(body.count):
        k = kinds[i % len(kinds)]
        if k == "proc_step":
            s = sim.generate_proc_step()
        elif k == "anomaly":
            s = sim.generate_anomaly()
        elif k == "fail_evt":
            s = sim.generate_fail_evt()
        elif k == "sensor":
            s = sim.generate_sensor()
        elif k == "qc_failure":
            s = sim.generate_qc_failure()
        else:
            s = sim.generate_random()
        out.append(s.to_dict())
    return {"signals": out}


@app.delete("/api/runtime/signals")
def api_signals_clear():
    get_simulator().clear()
    return {"ok": True}


def _event_pair_to_signal(event_type: str, data: dict) -> dict:
    """将轨迹中的事件对还原为可编辑、可再生效的信号 dict（供「从历史生成」使用）。"""
    d = data or {}
    if event_type == "ProcStep":
        dur = d.get("dur") or [0, 0]
        return {
            "kind": "proc_step",
            "batch_id": d.get("b"),
            "machine": d.get("m"),
            "start_time": dur[0] if isinstance(dur, (list, tuple)) and len(dur) > 0 else d.get("start_time"),
            "end_time": dur[1] if isinstance(dur, (list, tuple)) and len(dur) > 1 else d.get("end_time"),
            "timestamp": d.get("t", d.get("timestamp", 0)),
        }
    if event_type == "Anomaly":
        return {
            "kind": "anomaly",
            "machine": d.get("m"),
            "param": d.get("p"),
            "value": d.get("v"),
            "timestamp": d.get("t", d.get("timestamp", 0)),
        }
    if event_type == "FailEvt":
        return {
            "kind": "fail_evt",
            "batch_id": d.get("b"),
            "error_code": d.get("err"),
            "timestamp": d.get("t", d.get("timestamp", 0)),
        }
    if event_type == "Sensor":
        ts = d.get("ts") or d.get("timestamp") or 0
        return {
            "kind": "sensor",
            "name": d.get("name"),
            "value": d.get("value"),
            "unit": d.get("unit", ""),
            "ts": ts,
            "timestamp": ts,
        }
    if event_type == "QcFailure":
        ts = d.get("ts") or d.get("timestamp") or 0
        return {
            "kind": "qc_failure",
            "batch": d.get("batch"),
            "defect": d.get("defect"),
            "severity": d.get("severity"),
            "ts": ts,
            "timestamp": ts,
        }
    return {"kind": "unknown", "event_type": event_type, **d}


class SuggestFromTraceBody(BaseModel):
    limit: int = 10  # 取最近 N 条轨迹步


@app.post("/api/runtime/signals/suggest_from_trace")
def api_signals_suggest_from_trace(body: SuggestFromTraceBody):
    """根据 Kernel 轨迹历史生成可编辑的有效事件示例（供用户修改后生效）。"""
    state = get_state()
    trace = getattr(state, "trace", []) or []
    n = max(1, min(body.limit, 50))
    steps = trace[-n:]
    suggested = []
    for s in steps:
        ep = getattr(s, "event_pair", None) if hasattr(s, "event_pair") else (s.get("event_pair") if isinstance(s, dict) else None)
        if not ep:
            continue
        if isinstance(ep, dict):
            event_type = ep.get("event_type")
            data = ep.get("data") or {}
        else:
            event_type = getattr(ep, "event_type", None)
            data = getattr(ep, "data", None) or {}
        if event_type:
            suggested.append(_event_pair_to_signal(event_type, data))
    return {"signals": suggested}


class ApplyBody(BaseModel):
    signals: list[dict]


@app.post("/api/runtime/apply")
def api_runtime_apply(body: ApplyBody):
    """仅将能精化为有效事件类型的信号加入 Kernel 事件队列 P；不符合要求的拒绝并返回原因。若正在录制，仅追加已入队的信号。"""
    global _recording_buffer
    n, rejected = enqueue_signals(body.signals)
    if _recording and n > 0:
        accepted = [s for s in body.signals if not any(r["signal"] == s for r in rejected)]
        _recording_buffer.extend(accepted)
    return {
        "ok": True,
        "enqueued": n,
        "rejected": rejected,
        "rejected_count": len(rejected),
        "state": get_state().to_dict(),
    }


class LogSaveBody(BaseModel):
    signals: list[dict]
    name: str | None = None


@app.post("/api/runtime/log/save")
def api_runtime_log_save(body: LogSaveBody):
    """保存当前信号列表为日志，供重放。"""
    log_id = str(uuid.uuid4())[:8]
    _runtime_logs.append({
        "id": log_id,
        "name": body.name or f"log_{log_id}",
        "signals": body.signals,
        "created_at": time.time(),
    })
    return {"ok": True, "id": log_id, "count": len(body.signals)}


@app.get("/api/runtime/log/list")
def api_runtime_log_list():
    """返回所有已保存日志（含创建时间），按时间倒序。"""
    out = []
    for x in _runtime_logs:
        out.append({
            "id": x["id"],
            "name": x.get("name", x["id"]),
            "count": len(x.get("signals", [])),
            "created_at": x.get("created_at"),
        })
    out.sort(key=lambda e: (e.get("created_at") or 0), reverse=True)
    return {"logs": out}


@app.get("/api/runtime/log/{log_id}/signals")
def api_runtime_log_signals(log_id: str):
    """返回指定日志中的信号列表（用于「从历史加载」后编辑再生效）。"""
    found = [x for x in _runtime_logs if x["id"] == log_id]
    if not found:
        raise HTTPException(status_code=404, detail="日志不存在")
    return {"signals": found[0].get("signals", [])}


# ---------- 实时录制：生效时自动记录，停止时保存为日志 ----------
@app.get("/api/runtime/log/recording/status")
def api_runtime_log_recording_status():
    """当前是否在录制及已录制条数。"""
    return {"recording": _recording, "count": len(_recording_buffer)}


@app.post("/api/runtime/log/recording/start")
def api_runtime_log_recording_start():
    """开始实时录制：之后每次「生效」的信号会追加到录制缓冲。"""
    global _recording, _recording_buffer
    _recording = True
    _recording_buffer = []
    return {"ok": True, "recording": True, "count": 0}


@app.post("/api/runtime/log/recording/stop")
def api_runtime_log_recording_stop(name: str | None = None):
    """停止录制并将缓冲保存为一条日志；返回 log id。"""
    global _recording, _recording_buffer, _runtime_logs
    _recording = False
    log_id = str(uuid.uuid4())[:8]
    log_name = (name or "").strip() or f"rec_{log_id}"
    _runtime_logs.append({
        "id": log_id,
        "name": log_name,
        "signals": list(_recording_buffer),
        "created_at": time.time(),
    })
    count = len(_recording_buffer)
    _recording_buffer = []
    return {"ok": True, "recording": False, "id": log_id, "name": log_name, "count": count}


class AutoEvolveBody(BaseModel):
    sequence_length: int = 10
    max_steps: int = 500


@app.post("/api/runtime/auto_evolve")
def api_runtime_auto_evolve(body: AutoEvolveBody):
    """
    自动演化：按当前情况生成指定长度的信号序列，能精化为事件类型的入队，再自动执行小步直至队列空或达到 max_steps。
    返回生成数、入队数、执行步数及最终状态，便于回放与合规审计。
    """
    from app.kernel_engine import ingest_one
    sim = get_simulator()
    n = max(1, min(body.sequence_length, 200))
    kinds = ["proc_step", "anomaly", "fail_evt", "sensor", "qc_failure"]
    generated: list[dict] = []
    for i in range(n):
        k = kinds[i % len(kinds)]
        if k == "proc_step":
            s = sim.generate_proc_step()
        elif k == "anomaly":
            s = sim.generate_anomaly()
        elif k == "fail_evt":
            s = sim.generate_fail_evt()
        elif k == "sensor":
            s = sim.generate_sensor()
        elif k == "qc_failure":
            s = sim.generate_qc_failure()
        else:
            s = sim.generate_random()
        generated.append(s.to_dict())
    enqueued, rejected = enqueue_signals(generated)
    steps_done = 0
    need_new_type = None
    max_steps = max(0, min(body.max_steps, 1000))
    while steps_done < max_steps:
        ok, msg, need_new = ingest_one()
        if not ok:
            if "队列为空" in msg or "queue" in msg.lower():
                break
            need_new_type = need_new
            break
        steps_done += 1
        if len(get_state().P) <= 0:
            break
    return {
        "ok": True,
        "generated": len(generated),
        "enqueued": enqueued,
        "rejected_count": len(rejected),
        "steps_done": steps_done,
        "need_new_type": need_new_type,
        "state": get_state().to_dict(),
        "generated_signals": generated,
    }


class LogReplayBody(BaseModel):
    id: str | None = None  # 若指定则重放该日志
    signals: list[dict] | None = None  # 或直接传信号列表
    replay_and_evolve: bool = False  # 若 True，入队后自动执行小步直至队列空（最多 500 步）


@app.post("/api/runtime/log/replay")
def api_runtime_log_replay(body: LogReplayBody):
    """重放日志：仅将能精化为有效事件类型的信号入队到 Kernel P；可选入队后自动演化。"""
    from app.kernel_engine import ingest_one
    if body.signals:
        signals = body.signals
    elif body.id:
        found = [x for x in _runtime_logs if x["id"] == body.id]
        if not found:
            raise HTTPException(status_code=404, detail="日志不存在")
        signals = found[0].get("signals", [])
    else:
        raise HTTPException(status_code=400, detail="请指定 id 或 signals")
    n, rejected = enqueue_signals(signals)
    steps_done = 0
    if body.replay_and_evolve and n > 0:
        for _ in range(500):
            ok, msg, _ = ingest_one()
            if not ok:
                break
            steps_done += 1
            if len(get_state().P) <= 0:
                break
    return {
        "ok": True,
        "enqueued": n,
        "rejected": rejected,
        "rejected_count": len(rejected),
        "steps_done": steps_done,
        "state": get_state().to_dict(),
    }


# ---------- Kernel：Γ、σ、队列 P、小步、自动场景 ----------
@app.get("/api/kernel/valid_event_types")
def api_kernel_valid_event_types():
    """返回当前可入队的事件类型列表（Runtime 模拟器仅此类可生效）。"""
    return {"event_types": get_valid_event_types()}


@app.get("/api/kernel/state")
def api_kernel_state():
    return get_state().to_dict()


@app.post("/api/kernel/reset")
def api_kernel_reset():
    return reset_state().to_dict()


class IngestBody(BaseModel):
    signal: dict


@app.post("/api/kernel/ingest")
def api_kernel_ingest(body: IngestBody):
    """单条信号入队并立即执行一步（兼容旧 API）。"""
    ok, msg = ingest_signal(body.signal)
    if not ok:
        raise HTTPException(status_code=400, detail=msg)
    return get_state().to_dict()


@app.post("/api/kernel/step")
def api_kernel_step():
    """从事件队列 P 取一条执行小步；按事件类型自动推演根因/审计等。未知类型时返回 need_new_type 供前端弹窗。"""
    from fastapi.responses import JSONResponse
    ok, msg, need_new = ingest_one()
    if not ok:
        return JSONResponse(
            status_code=400,
            content={"detail": msg, "need_new_type": need_new},
        )
    return {"ok": True, "state": get_state().to_dict(), "need_new_type": need_new}


@app.post("/api/kernel/step_drain")
def api_kernel_step_drain(max_steps: int = 500):
    """连续执行小步直到事件队列 P 为空或达到 max_steps，返回执行步数与最终状态。"""
    from app.kernel_engine import ingest_one
    steps_done = 0
    last_need_new = None
    while steps_done < max_steps:
        ok, msg, need_new = ingest_one()
        if not ok:
            # 队列已空时 ingest_one 返回失败，视为正常结束
            if "队列为空" in msg or "queue" in msg.lower():
                return {
                    "ok": True,
                    "steps_done": steps_done,
                    "state": get_state().to_dict(),
                    "stopped_reason": "queue_empty",
                    "need_new_type": last_need_new,
                }
            return {
                "ok": True,
                "steps_done": steps_done,
                "state": get_state().to_dict(),
                "stopped_reason": "error",
                "error": msg,
                "need_new_type": need_new,
            }
        steps_done += 1
        if need_new:
            last_need_new = need_new
        if len(get_state().P) <= 0:
            break
    return {
        "ok": True,
        "steps_done": steps_done,
        "state": get_state().to_dict(),
        "stopped_reason": "queue_empty" if len(get_state().P) <= 0 else "max_steps",
        "need_new_type": last_need_new,
    }


@app.get("/api/kernel/trace")
def api_kernel_trace():
    return {"trace": get_state().to_dict()["trace"]}


# ---------- 知识图谱 ----------
@app.get("/api/graph")
def api_graph(step: int | None = None):
    from app.graph_data import get_graph_data
    return get_graph_data(step)


@app.get("/api/graph/trajectory_meta")
def api_trajectory_meta():
    state = get_state()
    steps = []
    for i, s in enumerate(state.trace):
        steps.append({
            "step_index": i,
            "ts_after": s.ts_after,
            "event_label": s.event_label,
            "scenario_result": s.scenario_result,
        })
    return {"max_step": max(len(steps) - 1, 0), "steps": steps}


# ---------- Scenarios ----------
@app.get("/api/scenarios/root_cause")
def api_scenarios_root_cause():
    return scenario_root_cause()


@app.get("/api/scenarios/counterfactual")
def api_scenarios_counterfactual():
    return scenario_counterfactual()


@app.get("/api/scenarios/counterfactual/env")
def api_scenarios_counterfactual_env():
    """反事实推理应用环境描述：两个上下文、证明目标、以及 Runtime 层建议构建的信号。"""
    return {
        "goal": "RootCauseReport",
        "factual": {
            "name": "quality_traceability_prove",
            "description_zh": "事实世界：含异常 anomalyEx（M_03 炉 08:15 电压波动），可构造根因报告。",
            "description_en": "Factual world: includes anomalyEx (M_03 furnace 08:15 voltage drop); RootCauseReport provable.",
            "signals": ["proc_step", "anomaly", "fail_evt"],
            "signal_hint_zh": "在 Runtime 生成并生效：生产步骤 → 异常 → 失效事件（按时间顺序）。",
            "signal_hint_en": "In Runtime generate and apply: proc_step → anomaly → fail_evt (in time order).",
        },
        "counterfactual": {
            "name": "quality_traceability_prove_no_anomaly",
            "description_zh": "反事实世界：排除 anomalyEx（假设该异常未发生），无法构造根因报告。",
            "description_en": "Counterfactual world: anomalyEx excluded; RootCauseReport not provable.",
            "signals": ["proc_step", "fail_evt"],
            "signal_hint_zh": "在 Runtime 仅生成：生产步骤 → 失效事件（无异常）。",
            "signal_hint_en": "In Runtime generate only: proc_step → fail_evt (no anomaly).",
        },
        "conclusion_zh": "若事实可证、反事实不可证，则 anomalyEx 对根因因果必要。",
        "conclusion_en": "If factual proves and counterfactual does not, anomalyEx is causally necessary for root cause.",
    }


@app.get("/api/scenarios/audit/env")
def api_scenarios_audit_env():
    """审计与问责应用环境描述。"""
    return {
        "description_zh": "导出当前 Kernel 轨迹为审计 JSON，包含每步的 step_index、ts_after、event_label、event_pair，用于追溯与问责。",
        "description_en": "Export current Kernel trace as audit JSON (step_index, ts_after, event_label, event_pair) for traceability and accountability.",
        "signal_hint_zh": "先在 Runtime 生成并生效若干信号（如 proc_step、anomaly、fail_evt），再在 Kernel 执行若干步形成轨迹，最后在此执行审计导出。",
        "signal_hint_en": "Generate and apply signals in Runtime, run steps in Kernel to build trace, then run audit here to export.",
    }


@app.get("/api/scenarios/audit")
def api_scenarios_audit():
    return scenario_audit_trail()


@app.get("/api/scenarios/evolve_idle/env")
def api_scenarios_evolve_idle_env():
    """复杂系统治理（演化直至空闲）应用环境描述。"""
    return {
        "description_zh": "从 Runtime 模拟器取最近最多 50 条信号，逐条摄入 Kernel 直至队列空闲，统计步数与消费数，用于复杂系统治理演示。",
        "description_en": "Take up to 50 latest signals from Runtime simulator, ingest each into Kernel until idle; report steps and consumed count for governance demo.",
        "signal_hint_zh": "先在 Runtime 生成或从历史/日志加载多类信号（proc_step、anomaly、fail_evt 等），再在此执行「演化直至空闲」。",
        "signal_hint_en": "Generate or load signals in Runtime (proc_step, anomaly, fail_evt, etc.), then run evolve-idle here.",
    }


@app.post("/api/scenarios/evolve_idle")
def api_scenarios_evolve_idle():
    return scenario_evolve_idle()


@app.get("/api/scenarios/verify_ai/env")
def api_scenarios_verify_ai_env():
    """AI 治理（类型校验）应用环境描述。"""
    return {
        "description_zh": "校验给定项（term）是否满足给定类型（type），调用 kos-core check-term，用于 AI 建议项的类型合规检查。",
        "description_en": "Check whether a term satisfies a type via kos-core check-term; used for AI-suggestion type compliance.",
        "default_term": "Prop P",
        "default_type": "Prop P",
        "signal_hint_zh": "不依赖 Runtime 信号；输入为 term 与 type 表达式字符串。",
        "signal_hint_en": "No Runtime signals; input is term and type expression strings.",
    }


class VerifyAiBody(BaseModel):
    term_expr: str = "Prop P"
    type_expr: str = "Prop P"


@app.post("/api/scenarios/verify_ai")
def api_scenarios_verify_ai(body: VerifyAiBody):
    return scenario_verify_ai(body.term_expr, body.type_expr)


@app.get("/api/scenarios/compliance/env")
def api_scenarios_compliance_env():
    """合规性决策应用环境描述：允许的信号类型、在 Runtime 如何构建合规/不合规信号。"""
    return {
        "description_zh": "在信号入队前检查是否合规（当前规则：仅 proc_step、anomaly、fail_evt 为合规类型）。合规则允许入队，不合规则拒绝并返回原因。",
        "description_en": "Check signal compliance before enqueue. Current rule: only proc_step, anomaly, fail_evt are compliant. Compliant signals may enqueue; others are rejected with reason.",
        "valid_kinds": ["proc_step", "anomaly", "fail_evt"],
        "invalid_kinds_example": ["sensor", "qc_failure"],
        "compliant_hint_zh": "在 Runtime 生成 proc_step、anomaly 或 fail_evt 任一条，在此或场景页执行「检查合规性」应得到合规。",
        "compliant_hint_en": "In Runtime generate one of proc_step, anomaly, or fail_evt; run compliance check here or on Scenarios to get compliant.",
        "non_compliant_hint_zh": "使用 sensor、qc_failure 或自定义 kind 的信号会得到「未知信号类型」不合规。",
        "non_compliant_hint_en": "Signals with kind sensor, qc_failure, or custom kind will be rejected as non-compliant.",
    }


class ComplianceCheckBody(BaseModel):
    signal: dict


@app.post("/api/scenarios/compliance_check")
def api_scenarios_compliance(body: ComplianceCheckBody):
    return scenario_compliance_check(body.signal)
