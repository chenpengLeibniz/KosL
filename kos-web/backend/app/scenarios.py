"""
KOS-TL 六大核心场景引擎。
通过 subprocess 调用 kos-core 实现：根因追溯、反事实推理、AI 建议校验等。
当 kos-core 不可用时返回模拟结果，供前端演示。

Runtime 层信号与场景对应（详见 docs/SCENARIOS_RUNTIME_SIGNALS.md）：
  1 根因追溯   → fail_evt（必）；完整链：proc_step → anomaly → fail_evt
  2 反事实推理 → 不直接消费信号（或两条轨迹：含/不含 anomaly）
  3 合规性决策 → 单条 proc_step / anomaly / fail_evt
  4 审计与问责 → 任意 proc_step, anomaly, fail_evt, sensor, qc_failure 若干条，先入队执行步
  5 演化直至空闲 → 上述五类混合多条，由场景消费模拟器 buffer
  6 AI 类型校验 → 不需要 Runtime 信号（仅 term/type 表达式）
"""
import json
import os
import shutil
import subprocess
from pathlib import Path
from typing import Any

_DATA_DIR = Path(__file__).resolve().parent / "data" / "scenarios"
_CTX_PROVE = str(_DATA_DIR / "quality_traceability_prove.kos")
_CTX_NO_ANOMALY = str(_DATA_DIR / "quality_traceability_prove_no_anomaly.kos")

def _find_kos_core() -> str | None:
    """查找 kos-core 可执行文件。需设置环境变量 KOS_CORE_PATH 指向可执行文件。"""
    path = os.environ.get("KOS_CORE_PATH")
    if not path:
        return None
    if shutil.which(path):
        return path
    if Path(path).is_file():
        return path
    return None


def _run_kos_prove(ctx_file: str, goal: str) -> tuple[bool, str]:
    """执行 kos-core prove --ctx <file> <goal>，返回 (成功?, 输出/错误)。"""
    kc = _find_kos_core()
    if kc is None:
        return False, "kos-core not found (set KOS_CORE_PATH)"
    ctx_abs = str(Path(ctx_file).resolve())
    try:
        proc = subprocess.run(
            [kc, "prove", "--ctx", ctx_abs, goal],
            capture_output=True,
            text=True,
            timeout=10,
        )
        if proc.returncode == 0:
            return True, (proc.stdout or "").strip()
        return False, (proc.stderr or proc.stdout or "prove failed").strip()
    except subprocess.TimeoutExpired:
        return False, "timeout"
    except Exception as e:
        return False, str(e)


def _run_kos_check_term(term_expr: str, type_expr: str) -> tuple[bool, str]:
    """执行 kos-core check-term <term> <type>。"""
    kc = _find_kos_core()
    if kc is None:
        return False, "kos-core not found"
    try:
        proc = subprocess.run(
            [kc, "check-term", term_expr, type_expr],
            capture_output=True,
            text=True,
            timeout=5,
        )
        if proc.returncode == 0:
            return True, (proc.stdout or "").strip()
        return False, (proc.stderr or proc.stdout or "check-term failed").strip()
    except Exception as e:
        return False, str(e)


def scenario_root_cause() -> dict[str, Any]:
    """场景 1：根因追溯。prove RootCauseReport。"""
    ok, out = _run_kos_prove(_CTX_PROVE, "RootCauseReport")
    if ok:
        return {"ok": True, "provable": True, "proof_term": out, "summary": "根因已定位：M_03 炉 08:15 电压波动 → 硬度异常"}
    # 模拟：当 kos-core 不可用时的演示结果
    if "not found" in out.lower():
        return {
            "ok": True,
            "provable": True,
            "proof_term": "mkRootCauseReport failEvtUnderInvestigation anomalyEx (mkCausalProof ...)",
            "summary": "[模拟] 根因已定位：M_03 炉 08:15 电压波动 → 硬度异常",
            "simulated": True,
        }
    return {"ok": False, "provable": False, "error": out}


def scenario_counterfactual() -> dict[str, Any]:
    """场景 2：反事实推理。事实 ctx 可证 RootCauseReport，反事实 ctx（无 anomalyEx）不可证。"""
    factual_ok, factual_out = _run_kos_prove(_CTX_PROVE, "RootCauseReport")
    counter_ok, counter_out = _run_kos_prove(_CTX_NO_ANOMALY, "RootCauseReport")
    kos_available = _find_kos_core() is not None
    if kos_available:
        return {
            "ok": True,
            "factual_provable": factual_ok,
            "counterfactual_provable": counter_ok,
            "excluded_necessary": factual_ok and not counter_ok,
            "summary": "anomalyEx 对根因必要" if (factual_ok and not counter_ok) else "结论需结合两次证明结果",
            "factual_output": factual_out,
            "counterfactual_output": counter_out,
        }
    return {
        "ok": True,
        "factual_provable": True,
        "counterfactual_provable": False,
        "excluded_necessary": True,
        "summary": "[模拟] 事实可证、反事实不可证 → anomalyEx 对根因因果必要",
        "simulated": True,
    }


def scenario_audit_trail() -> dict[str, Any]:
    """场景 4：审计与问责。导出当前 Kernel 轨迹为审计 JSON。"""
    from app.kernel_engine import get_state

    state = get_state()
    audit = {
        "audit_type": "trajectory",
        "trace_length": len(state.trace),
        "trace": [
            {
                "step_index": s.step_index,
                "ts_after": s.ts_after,
                "event_label": s.event_label,
                "event_pair": s.event_pair,
            }
            for s in state.trace
        ],
    }
    return {"ok": True, "audit": audit}


def scenario_evolve_idle() -> dict[str, Any]:
    """场景 5：复杂系统治理。演化直至空闲（摄入所有待处理信号）。"""
    from app.kernel_engine import get_state, ingest_signal
    from app.sensor_simulator import get_simulator

    sim = get_simulator()
    signals = sim.get_latest(50)
    steps = 0
    consumed = []
    for s in signals:
        ok, _ = ingest_signal(s.to_dict())
        if ok:
            steps += 1
            consumed.append(s.to_dict())
    sim.clear()  # 清空已摄入
    return {
        "ok": True,
        "steps": steps,
        "consumed_count": len(consumed),
        "state": get_state().to_dict(),
    }


def scenario_verify_ai(term_expr: str, type_expr: str) -> dict[str, Any]:
    """场景 6：AI 治理。校验 AI 建议项是否满足给定类型。"""
    ok, out = _run_kos_check_term(term_expr, type_expr)
    if ok:
        return {"ok": True, "compliant": True, "output": out}
    if "not found" in (out or "").lower():
        # 模拟：Prop P : Prop P 恒成立
        if "Prop P" in term_expr and "Prop P" in type_expr:
            return {"ok": True, "compliant": True, "output": "[模拟] 类型检查通过", "simulated": True}
    return {"ok": True, "compliant": False, "error": out}


def scenario_compliance_check(signal: dict) -> dict[str, Any]:
    """场景 3：合规性决策。在摄入前检查信号是否合规（演示简化：仅检查类型存在）。"""
    kind = signal.get("kind")
    valid = kind in ("proc_step", "anomaly", "fail_evt")
    return {"ok": True, "compliant": valid, "reason": "类型合法" if valid else "未知信号类型"}
