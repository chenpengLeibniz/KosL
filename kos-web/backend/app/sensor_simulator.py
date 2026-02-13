"""
运行时层：传感器信号模拟生成器。
模拟 IoT 传感器流、质检上报、工单执行等，输出可被 Kernel 精化为事件对的原始信号。
参考 monograph：SensorTimeSeries, Quality_Report, Execution_Log。
"""
import random
import time
from dataclasses import dataclass, field
from enum import Enum
from typing import Literal

# 预定义枚举，便于前端展示
BATCH_IDS = ["B2310", "Batch_202310-01", "B2311"]
MACHINES = ["M_03", "HeatTreatment_03", "M_01"]
ERROR_CODES = ["HARD_ERR", "Hardness_Issue", "DIM_ERR"]
PARAMS = ["Voltage_Drop", "Temperature", "Pressure", "Humidity"]


class SignalKind(str, Enum):
    PROC_STEP = "proc_step"   # 生产步骤：批次在机器上加工
    ANOMALY = "anomaly"       # 设备异常（温控、电压等）
    FAIL_EVT = "fail_evt"     # 质检失效事件
    SENSOR = "sensor"         # 传感器（与 signal_simulate.c 一致：name, value, unit, ts）
    QC_FAILURE = "qc_failure" # 质检失效（与 signal_simulate.c 一致：batch, defect, severity, ts）


@dataclass
class RawSignal:
    """原始信号（运行时层捕获）。"""
    kind: SignalKind
    batch_id: str | None = None
    machine: str | None = None
    param: str | None = None
    value: float | None = None
    timestamp: float = field(default_factory=time.time)
    error_code: str | None = None
    start_time: float | None = None  # 用于 ProcStep 区间
    end_time: float | None = None
    extra: dict = field(default_factory=dict)

    def to_dict(self) -> dict:
        d = {
            "kind": self.kind.value,
            "batch_id": self.batch_id,
            "machine": self.machine,
            "param": self.param,
            "value": self.value,
            "timestamp": self.timestamp,
            "error_code": self.error_code,
            "start_time": self.start_time,
            "end_time": self.end_time,
        }
        if self.extra:
            d.update(self.extra)
        return d


class SensorSimulator:
    """传感器信号模拟器：按策略生成 RawSignal 序列。"""

    def __init__(self, seed: int | None = None):
        self._rng = random.Random(seed)
        self._signals: list[RawSignal] = []
        self._logical_time = 0

    def _next_ts(self) -> float:
        self._logical_time += 1
        return float(self._logical_time)

    def generate_proc_step(self, batch_id: str | None = None, machine: str | None = None) -> RawSignal:
        batch_id = batch_id or self._rng.choice(BATCH_IDS)
        machine = machine or self._rng.choice(MACHINES)
        t = self._next_ts()
        s = RawSignal(
            kind=SignalKind.PROC_STEP,
            batch_id=batch_id,
            machine=machine,
            start_time=t,
            end_time=t + 1.5,
        )
        self._signals.append(s)
        return s

    def generate_anomaly(self, machine: str | None = None, param: str | None = None) -> RawSignal:
        machine = machine or self._rng.choice(MACHINES)
        param = param or self._rng.choice(PARAMS)
        t = self._next_ts()
        value = round(self._rng.uniform(0.7, 1.3), 3)
        s = RawSignal(kind=SignalKind.ANOMALY, machine=machine, param=param, value=value, timestamp=t)
        self._signals.append(s)
        return s

    def generate_fail_evt(self, batch_id: str | None = None, error_code: str | None = None) -> RawSignal:
        batch_id = batch_id or self._rng.choice(BATCH_IDS)
        error_code = error_code or self._rng.choice(ERROR_CODES)
        t = self._next_ts()
        s = RawSignal(kind=SignalKind.FAIL_EVT, batch_id=batch_id, error_code=error_code, timestamp=t)
        self._signals.append(s)
        return s

    def generate_sensor(self, name: str | None = None, value: float | None = None, unit: str = "") -> RawSignal:
        """与 src/utils/signal_simulate.c kos_sim_signal_sensor 输出格式一致。"""
        name = name or self._rng.choice(["Temperature", "Pressure", "Voltage", "Humidity"])
        value = value if value is not None else round(self._rng.uniform(20.0, 80.0), 2)
        unit = unit or ("°C" if "temp" in name.lower() else "V" if "volt" in name.lower() else "")
        t = self._next_ts()
        s = RawSignal(kind=SignalKind.SENSOR, timestamp=t, extra={"name": name, "value": value, "unit": unit, "ts": int(t * 1000)})
        self._signals.append(s)
        return s

    def generate_qc_failure(self, batch_id: str | None = None, defect: str | None = None, severity: int | None = None) -> RawSignal:
        """与 src/utils/signal_simulate.c kos_sim_signal_qc_failure 输出格式一致。"""
        batch_id = batch_id or self._rng.choice(BATCH_IDS)
        defect = defect or self._rng.choice(["Hardness_Issue", "DIM_ERR", "Surface_Defect"])
        severity = severity if severity is not None else self._rng.randint(1, 5)
        t = self._next_ts()
        s = RawSignal(kind=SignalKind.QC_FAILURE, timestamp=t, extra={"batch": batch_id, "defect": defect, "severity": severity, "ts": int(t * 1000)})
        self._signals.append(s)
        return s

    def generate_random(self) -> RawSignal:
        k = self._rng.choice([SignalKind.PROC_STEP, SignalKind.ANOMALY, SignalKind.FAIL_EVT, SignalKind.SENSOR, SignalKind.QC_FAILURE])
        if k == SignalKind.PROC_STEP:
            return self.generate_proc_step()
        if k == SignalKind.ANOMALY:
            return self.generate_anomaly()
        if k == SignalKind.FAIL_EVT:
            return self.generate_fail_evt()
        if k == SignalKind.SENSOR:
            return self.generate_sensor()
        return self.generate_qc_failure()

    def get_latest(self, n: int = 10) -> list[dict]:
        return [s.to_dict() for s in self._signals[-n:]]

    def get_all(self) -> list[dict]:
        return [s.to_dict() for s in self._signals]

    def clear(self) -> None:
        self._signals.clear()
        self._logical_time = 0


# 单例，供 API 使用
_simulator: SensorSimulator | None = None


def get_simulator() -> SensorSimulator:
    global _simulator
    if _simulator is None:
        _simulator = SensorSimulator()
    return _simulator
