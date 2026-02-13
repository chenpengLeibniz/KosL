const API = '/api';

export async function getCoreTypes() {
  const r = await fetch(`${API}/core/types`);
  if (!r.ok) throw new Error(r.statusText);
  return r.json();
}

export async function getCorePredicates() {
  const r = await fetch(`${API}/core/predicates`);
  if (!r.ok) throw new Error(r.statusText);
  return r.json();
}

export async function getCoreConstructors() {
  const r = await fetch(`${API}/core/constructors`);
  if (!r.ok) throw new Error(r.statusText);
  return r.json();
}

export async function loadKos(kosSource) {
  const r = await fetch(`${API}/core/load`, {
    method: 'POST',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify({ kos_source: kosSource }),
  });
  const j = await r.json().catch(() => ({}));
  if (!r.ok) throw new Error(j.detail || r.statusText);
  return j;
}

export async function loadKosFile(file) {
  if (!file || !(file instanceof File)) throw new Error('请选择 .kos 文件');
  const form = new FormData();
  form.append('file', file);
  const r = await fetch(`${API}/core/load_file`, { method: 'POST', body: form });
  const j = await r.json().catch(() => ({}));
  if (!r.ok) {
    const msg = Array.isArray(j.detail) ? j.detail.map(d => d.msg || d).join('; ') : (j.detail || r.statusText);
    throw new Error(msg);
  }
  return j;
}

export async function clearCoreDynamic() {
  const r = await fetch(`${API}/core/clear_dynamic`, { method: 'POST' });
  if (!r.ok) throw new Error(r.statusText);
  return r.json();
}

export async function coreAddType(name, definition = null) {
  const r = await fetch(`${API}/core/add_type`, {
    method: 'POST',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify({ name, definition }),
  });
  const j = await r.json().catch(() => ({}));
  if (!r.ok) throw new Error(j.detail || r.statusText);
  return j;
}

export async function getValidEventTypes() {
  const r = await fetch(`${API}/kernel/valid_event_types`);
  if (!r.ok) throw new Error(r.statusText);
  return r.json();
}

export async function getCreatableTypes() {
  const r = await fetch(`${API}/runtime/creatable_types`);
  if (!r.ok) throw new Error(r.statusText);
  return r.json();
}

export async function suggestSignalsFromTrace(limit = 10) {
  const r = await fetch(`${API}/runtime/signals/suggest_from_trace`, {
    method: 'POST',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify({ limit }),
  });
  if (!r.ok) throw new Error(r.statusText);
  return r.json();
}

export async function getLogSignals(logId) {
  const r = await fetch(`${API}/runtime/log/${encodeURIComponent(logId)}/signals`);
  if (!r.ok) throw new Error(r.statusText);
  return r.json();
}

export async function getSignalsLatest(n = 10) {
  const r = await fetch(`${API}/runtime/signals/latest?n=${n}`);
  if (!r.ok) throw new Error(r.statusText);
  return r.json();
}

export async function generateSignal(kind) {
  const r = await fetch(`${API}/runtime/signals/generate?kind=${kind || 'random'}`, { method: 'POST' });
  if (!r.ok) throw new Error(r.statusText);
  return r.json();
}

export async function generateBatch(count = 5, kinds = null) {
  const r = await fetch(`${API}/runtime/signals/generate_batch`, {
    method: 'POST',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify({ count, kinds }),
  });
  if (!r.ok) throw new Error(r.statusText);
  return r.json();
}

export async function runtimeApply(signals) {
  const r = await fetch(`${API}/runtime/apply`, {
    method: 'POST',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify({ signals: Array.isArray(signals) ? signals : [] }),
  });
  const j = await r.json().catch(() => ({}));
  if (!r.ok) throw new Error(j.detail || r.statusText);
  return j;
}

export async function runtimeAutoEvolve(sequenceLength = 10, maxSteps = 500) {
  const r = await fetch(`${API}/runtime/auto_evolve`, {
    method: 'POST',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify({ sequence_length: sequenceLength, max_steps: maxSteps }),
  });
  const j = await r.json().catch(() => ({}));
  if (!r.ok) throw new Error(j.detail || r.statusText);
  return j;
}

export async function runtimeLogSave(signals, name = null) {
  const r = await fetch(`${API}/runtime/log/save`, {
    method: 'POST',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify({ signals, name }),
  });
  if (!r.ok) throw new Error(r.statusText);
  return r.json();
}

export async function runtimeLogList() {
  const r = await fetch(`${API}/runtime/log/list`);
  if (!r.ok) throw new Error(r.statusText);
  return r.json();
}

export async function runtimeLogRecordingStatus() {
  const r = await fetch(`${API}/runtime/log/recording/status`);
  if (!r.ok) throw new Error(r.statusText);
  return r.json();
}

export async function runtimeLogRecordingStart() {
  const r = await fetch(`${API}/runtime/log/recording/start`, { method: 'POST' });
  if (!r.ok) throw new Error(r.statusText);
  return r.json();
}

export async function runtimeLogRecordingStop(name = null) {
  const url = name ? `${API}/runtime/log/recording/stop?name=${encodeURIComponent(name)}` : `${API}/runtime/log/recording/stop`;
  const r = await fetch(url, { method: 'POST' });
  if (!r.ok) throw new Error(r.statusText);
  return r.json();
}

export async function runtimeLogReplay(logId = null, signals = null, replayAndEvolve = false) {
  const r = await fetch(`${API}/runtime/log/replay`, {
    method: 'POST',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify({ id: logId, signals, replay_and_evolve: replayAndEvolve }),
  });
  if (!r.ok) throw new Error(r.statusText);
  return r.json();
}

export async function clearSignals() {
  const r = await fetch(`${API}/runtime/signals`, { method: 'DELETE' });
  if (!r.ok) throw new Error(r.statusText);
  return r.json();
}

export async function getKernelState() {
  const r = await fetch(`${API}/kernel/state`);
  if (!r.ok) throw new Error(r.statusText);
  return r.json();
}

export async function kernelReset() {
  const r = await fetch(`${API}/kernel/reset`, { method: 'POST' });
  if (!r.ok) throw new Error(r.statusText);
  return r.json();
}

export async function kernelIngest(signal) {
  const r = await fetch(`${API}/kernel/ingest`, {
    method: 'POST',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify({ signal }),
  });
  if (!r.ok) {
    const e = await r.json().catch(() => ({}));
    throw new Error(e.detail || r.statusText);
  }
  return r.json();
}

export async function kernelStep() {
  const r = await fetch(`${API}/kernel/step`, { method: 'POST' });
  const j = await r.json().catch(() => ({}));
  if (!r.ok) {
    const err = new Error(j.detail || r.statusText);
    if (j.need_new_type) err.needNewType = j.need_new_type;
    throw err;
  }
  return j;
}

export async function kernelStepDrain(maxSteps = 500) {
  const r = await fetch(`${API}/kernel/step_drain?max_steps=${maxSteps}`, { method: 'POST' });
  const j = await r.json().catch(() => ({}));
  if (!r.ok) throw new Error(j.detail || r.statusText);
  return j;
}

export async function getKernelTrace() {
  const r = await fetch(`${API}/kernel/trace`);
  if (!r.ok) throw new Error(r.statusText);
  return r.json();
}

export async function getGraph(step = null) {
  const q = step != null ? `?step=${step}` : '';
  const r = await fetch(`${API}/graph${q}`);
  if (!r.ok) throw new Error(r.statusText);
  return r.json();
}

export async function getTrajectoryMeta() {
  const r = await fetch(`${API}/graph/trajectory_meta`);
  if (!r.ok) throw new Error(r.statusText);
  return r.json();
}

// ---------- Scenarios ----------
export async function scenarioRootCause() {
  const r = await fetch(`${API}/scenarios/root_cause`);
  if (!r.ok) throw new Error(r.statusText);
  return r.json();
}

export async function scenarioCounterfactual() {
  const r = await fetch(`${API}/scenarios/counterfactual`);
  if (!r.ok) throw new Error(r.statusText);
  return r.json();
}

export async function getCounterfactualEnv() {
  const r = await fetch(`${API}/scenarios/counterfactual/env`);
  if (!r.ok) throw new Error(r.statusText);
  return r.json();
}

export async function getAuditEnv() {
  const r = await fetch(`${API}/scenarios/audit/env`);
  if (!r.ok) throw new Error(r.statusText);
  return r.json();
}

export async function scenarioAudit() {
  const r = await fetch(`${API}/scenarios/audit`);
  if (!r.ok) throw new Error(r.statusText);
  return r.json();
}

export async function getGovernanceEnv() {
  const r = await fetch(`${API}/scenarios/evolve_idle/env`);
  if (!r.ok) throw new Error(r.statusText);
  return r.json();
}

export async function scenarioEvolveIdle() {
  const r = await fetch(`${API}/scenarios/evolve_idle`, { method: 'POST' });
  if (!r.ok) throw new Error(r.statusText);
  return r.json();
}

export async function getAiGovernanceEnv() {
  const r = await fetch(`${API}/scenarios/verify_ai/env`);
  if (!r.ok) throw new Error(r.statusText);
  return r.json();
}

export async function scenarioVerifyAi(termExpr, typeExpr) {
  const r = await fetch(`${API}/scenarios/verify_ai`, {
    method: 'POST',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify({ term_expr: termExpr, type_expr: typeExpr }),
  });
  if (!r.ok) throw new Error(r.statusText);
  return r.json();
}

export async function getComplianceEnv() {
  const r = await fetch(`${API}/scenarios/compliance/env`);
  if (!r.ok) throw new Error(r.statusText);
  return r.json();
}

export async function scenarioComplianceCheck(signal) {
  const r = await fetch(`${API}/scenarios/compliance_check`, {
    method: 'POST',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify({ signal }),
  });
  if (!r.ok) throw new Error(r.statusText);
  return r.json();
}
