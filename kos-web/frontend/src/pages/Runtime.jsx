import { useState, useEffect } from 'react'
import { Link } from 'react-router-dom'
import { useLocale } from '../context/LocaleContext'
import {
  getSignalsLatest,
  generateSignal,
  generateBatch,
  clearSignals,
  runtimeApply,
  runtimeAutoEvolve,
  runtimeLogSave,
  runtimeLogList,
  runtimeLogReplay,
  runtimeLogRecordingStatus,
  runtimeLogRecordingStart,
  runtimeLogRecordingStop,
  getValidEventTypes,
  suggestSignalsFromTrace,
  getLogSignals,
  getCreatableTypes,
} from '../api'

const VALID_KINDS = ['proc_step', 'anomaly', 'fail_evt', 'sensor', 'qc_failure']

function useKinds(t) {
  return [
    { value: 'random', label: t('kindRandom') },
    { value: 'proc_step', label: t('kindProcStep') },
    { value: 'anomaly', label: t('kindAnomaly') },
    { value: 'fail_evt', label: t('kindFailEvt') },
    { value: 'sensor', label: t('kindSensor') },
    { value: 'qc_failure', label: t('kindQcFailure') },
  ].filter((k) => VALID_KINDS.includes(k.value) || k.value === 'random')
}

export default function Runtime() {
  const { t } = useLocale()
  const KINDS = useKinds(t)
  const [signals, setSignals] = useState([])
  const [loading, setLoading] = useState(true)
  const [busy, setBusy] = useState(false)
  const [error, setError] = useState(null)
  const [needNewType, setNeedNewType] = useState(null)
  const [logs, setLogs] = useState([])
  const [batchCount, setBatchCount] = useState(5)
  const [recording, setRecording] = useState(false)
  const [recordingCount, setRecordingCount] = useState(0)
  const [logName, setLogName] = useState('')
  const [lastEnqueued, setLastEnqueued] = useState(null)
  const [validEventTypes, setValidEventTypes] = useState([])
  const [suggestedSignals, setSuggestedSignals] = useState([])
  const [applyRejected, setApplyRejected] = useState([])
  const [applySource, setApplySource] = useState('buffer') // 'buffer' | 'suggested'
  const [creatableTypes, setCreatableTypes] = useState([])
  const [autoEvolveLength, setAutoEvolveLength] = useState(10)
  const [autoEvolveResult, setAutoEvolveResult] = useState(null)
  const [replayResult, setReplayResult] = useState(null)

  const load = async () => {
    setLoading(true)
    try {
      const r = await getSignalsLatest(50)
      setSignals(r.signals || [])
      setError(null)
    } catch (e) {
      setError(e.message)
    } finally {
      setLoading(false)
    }
  }

  const loadLogs = async () => {
    try {
      const r = await runtimeLogList()
      setLogs(r.logs || [])
    } catch (_) {}
  }

  const loadRecordingStatus = async () => {
    try {
      const r = await runtimeLogRecordingStatus()
      setRecording(r.recording ?? false)
      setRecordingCount(r.count ?? 0)
    } catch (_) {}
  }

  const loadValidEventTypes = async () => {
    try {
      const r = await getValidEventTypes()
      setValidEventTypes(r.event_types || [])
    } catch (_) {}
  }

  const loadCreatableTypes = async () => {
    try {
      const r = await getCreatableTypes()
      setCreatableTypes(r.creatable_types || [])
    } catch (_) {}
  }

  useEffect(() => {
    load()
  }, [])
  useEffect(() => {
    loadLogs()
  }, [])
  useEffect(() => {
    loadValidEventTypes()
  }, [])
  useEffect(() => {
    loadCreatableTypes()
  }, [])
  useEffect(() => {
    loadRecordingStatus()
  }, [])
  useEffect(() => {
    if (!recording) return
    const t = setInterval(loadRecordingStatus, 1500)
    return () => clearInterval(t)
  }, [recording])

  const onGenerate = async (kind) => {
    setBusy(true)
    setError(null)
    try {
      await generateSignal(kind)
      await load()
    } catch (e) {
      setError(e.message)
    } finally {
      setBusy(false)
    }
  }

  const onBatch = async () => {
    setBusy(true)
    setError(null)
    try {
      await generateBatch(batchCount)
      await load()
    } catch (e) {
      setError(e.message)
    } finally {
      setBusy(false)
    }
  }

  const onApply = async (useSuggested = false) => {
    setBusy(true)
    setError(null)
    setNeedNewType(null)
    setApplyRejected([])
    try {
      const toApply = useSuggested ? suggestedSignals : (await getSignalsLatest(100)).signals || []
      if (toApply.length === 0) {
        setError(useSuggested ? t('noSuggestedToApply') : t('noSignalsToApply'))
        setBusy(false)
        return
      }
      const res = await runtimeApply(toApply)
      setError(null)
      setLastEnqueued(res.enqueued ?? 0)
      setApplyRejected(res.rejected || [])
      if (res.rejected_count > 0) {
        setApplySource(useSuggested ? 'suggested' : 'buffer')
      }
      if (recording && (res.enqueued || 0) > 0) await loadRecordingStatus()
      if (!useSuggested) await load()
    } catch (e) {
      const msg = e.message || ''
      if (msg.includes('类型') || msg.includes('type')) {
        setNeedNewType({ message: msg, suggestion: msg })
      }
      setError(e.message)
    } finally {
      setBusy(false)
    }
  }

  const onRecordingStart = async () => {
    setBusy(true)
    setError(null)
    try {
      await runtimeLogRecordingStart()
      setRecording(true)
      setRecordingCount(0)
    } catch (e) {
      setError(e.message)
    } finally {
      setBusy(false)
    }
  }

  const onRecordingStop = async () => {
    setBusy(true)
    setError(null)
    try {
      const r = await runtimeLogRecordingStop(logName.trim() || null)
      setRecording(false)
      setRecordingCount(0)
      setLogName('')
      await loadLogs()
    } catch (e) {
      setError(e.message)
    } finally {
      setBusy(false)
    }
  }

  const onSaveLog = async () => {
    setBusy(true)
    setError(null)
    try {
      const r = await getSignalsLatest(100)
      const name = (logName.trim() || `log_${Date.now()}`)
      await runtimeLogSave(r.signals || [], name)
      setLogName('')
      await loadLogs()
    } catch (e) {
      setError(e.message)
    } finally {
      setBusy(false)
    }
  }

  const onReplay = async (logId, andEvolve = false) => {
    setBusy(true)
    setError(null)
    setReplayResult(null)
    try {
      const r = await runtimeLogReplay(logId, null, andEvolve)
      setReplayResult(r)
      if (r.enqueued != null) {
        setLastEnqueued(r.enqueued)
        setApplyRejected(r.rejected || [])
      }
    } catch (e) {
      setError(e.message)
    } finally {
      setBusy(false)
    }
  }

  const onAutoEvolve = async () => {
    setBusy(true)
    setError(null)
    setAutoEvolveResult(null)
    try {
      const r = await runtimeAutoEvolve(autoEvolveLength, 500)
      setAutoEvolveResult(r)
      if (r.enqueued != null) setLastEnqueued(r.enqueued)
      await load()
      await loadLogs()
    } catch (e) {
      setError(e.message)
    } finally {
      setBusy(false)
    }
  }

  const onSaveAutoEvolveLog = async (name) => {
    if (!autoEvolveResult?.generated_signals?.length) return
    setBusy(true)
    setError(null)
    try {
      await runtimeLogSave(autoEvolveResult.generated_signals, name || `auto_evolve_${Date.now()}`)
      await loadLogs()
    } catch (e) {
      setError(e.message)
    } finally {
      setBusy(false)
    }
  }

  const formatLogTime = (ts) => {
    if (ts == null) return ''
    try {
      const d = new Date(ts * 1000)
      return d.toLocaleString()
    } catch (_) {
      return ''
    }
  }

  const onClear = async () => {
    setBusy(true)
    setError(null)
    setLastEnqueued(null)
    setApplyRejected([])
    try {
      await clearSignals()
      await load()
    } catch (e) {
      setError(e.message)
    } finally {
      setBusy(false)
    }
  }

  const onSuggestFromTrace = async () => {
    setBusy(true)
    setError(null)
    try {
      const r = await suggestSignalsFromTrace(15)
      setSuggestedSignals(r.signals || [])
    } catch (e) {
      setError(e.message)
    } finally {
      setBusy(false)
    }
  }

  const onLoadLogSignals = async (logId) => {
    setBusy(true)
    setError(null)
    try {
      const r = await getLogSignals(logId)
      setSuggestedSignals(r.signals || [])
    } catch (e) {
      setError(e.message)
    } finally {
      setBusy(false)
    }
  }

  const updateSuggestedSignal = (index, field, value) => {
    setSuggestedSignals((prev) => {
      const next = prev.map((s, i) => (i === index ? { ...s, [field]: value } : s))
      return next
    })
  }

  const removeSuggested = (index) => {
    setSuggestedSignals((prev) => prev.filter((_, i) => i !== index))
  }

  return (
    <div>
      <div className="card">
        <h2>{t('runtimeTitle')}</h2>
        <p style={{ color: 'var(--text-muted)', marginBottom: 8 }}>{t('runtimeIntro')}</p>
        {validEventTypes.length > 0 && (
          <p style={{ fontSize: 12, color: 'var(--text-muted)', marginBottom: 12 }}>
            {t('validEventTypesHint')}: {validEventTypes.join(', ')}
          </p>
        )}
        {error && <p style={{ color: 'var(--danger)' }}>{error}</p>}
        {lastEnqueued != null && lastEnqueued > 0 && (
          <p style={{ color: 'var(--success)', marginBottom: 12 }}>
            {t('applyEnqueued', { n: lastEnqueued })}
            {applyRejected.length > 0 && (
              <span style={{ color: 'var(--warn)', marginLeft: 8 }}>{t('applyRejected', { n: applyRejected.length })}</span>
            )}
            <Link to="/kernel" style={{ marginLeft: 8 }}>{t('applyGoToKernel')}</Link>
          </p>
        )}
        {creatableTypes.length > 0 && (
          <div style={{ marginBottom: 12 }}>
            <span style={{ fontSize: 12, color: 'var(--text-muted)', marginRight: 8 }}>{t('creatableTypesFromCore')}</span>
            <div style={{ display: 'flex', gap: 6, flexWrap: 'wrap' }}>
              {creatableTypes.map((ct) => (
                <button key={`${ct.constructor}-${ct.signal_kind}`} onClick={() => onGenerate(ct.signal_kind)} disabled={busy} title={ct.target_type}>
                  {ct.label}
                </button>
              ))}
            </div>
          </div>
        )}
        <div style={{ display: 'flex', gap: 8, flexWrap: 'wrap', marginBottom: 16 }}>
          {KINDS.map((k) => (
            <button key={k.value} onClick={() => onGenerate(k.value)} disabled={busy} className="primary">{k.label}</button>
          ))}
          <input type="number" min={1} max={50} value={batchCount} onChange={(e) => setBatchCount(Number(e.target.value) || 5)} style={{ width: 48 }} />
          <button onClick={onBatch} disabled={busy}>{t('batchN', { n: batchCount })}</button>
          <button onClick={() => onApply(false)} disabled={busy} className="primary">{t('applySignals')}</button>
          <button onClick={onClear} disabled={busy}>{t('commonClear')}</button>
        </div>
      </div>

      <div className="card">
        <h2>{t('autoEvolveTitle')}</h2>
        <p style={{ color: 'var(--text-muted)', marginBottom: 12 }}>{t('autoEvolveIntro')}</p>
        <div style={{ display: 'flex', gap: 8, flexWrap: 'wrap', alignItems: 'center', marginBottom: 12 }}>
          <label style={{ fontSize: 13 }}>{t('autoEvolveSequenceLength')}</label>
          <input
            type="number"
            min={1}
            max={200}
            value={autoEvolveLength}
            onChange={(e) => setAutoEvolveLength(Number(e.target.value) || 10)}
            style={{ width: 56 }}
          />
          <button onClick={onAutoEvolve} disabled={busy} className="primary">{t('autoEvolveGenerate')}</button>
          {autoEvolveResult?.ok && (
            <>
              <span style={{ color: 'var(--success)', fontSize: 13 }}>
                {t('autoEvolveResult', {
                  generated: autoEvolveResult.generated,
                  enqueued: autoEvolveResult.enqueued,
                  steps: autoEvolveResult.steps_done,
                })}
              </span>
              <Link to="/kernel" style={{ fontSize: 13 }}>{t('autoEvolveGoToKernel')}</Link>
              <Link to="/scenarios/audit" style={{ fontSize: 13 }}>{t('autoEvolveGoToScenarios')}</Link>
              {autoEvolveResult.generated_signals?.length > 0 && (
                <button onClick={() => onSaveAutoEvolveLog()} disabled={busy} style={{ fontSize: 12 }}>{t('autoEvolveSaveAsLog')}</button>
              )}
            </>
          )}
        </div>
        {autoEvolveResult?.need_new_type && (
          <p style={{ color: 'var(--warn)', fontSize: 12 }}>{t('needNewTypeTitle')}: {autoEvolveResult.need_new_type}</p>
        )}
      </div>

      <div className="card">
        <h2>{t('suggestFromHistoryTitle')}</h2>
        <p style={{ color: 'var(--text-muted)', marginBottom: 12 }}>{t('suggestFromHistoryIntro')}</p>
        <div style={{ display: 'flex', gap: 8, flexWrap: 'wrap', marginBottom: 12 }}>
          <button onClick={onSuggestFromTrace} disabled={busy}>{t('suggestFromTrace')}</button>
          <select
            style={{ minWidth: 140 }}
            onChange={(e) => { const v = e.target.value; if (v) onLoadLogSignals(v); e.target.value = ''; }}
            disabled={busy}
          >
            <option value="">{t('loadFromLog')}...</option>
            {logs.map((log) => (
              <option key={log.id} value={log.id}>{log.name || log.id} ({log.count})</option>
            ))}
          </select>
          {suggestedSignals.length > 0 && (
            <button onClick={() => onApply(true)} disabled={busy} className="primary">{t('applySuggested')}</button>
          )}
        </div>
        {suggestedSignals.length > 0 && (
          <div style={{ marginTop: 8 }}>
            <h3 style={{ fontSize: 14 }}>{t('editableEventList')} ({suggestedSignals.length})</h3>
            <ul style={{ listStyle: 'none', padding: 0, margin: 0 }}>
              {suggestedSignals.map((s, i) => (
                <li key={i} style={{ padding: '8px 0', borderBottom: '1px solid var(--border)', display: 'flex', alignItems: 'center', gap: 8, flexWrap: 'wrap' }}>
                  <span style={{ color: 'var(--warn)', fontFamily: 'var(--font-mono)', fontSize: 12 }}>[{s.kind}]</span>
                  {s.kind === 'proc_step' && (
                    <>
                      <input placeholder="batch_id" value={s.batch_id || ''} onChange={(e) => updateSuggestedSignal(i, 'batch_id', e.target.value)} style={{ width: 90, fontSize: 12 }} />
                      <input placeholder="machine" value={s.machine || ''} onChange={(e) => updateSuggestedSignal(i, 'machine', e.target.value)} style={{ width: 90, fontSize: 12 }} />
                      <input type="number" placeholder="start_time" value={s.start_time ?? ''} onChange={(e) => updateSuggestedSignal(i, 'start_time', e.target.value ? Number(e.target.value) : undefined)} style={{ width: 70, fontSize: 12 }} />
                      <input type="number" placeholder="end_time" value={s.end_time ?? ''} onChange={(e) => updateSuggestedSignal(i, 'end_time', e.target.value ? Number(e.target.value) : undefined)} style={{ width: 70, fontSize: 12 }} />
                    </>
                  )}
                  {s.kind === 'anomaly' && (
                    <>
                      <input placeholder="machine" value={s.machine || ''} onChange={(e) => updateSuggestedSignal(i, 'machine', e.target.value)} style={{ width: 90, fontSize: 12 }} />
                      <input placeholder="param" value={s.param || ''} onChange={(e) => updateSuggestedSignal(i, 'param', e.target.value)} style={{ width: 90, fontSize: 12 }} />
                      <input type="number" step="any" placeholder="value" value={s.value ?? ''} onChange={(e) => updateSuggestedSignal(i, 'value', e.target.value ? Number(e.target.value) : undefined)} style={{ width: 70, fontSize: 12 }} />
                      <input type="number" placeholder="timestamp" value={s.timestamp ?? ''} onChange={(e) => updateSuggestedSignal(i, 'timestamp', e.target.value ? Number(e.target.value) : undefined)} style={{ width: 70, fontSize: 12 }} />
                    </>
                  )}
                  {s.kind === 'fail_evt' && (
                    <>
                      <input placeholder="batch_id" value={s.batch_id || ''} onChange={(e) => updateSuggestedSignal(i, 'batch_id', e.target.value)} style={{ width: 90, fontSize: 12 }} />
                      <input placeholder="error_code" value={s.error_code || ''} onChange={(e) => updateSuggestedSignal(i, 'error_code', e.target.value)} style={{ width: 90, fontSize: 12 }} />
                      <input type="number" placeholder="timestamp" value={s.timestamp ?? ''} onChange={(e) => updateSuggestedSignal(i, 'timestamp', e.target.value ? Number(e.target.value) : undefined)} style={{ width: 70, fontSize: 12 }} />
                    </>
                  )}
                  {s.kind === 'sensor' && (
                    <>
                      <input placeholder="name" value={s.name || ''} onChange={(e) => updateSuggestedSignal(i, 'name', e.target.value)} style={{ width: 90, fontSize: 12 }} />
                      <input type="number" step="any" placeholder="value" value={s.value ?? ''} onChange={(e) => updateSuggestedSignal(i, 'value', e.target.value ? Number(e.target.value) : undefined)} style={{ width: 70, fontSize: 12 }} />
                      <input placeholder="unit" value={s.unit || ''} onChange={(e) => updateSuggestedSignal(i, 'unit', e.target.value)} style={{ width: 50, fontSize: 12 }} />
                      <input type="number" placeholder="ts" value={s.ts ?? s.timestamp ?? ''} onChange={(e) => { const v = e.target.value ? Number(e.target.value) : undefined; updateSuggestedSignal(i, 'ts', v); updateSuggestedSignal(i, 'timestamp', v); }} style={{ width: 70, fontSize: 12 }} />
                    </>
                  )}
                  {s.kind === 'qc_failure' && (
                    <>
                      <input placeholder="batch" value={s.batch || ''} onChange={(e) => updateSuggestedSignal(i, 'batch', e.target.value)} style={{ width: 90, fontSize: 12 }} />
                      <input placeholder="defect" value={s.defect || ''} onChange={(e) => updateSuggestedSignal(i, 'defect', e.target.value)} style={{ width: 90, fontSize: 12 }} />
                      <input type="number" placeholder="severity" value={s.severity ?? ''} onChange={(e) => updateSuggestedSignal(i, 'severity', e.target.value ? Number(e.target.value) : undefined)} style={{ width: 60, fontSize: 12 }} />
                      <input type="number" placeholder="ts" value={s.ts ?? s.timestamp ?? ''} onChange={(e) => { const v = e.target.value ? Number(e.target.value) : undefined; updateSuggestedSignal(i, 'ts', v); updateSuggestedSignal(i, 'timestamp', v); }} style={{ width: 70, fontSize: 12 }} />
                    </>
                  )}
                  <button type="button" onClick={() => removeSuggested(i)} style={{ fontSize: 11 }}>{t('commonRemove')}</button>
                </li>
              ))}
            </ul>
          </div>
        )}
        {applyRejected.length > 0 && (
          <div style={{ marginTop: 12, padding: 10, background: 'var(--bg)', borderRadius: 8, border: '1px solid var(--warn)' }}>
            <strong>{t('rejectedList')}</strong>
            <ul style={{ listStyle: 'none', padding: 0, margin: '8px 0 0 0', fontSize: 12 }}>
              {applyRejected.slice(0, 10).map((r, i) => (
                <li key={i} style={{ padding: '4px 0' }}><span style={{ color: 'var(--warn)' }}>{r.reason}</span> — <code>{JSON.stringify(r.signal).slice(0, 80)}…</code></li>
              ))}
              {applyRejected.length > 10 && <li style={{ color: 'var(--text-muted)' }}>… 共 {applyRejected.length} 条被拒绝</li>}
            </ul>
          </div>
        )}
      </div>

      <div className="card">
        <h2>{t('logSectionTitle')}</h2>
        <p style={{ color: 'var(--text-muted)', marginBottom: 16 }}>{t('logSectionIntro')}</p>
        <div style={{ display: 'flex', gap: 8, flexWrap: 'wrap', alignItems: 'center', marginBottom: 16 }}>
          {recording ? (
            <>
              <span style={{ color: 'var(--warn)' }}>{t('logRecordingStatus', { n: recordingCount })}</span>
              <input
                type="text"
                placeholder={t('logNamePlaceholder')}
                value={logName}
                onChange={(e) => setLogName(e.target.value)}
                style={{ width: 180 }}
              />
              <button onClick={onRecordingStop} disabled={busy} className="primary">{t('logRecordingStop')}</button>
            </>
          ) : (
            <>
              <button onClick={onRecordingStart} disabled={busy}>{t('logRecordingStart')}</button>
              <input
                type="text"
                placeholder={t('logNamePlaceholder')}
                value={logName}
                onChange={(e) => setLogName(e.target.value)}
                style={{ width: 160 }}
              />
              <button onClick={onSaveLog} disabled={busy}>{t('logSaveCurrent')}</button>
            </>
          )}
        </div>
        <h3 style={{ marginBottom: 8 }}>{t('logListTitle')}</h3>
        {replayResult?.steps_done != null && replayResult.steps_done > 0 && (
          <p style={{ color: 'var(--success)', marginBottom: 8 }}>{t('logReplayStepsDone', { n: replayResult.steps_done })}</p>
        )}
        {logs.length === 0 ? (
          <p style={{ color: 'var(--text-muted)' }}>{t('logListEmpty')}</p>
        ) : (
          <ul style={{ listStyle: 'none', padding: 0, margin: 0 }}>
            {logs.map((log) => (
              <li key={log.id} style={{ display: 'flex', alignItems: 'center', gap: 12, padding: '8px 0', borderBottom: '1px solid var(--border)' }}>
                <span style={{ flex: 1, fontFamily: 'var(--font-mono)', fontSize: 13 }}>{log.name || log.id}</span>
                <span style={{ color: 'var(--text-muted)', fontSize: 12 }}>{t('logCount', { n: log.count || 0 })}</span>
                {log.created_at != null && (
                  <span style={{ color: 'var(--text-muted)', fontSize: 11 }}>{formatLogTime(log.created_at)}</span>
                )}
                <button onClick={() => onReplay(log.id)} disabled={busy} className="primary" style={{ fontSize: 12 }}>{t('logReplayThis')}</button>
                <button onClick={() => onReplay(log.id, true)} disabled={busy} style={{ fontSize: 12 }}>{t('logReplayAndEvolve')}</button>
              </li>
            ))}
          </ul>
        )}
      </div>
      {needNewType && (
        <div className="card" style={{ borderColor: 'var(--warn)' }}>
          <h3>{t('needNewTypeTitle')}</h3>
          <p>{needNewType.message}</p>
          {needNewType.suggestion && <pre style={{ fontSize: 12, overflow: 'auto' }}>{needNewType.suggestion}</pre>}
          <p style={{ color: 'var(--text-muted)' }}>{t('needNewTypeHint')}</p>
          <button onClick={() => setNeedNewType(null)}>{t('commonClose')}</button>
        </div>
      )}
      <div className="card">
        <h2>{t('latestSignals')}</h2>
        {loading ? (
          <p>{t('commonLoading')}</p>
        ) : signals.length === 0 ? (
          <p style={{ color: 'var(--text-muted)' }}>{t('noSignals')}</p>
        ) : (
          <ul style={{ listStyle: 'none', padding: 0, margin: 0 }}>
            {signals.map((s, i) => (
              <li key={i} style={{ padding: '10px 0', borderBottom: '1px solid var(--border)', fontFamily: 'var(--font-mono)', fontSize: 13 }}>
                <span style={{ color: 'var(--warn)' }}>[{s.kind}]</span>{' '}
                {s.batch_id && `batch=${s.batch_id} `}
                {s.machine && `machine=${s.machine} `}
                {s.param && `param=${s.param} `}
                {s.value != null && `value=${s.value} `}
                {s.name != null && `name=${s.name} `}
                {s.defect != null && `defect=${s.defect} `}
                {s.severity != null && `severity=${s.severity} `}
                {s.error_code && `err=${s.error_code} `}
                ts={typeof s.timestamp === 'number' ? s.timestamp.toFixed(1) : s.timestamp}
              </li>
            ))}
          </ul>
        )}
      </div>
    </div>
  )
}
