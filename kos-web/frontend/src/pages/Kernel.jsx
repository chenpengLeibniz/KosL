import { useState, useEffect, useMemo } from 'react'
import { useLocation, Link } from 'react-router-dom'
import { useLocale } from '../context/LocaleContext'
import { getKernelState, kernelReset, kernelStep, kernelStepDrain, coreAddType } from '../api'

export default function Kernel() {
  const { t } = useLocale()
  const location = useLocation()
  const [state, setState] = useState(null)
  const [loading, setLoading] = useState(true)
  const [busy, setBusy] = useState(false)
  const [error, setError] = useState(null)
  const [needNewType, setNeedNewType] = useState(null)
  const [traceEventFilter, setTraceEventFilter] = useState('')
  const [traceExpanded, setTraceExpanded] = useState({})

  const load = async () => {
    setLoading(true)
    setError(null)
    const controller = new AbortController()
    const timeoutId = setTimeout(() => controller.abort(), 15000) // 15s 超时
    try {
      const r = await fetch('/api/kernel/state', { signal: controller.signal })
      clearTimeout(timeoutId)
      if (!r.ok) throw new Error(r.statusText || `HTTP ${r.status}`)
      const data = await r.json()
      setState(data)
    } catch (e) {
      clearTimeout(timeoutId)
      if (e.name === 'AbortError') {
        setError('请求超时，请确认后端已启动（默认端口 8000）。')
      } else {
        setError(e.message || '无法连接后端，请确认已启动：cd kos-web/backend && uvicorn app.main:app --reload --port 8000')
      }
    } finally {
      setLoading(false)
    }
  }

  // 进入 Kernel 页时始终拉取最新状态（解决 Runtime 生效后队列/K 不同步问题）
  useEffect(() => {
    if (location.pathname === '/kernel') {
      load()
    }
  }, [location.pathname])

  // 以下必须放在任何 return 之前，保证 Hooks 每次渲染顺序一致（否则 /kernel 白屏）
  const gamma = state?.gamma || {}
  const types = Array.isArray(gamma.types) ? gamma.types : []
  const predicates = Array.isArray(gamma.predicates) ? gamma.predicates : []
  const queue = Array.isArray(state?.queue_preview) ? state.queue_preview : []
  const queueLength = state?.queue_length ?? 0
  const rawTrace = Array.isArray(state?.trace) ? state.trace : []
  const traceEventTypes = useMemo(() => {
    const set = new Set()
    rawTrace.forEach((s) => {
      const et = s && typeof s === 'object' && s.event_pair && s.event_pair.event_type
      if (et) set.add(et)
    })
    return ['', ...Array.from(set).sort()]
  }, [rawTrace])
  const filteredTrace = useMemo(() => {
    if (!traceEventFilter) return rawTrace
    return rawTrace.filter((s) => (s?.event_pair?.event_type || '') === traceEventFilter)
  }, [rawTrace, traceEventFilter])
  const toggleTraceStep = (i) => {
    setTraceExpanded((prev) => ({ ...prev, [i]: !prev[i] }))
  }

  const onStep = async () => {
    setBusy(true)
    setError(null)
    setNeedNewType(null)
    try {
      const r = await kernelStep()
      if (r.need_new_type) {
        setNeedNewType(r.need_new_type)
      }
      setState(r.state)
    } catch (e) {
      const msg = e.message || ''
      if (e.needNewType) {
        setNeedNewType(e.needNewType)
      } else if (msg.includes('类型') || msg.includes('type') || msg.includes('新类型')) {
        setNeedNewType({ message: msg, suggestion: msg })
      }
      setError(e.message)
    } finally {
      setBusy(false)
    }
  }

  const onReset = async () => {
    setBusy(true)
    setError(null)
    setNeedNewType(null)
    try {
      const r = await kernelReset()
      setState(r)
    } catch (e) {
      setError(e.message)
    } finally {
      setBusy(false)
    }
  }

  const onStepDrain = async () => {
    setBusy(true)
    setError(null)
    setNeedNewType(null)
    try {
      const r = await kernelStepDrain(500)
      if (r.need_new_type) setNeedNewType(r.need_new_type)
      setState(r.state)
      if (r.stopped_reason === 'error' && r.error) setError(r.error)
    } catch (e) {
      setError(e.message)
    } finally {
      setBusy(false)
    }
  }

  if (loading) {
    return (
      <div className="card">
        <h2>{t('kernelTitle')}</h2>
        <p>{t('commonLoading')}</p>
      </div>
    )
  }

  return (
    <div>
      <div className="card">
        <h2>{t('kernelTitle')}</h2>
        <p style={{ color: 'var(--text-muted)', marginBottom: 16 }}>{t('kernelIntro')}</p>
        {error && (
          <div style={{ marginBottom: 12, padding: 12, background: 'var(--bg)', borderRadius: 8, border: '1px solid var(--danger)' }}>
            <p style={{ color: 'var(--danger)', margin: 0 }}>{error}</p>
            <button type="button" onClick={() => load()} style={{ marginTop: 8 }}>{t('retry')}</button>
          </div>
        )}
        {needNewType?.type_name && (
          <div className="modal-overlay" style={{ position: 'fixed', inset: 0, background: 'rgba(0,0,0,0.4)', display: 'flex', alignItems: 'center', justifyContent: 'center', zIndex: 1000 }} onClick={() => setNeedNewType(null)}>
            <div className="modal-card" style={{ background: 'var(--surface)', padding: 24, borderRadius: 12, maxWidth: 420, border: '1px solid var(--border)' }} onClick={(e) => e.stopPropagation()}>
              <h3 style={{ margin: '0 0 12px 0' }}>{t('newTypeModalTitle')}</h3>
              <p style={{ margin: 0, color: 'var(--text-muted)' }}>{t('newTypeModalMessage', { name: needNewType.type_name })}</p>
              <div style={{ display: 'flex', gap: 8, marginTop: 20 }}>
                <button className="primary" disabled={busy} onClick={async () => {
                  setBusy(true)
                  try {
                    await coreAddType(needNewType.type_name)
                    setNeedNewType(null)
                    setError(null)
                    await load()
                  } catch (err) {
                    setError(err.message)
                  } finally {
                    setBusy(false)
                  }
                }}>{t('newTypeAllowCreate')}</button>
                <button disabled={busy} onClick={() => setNeedNewType(null)}>{t('newTypeDiscard')}</button>
              </div>
            </div>
          </div>
        )}
        {needNewType && !needNewType.type_name && (
          <div style={{ marginBottom: 12, padding: 12, background: 'var(--bg)', borderRadius: 8, border: '1px solid var(--warn)' }}>
            <strong>{t('needNewTypeTitle')}</strong>
            <p style={{ margin: '8px 0 0 0' }}>{needNewType.message}</p>
            {needNewType.suggestion && <pre style={{ fontSize: 12, marginTop: 8 }}>{typeof needNewType.suggestion === 'string' ? needNewType.suggestion : JSON.stringify(needNewType.suggestion)}</pre>}
            <button onClick={() => setNeedNewType(null)} style={{ marginTop: 8 }}>{t('commonClose')}</button>
          </div>
        )}
        <div style={{ display: 'flex', gap: 8, flexWrap: 'wrap', marginBottom: 16 }}>
          <button onClick={onStep} disabled={busy || queueLength === 0} className="primary">{t('kernelStepOne')}</button>
          <button onClick={onStepDrain} disabled={busy || queueLength === 0}>{t('kernelStepDrain')}</button>
          <button onClick={onReset} disabled={busy}>{t('resetSigma')}</button>
        </div>
        <div style={{ display: 'flex', gap: 24, flexWrap: 'wrap', alignItems: 'center' }}>
          <span style={{ padding: '4px 10px', background: 'var(--bg)', borderRadius: 6, fontFamily: 'var(--font-mono)' }}>
            <strong>σ</strong> (TS={state?.TS ?? 0}, |K|={state?.K?.length ?? 0}, |P|={queueLength})
          </span>
          <span><strong>{t('logicClock')}</strong>: {state?.TS ?? 0}</span>
          <span><strong>{t('knowledgeCount')}</strong>: {state?.K?.length ?? 0}</span>
          <span><strong>{t('queueLength')}</strong>: {queueLength}</span>
          <span><strong>{t('traceSteps')}</strong>: {state?.trace_length ?? 0}</span>
        </div>
      </div>

      <div className="card">
        <h2>Γ ({t('gamma')})</h2>
        <p style={{ color: 'var(--text-muted)', fontSize: 13 }}>{t('gammaFromCore')}</p>
        {types.length === 0 && predicates.length === 0 ? (
          <p style={{ color: 'var(--text-muted)' }}>{t('gammaEmpty')}</p>
        ) : (
          <>
            <h3 style={{ fontSize: 14 }}>{t('typesInGamma')}</h3>
            <ul style={{ listStyle: 'none', padding: 0, margin: '0 0 12px 0', fontSize: 13 }}>
              {types.slice(0, 20).map((x, i) => (
                <li key={i} style={{ padding: '4px 0' }}><strong>{x.name}</strong> {x.definition && <span style={{ color: 'var(--text-muted)' }}>— {x.definition}</span>}</li>
              ))}
              {types.length > 20 && <li style={{ color: 'var(--text-muted)' }}>… 共 {types.length} 个</li>}
            </ul>
            <h3 style={{ fontSize: 14 }}>{t('predicates')}</h3>
            <ul style={{ listStyle: 'none', padding: 0, margin: 0, fontSize: 13 }}>
              {predicates.slice(0, 10).map((x, i) => (
                <li key={i} style={{ padding: '4px 0' }}><strong>{x.name}</strong> {x.description}</li>
              ))}
              {predicates.length > 10 && <li style={{ color: 'var(--text-muted)' }}>… 共 {predicates.length} 个</li>}
            </ul>
          </>
        )}
      </div>

      <div className="card">
        <h2>{t('eventQueue')}</h2>
        <p style={{ color: 'var(--text-muted)', fontSize: 13 }}>{t('eventQueueHint')}</p>
        {queueLength === 0 ? (
          <p style={{ color: 'var(--text-muted)' }}>{t('queueEmpty')}</p>
        ) : (
          <div className="queue-visual" style={{ display: 'flex', alignItems: 'flex-start', gap: 0, overflowX: 'auto', padding: '12px 0', minHeight: 80 }}>
            <div style={{ flexShrink: 0, alignSelf: 'stretch', display: 'flex', flexDirection: 'column', alignItems: 'center', justifyContent: 'center', padding: '0 8px', background: 'var(--accent)', color: 'var(--surface)', borderRadius: 8, marginRight: 8, fontSize: 12, fontWeight: 600 }}>
              {t('queueHead')}
            </div>
            {queue.slice(0, 30).map((s, i) => (
              <div key={i} style={{ flexShrink: 0, width: 120, padding: '8px 10px', background: 'var(--bg)', border: '1px solid var(--border)', borderRadius: 8, marginRight: 6, fontFamily: 'var(--font-mono)', fontSize: 11 }}>
                <div style={{ fontWeight: 600, marginBottom: 4, color: 'var(--accent)' }}>{s.kind}</div>
                {s.batch_id && <div style={{ overflow: 'hidden', textOverflow: 'ellipsis', whiteSpace: 'nowrap' }}>{s.batch_id}</div>}
                {s.machine && <div style={{ overflow: 'hidden', textOverflow: 'ellipsis', whiteSpace: 'nowrap' }}>{s.machine}</div>}
                <div style={{ color: 'var(--text-muted)', marginTop: 2 }}>ts={typeof s.timestamp === 'number' ? s.timestamp : (s.timestamp ?? '-')}</div>
              </div>
            ))}
            {queueLength > 30 && (
              <div style={{ flexShrink: 0, padding: '0 8px', display: 'flex', alignItems: 'center', color: 'var(--text-muted)', fontSize: 12 }}>… +{queueLength - 30}</div>
            )}
            <div style={{ flexShrink: 0, alignSelf: 'stretch', display: 'flex', flexDirection: 'column', alignItems: 'center', justifyContent: 'center', padding: '0 8px', background: 'var(--text-muted)', color: 'var(--surface)', borderRadius: 8, marginLeft: 6, fontSize: 12, fontWeight: 600 }}>
              {t('queueTail')}
            </div>
          </div>
        )}
      </div>

      <div className="card">
        <h2>{t('knowledgeSetK')}</h2>
        {!state?.K?.length ? (
          <p style={{ color: 'var(--text-muted)' }}>{t('noItems')}</p>
        ) : (
          <ul style={{ listStyle: 'none', padding: 0, margin: 0 }}>
            {state.K.map((item, i) => (
              <li key={i} style={{ padding: '8px 0', borderBottom: '1px solid var(--border)' }}>
                <strong>{item.type}</strong> {item.proof && `(${item.proof})`}
                <pre style={{ margin: '4px 0 0 0', fontSize: 12, overflow: 'auto' }}>{JSON.stringify(item.data, null, 0)}</pre>
              </li>
            ))}
          </ul>
        )}
      </div>

      <div className="card">
        <h2>{t('traceTitle')} / {t('knowledgeEvolveTrajectory')}</h2>
        <p style={{ color: 'var(--text-muted)', fontSize: 13, marginBottom: 12 }}>{t('eventQueueHint')}</p>
        {rawTrace.length > 0 && (
          <div style={{ marginBottom: 12 }}>
            <label style={{ display: 'inline-flex', alignItems: 'center', gap: 6 }}>
              <span style={{ color: 'var(--text-muted)', fontSize: 13 }}>{t('filterByEventType')}:</span>
              <select value={traceEventFilter} onChange={(e) => setTraceEventFilter(e.target.value)} style={{ minWidth: 100 }}>
                {traceEventTypes.map((et) => (
                  <option key={et || '_all'} value={et}>{et || t('filterAll')}</option>
                ))}
              </select>
            </label>
            <Link to="/trace" style={{ marginLeft: 12, fontSize: 13 }}>{t('traceFullPage')} →</Link>
          </div>
        )}
        {!rawTrace.length ? (
          <p style={{ color: 'var(--text-muted)' }}>{t('noTrace')}</p>
        ) : (
          <div style={{ position: 'relative', paddingLeft: 20 }}>
            <div style={{ position: 'absolute', left: 7, top: 10, bottom: 10, width: 2, background: 'var(--border)', borderRadius: 1 }} />
            {filteredTrace.slice(-20).reverse().map((s, idx) => {
              const i = s.step_index
              const isExp = traceExpanded[i] === true
              const et = s.event_pair?.event_type || ''
              return (
                <div key={i} style={{ position: 'relative', marginBottom: 8 }}>
                  <div style={{ position: 'absolute', left: -18, top: 8, width: 10, height: 10, borderRadius: '50%', background: 'var(--text-muted)', border: '2px solid var(--surface)' }} />
                  <div style={{ padding: '6px 10px', background: 'var(--bg)', borderRadius: 6, border: '1px solid var(--border)' }}>
                    <div style={{ display: 'flex', alignItems: 'center', justifyContent: 'space-between', flexWrap: 'wrap', gap: 6 }}>
                      <span style={{ color: 'var(--text-muted)', fontSize: 12 }}>{t('step')} {i}, TS={s.ts_after}</span>
                      {et && <span style={{ padding: '2px 6px', background: 'var(--surface)', borderRadius: 4, fontSize: 11 }}>{et}</span>}
                      <button type="button" onClick={() => toggleTraceStep(i)} style={{ fontSize: 11 }}>{isExp ? t('collapse') : t('expand')}</button>
                    </div>
                    <div style={{ fontFamily: 'var(--font-mono)', fontSize: 12, marginTop: 4 }}>{s.event_label}</div>
                    {isExp && s.event_pair?.data && (
                      <pre style={{ marginTop: 6, fontSize: 11, overflow: 'auto', background: 'var(--surface)', padding: 8, borderRadius: 4 }}>{JSON.stringify(s.event_pair.data, null, 1)}</pre>
                    )}
                    {isExp && s.scenario_result && (
                      <div style={{ marginTop: 6, padding: 6, background: 'var(--surface)', borderRadius: 4, fontSize: 12 }}>
                        {s.scenario_result.auto === 'root_cause' && t('autoRootCause')}
                        {s.scenario_result.auto === 'audit' && t('autoAudit')}
                        {(s.scenario_result.message || s.scenario_result.error) && <span> — {s.scenario_result.message || s.scenario_result.error}</span>}
                        {s.scenario_result.summary && <div style={{ marginTop: 4 }}>{s.scenario_result.summary}</div>}
                      </div>
                    )}
                  </div>
                </div>
              )
            })}
          </div>
        )}
      </div>

      {state?.materialized?.length > 0 && (
        <div className="card">
          <h2>{t('materialized')}</h2>
          <p style={{ color: 'var(--text-muted)', fontSize: 13 }}>{t('materializedHint')}</p>
          <ul style={{ listStyle: 'none', padding: 0, margin: 0, fontSize: 12 }}>
            {state.materialized.slice(-10).reverse().map((m, i) => (
              <li key={i} style={{ padding: '4px 0' }}>TS={m.ts} {m.item?.type}</li>
            ))}
          </ul>
        </div>
      )}
    </div>
  )
}
