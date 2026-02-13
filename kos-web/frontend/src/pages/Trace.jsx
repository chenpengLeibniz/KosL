import { useState, useEffect, useMemo } from 'react'
import { useLocale } from '../context/LocaleContext'
import { getKernelTrace, getTrajectoryMeta } from '../api'

export default function Trace() {
  const { t } = useLocale()
  const [trace, setTrace] = useState([])
  const [meta, setMeta] = useState({ steps: [], max_step: 0 })
  const [loading, setLoading] = useState(true)
  const [error, setError] = useState(null)
  const [eventTypeFilter, setEventTypeFilter] = useState('')
  const [expanded, setExpanded] = useState({})
  const [focusStep, setFocusStep] = useState(null)

  const load = async () => {
    setLoading(true)
    try {
      const [tRes, mRes] = await Promise.all([getKernelTrace(), getTrajectoryMeta()])
      setTrace(tRes.trace || [])
      setMeta({ steps: mRes.steps || [], max_step: mRes.max_step ?? 0 })
      setError(null)
    } catch (e) {
      setError(e.message)
    } finally {
      setLoading(false)
    }
  }

  useEffect(() => {
    load()
  }, [])

  const eventTypes = useMemo(() => {
    const set = new Set()
    trace.forEach((s) => {
      const et = s.event_pair?.event_type
      if (et) set.add(et)
    })
    return ['', ...Array.from(set).sort()]
  }, [trace])

  const filteredTrace = useMemo(() => {
    if (!eventTypeFilter) return trace
    return trace.filter((s) => (s.event_pair?.event_type || '') === eventTypeFilter)
  }, [trace, eventTypeFilter])

  const toggleExpanded = (i) => {
    setExpanded((prev) => ({ ...prev, [i]: !prev[i] }))
  }

  if (loading) return <div className="card">{t('commonLoading')}</div>

  return (
    <div>
      <div className="card">
        <h2>{t('traceTitle')}</h2>
        <p style={{ color: 'var(--text-muted)', marginBottom: 16 }}>{t('traceIntro')}</p>
        {error && <p style={{ color: 'var(--danger)' }}>{error}</p>}
        <div style={{ display: 'flex', gap: 12, flexWrap: 'wrap', alignItems: 'center' }}>
          <button onClick={load} disabled={loading}>{t('refresh')}</button>
          <label style={{ display: 'flex', alignItems: 'center', gap: 6 }}>
            <span style={{ color: 'var(--text-muted)', fontSize: 13 }}>{t('filterByEventType')}:</span>
            <select
              value={eventTypeFilter}
              onChange={(e) => setEventTypeFilter(e.target.value)}
              style={{ minWidth: 120 }}
            >
              {eventTypes.map((et) => (
                <option key={et || '_all'} value={et}>{et || t('filterAll')}</option>
              ))}
            </select>
          </label>
          {focusStep != null && (
            <button type="button" onClick={() => setFocusStep(null)}>{t('clearFocus')}</button>
          )}
        </div>
      </div>

      <div className="card">
        <h2>{t('stepList')} {filteredTrace.length < trace.length && `(${filteredTrace.length} / ${trace.length})`}</h2>
        {trace.length === 0 ? (
          <p style={{ color: 'var(--text-muted)' }}>{t('traceEmpty')}</p>
        ) : (
          <div className="trace-timeline" style={{ position: 'relative', paddingLeft: 24 }}>
            {/* 竖线 */}
            <div
              style={{
                position: 'absolute',
                left: 10,
                top: 12,
                bottom: 12,
                width: 2,
                background: 'var(--border)',
                borderRadius: 1,
              }}
            />
            {filteredTrace.map((s, idx) => {
              const i = s.step_index
              const isExp = expanded[i] !== false && (focusStep === i || focusStep == null)
              const isFocused = focusStep === i
              const et = s.event_pair?.event_type || ''
              return (
                <div
                  key={i}
                  role="button"
                  tabIndex={0}
                  onClick={() => setFocusStep(i)}
                  onKeyDown={(e) => { if (e.key === 'Enter' || e.key === ' ') { e.preventDefault(); setFocusStep(i); } }}
                  style={{
                    position: 'relative',
                    marginBottom: 12,
                    padding: '10px 12px',
                    background: isFocused ? 'var(--bg)' : 'transparent',
                    border: `1px solid ${isFocused ? 'var(--accent)' : 'var(--border)'}`,
                    borderRadius: 8,
                    cursor: 'pointer',
                  }}
                >
                  {/* 时间轴节点 */}
                  <div
                    style={{
                      position: 'absolute',
                      left: -20,
                      top: 14,
                      width: 12,
                      height: 12,
                      borderRadius: '50%',
                      background: isFocused ? 'var(--accent)' : 'var(--text-muted)',
                      border: '2px solid var(--surface)',
                    }}
                  />
                  <div style={{ display: 'flex', alignItems: 'center', justifyContent: 'space-between', flexWrap: 'wrap', gap: 8 }}>
                    <div>
                      <strong>{t('step')} {i}</strong>
                      <span style={{ color: 'var(--text-muted)', marginLeft: 8 }}>TS = {s.ts_after}</span>
                      {et && <span style={{ marginLeft: 8, padding: '2px 6px', background: 'var(--bg)', borderRadius: 4, fontSize: 12 }}>{et}</span>}
                    </div>
                    <button
                      type="button"
                      onClick={(e) => { e.stopPropagation(); toggleExpanded(i); }}
                      style={{ fontSize: 12 }}
                    >
                      {isExp ? t('collapse') : t('expand')}
                    </button>
                  </div>
                  <div style={{ marginTop: 4, fontFamily: 'var(--font-mono)', fontSize: 13 }}>{s.event_label}</div>
                  {isExp && (
                    <>
                      {s.event_pair?.data && (
                        <pre style={{ marginTop: 8, fontSize: 12, overflow: 'auto', background: 'var(--bg)', padding: 10, borderRadius: 6 }}>
                          {JSON.stringify(s.event_pair.data, null, 2)}
                        </pre>
                      )}
                      {s.scenario_result && (
                        <div style={{ marginTop: 8, padding: 10, background: 'var(--bg)', borderRadius: 6, fontSize: 13, borderLeft: '3px solid var(--accent)' }}>
                          {s.scenario_result.auto === 'root_cause' && <strong>{t('autoRootCause')}</strong>}
                          {s.scenario_result.auto === 'audit' && <strong>{t('autoAudit')}</strong>}
                          {(s.scenario_result.message || s.scenario_result.error) && <div style={{ marginTop: 4, color: 'var(--text-muted)' }}>{s.scenario_result.message || s.scenario_result.error}</div>}
                          {s.scenario_result.summary && <div style={{ marginTop: 4 }}>{s.scenario_result.summary}</div>}
                          {s.scenario_result.proof_term && <pre style={{ marginTop: 6, fontSize: 11, overflow: 'auto' }}>{s.scenario_result.proof_term}</pre>}
                          {s.scenario_result.audit && <details style={{ marginTop: 6 }}><summary>{t('vizRawJson')}</summary><pre style={{ fontSize: 11, maxHeight: 120, overflow: 'auto' }}>{JSON.stringify(s.scenario_result.audit, null, 1)}</pre></details>}
                        </div>
                      )}
                    </>
                  )}
                </div>
              )
            })}
          </div>
        )}
      </div>
    </div>
  )
}
