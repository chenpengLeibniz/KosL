import { useState, useEffect } from 'react'
import { Link } from 'react-router-dom'
import { useLocale } from '../context/LocaleContext'
import { getAuditEnv, scenarioAudit } from '../api'

const cardStyle = { padding: 20, marginBottom: 16, borderRadius: 12, background: 'var(--card-bg)', border: '1px solid var(--border)' }

export default function Audit() {
  const { t, locale } = useLocale()
  const [env, setEnv] = useState(null)
  const [loading, setLoading] = useState(false)
  const [result, setResult] = useState(null)
  const [error, setError] = useState(null)

  useEffect(() => {
    let cancelled = false
    getAuditEnv()
      .then((data) => { if (!cancelled) setEnv(data) })
      .catch((e) => { if (!cancelled) setError(e.message) })
    return () => { cancelled = true }
  }, [])

  const onRun = async () => {
    setLoading(true)
    setError(null)
    setResult(null)
    try {
      const data = await scenarioAudit()
      setResult(data)
    } catch (e) {
      setError(e.message)
    } finally {
      setLoading(false)
    }
  }

  const isZh = locale === 'zh'
  const desc = env && (isZh ? env.description_zh : env.description_en)
  const hint = env && (isZh ? env.signal_hint_zh : env.signal_hint_en)

  return (
    <div>
      <div className="card" style={{ marginBottom: 24 }}>
        <h2>{t('auditEnvTitle')}</h2>
        <p style={{ color: 'var(--text-muted)', marginBottom: 0 }}>{t('auditEnvIntro')}</p>
      </div>

      {error && <p style={{ color: 'var(--danger)', marginBottom: 16 }}>{error}</p>}

      {env && (
        <div className="card" style={cardStyle}>
          <h3 style={{ margin: '0 0 12px 0' }}>{t('auditEnvRule')}</h3>
          <p style={{ margin: 0, fontSize: 14, color: 'var(--text-muted)' }}>{desc}</p>
          <p style={{ margin: '12px 0 0 0', fontSize: 13, color: 'var(--text-muted)' }}>{hint}</p>
          <div style={{ marginTop: 12 }}>
            <Link to="/runtime" style={{ marginRight: 12 }}>{t('counterfactualGoRuntime')}</Link>
            <Link to="/kernel" style={{ marginRight: 12 }}>Kernel</Link>
            <Link to="/scenarios">{t('counterfactualGoScenarios')}</Link>
          </div>
        </div>
      )}

      <div className="card" style={cardStyle}>
        <h3 style={{ margin: '0 0 12px 0' }}>{t('auditRunTitle')}</h3>
        <button className="primary" onClick={onRun} disabled={loading} style={{ padding: '10px 20px' }}>
          {loading ? t('commonLoading') : t('scenario4Run')}
        </button>

        {result?.audit && (
          <div style={{ marginTop: 20 }}>
            <h4 style={{ margin: '0 0 8px 0', fontSize: 14 }}>{t('vizAuditTitle')}</h4>
            <p style={{ color: 'var(--text-muted)', fontSize: 13 }}>{t('auditTraceLength')}: {result.audit.trace_length}</p>
            <div className="scenario-audit-timeline" style={{ marginTop: 12 }}>
              {(result.audit.trace || []).slice(-15).map((s, i) => (
                <div key={i} className="scenario-audit-step" style={{ padding: '8px 0', borderBottom: '1px solid var(--border)' }}>
                  <span style={{ color: 'var(--text-muted)', fontSize: 12 }}>{t('step')} {s.step_index} · TS={s.ts_after}</span>
                  <div style={{ marginTop: 4 }}>{s.event_label || JSON.stringify(s.event_pair)}</div>
                </div>
              ))}
            </div>
            {(!result.audit.trace || result.audit.trace.length === 0) && <p style={{ color: 'var(--text-muted)' }}>{t('traceEmpty')}</p>}
            <details style={{ marginTop: 12 }}>
              <summary style={{ cursor: 'pointer', fontSize: 13 }}>{t('vizRawJson')}</summary>
              <pre style={{ margin: '8px 0 0 0', fontSize: 11, maxHeight: 200, overflow: 'auto' }}>{JSON.stringify(result.audit, null, 2)}</pre>
            </details>
            <button
              type="button"
              style={{ marginTop: 8 }}
              onClick={() => {
                const blob = new Blob([JSON.stringify(result.audit, null, 2)], { type: 'application/json' })
                const a = document.createElement('a')
                a.href = URL.createObjectURL(blob)
                a.download = `audit_trail_${Date.now()}.json`
                a.click()
                URL.revokeObjectURL(a.href)
              }}
            >
              {t('downloadAuditJson')}
            </button>
          </div>
        )}
      </div>
    </div>
  )
}
