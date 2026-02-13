import { useState, useEffect } from 'react'
import { Link } from 'react-router-dom'
import { useLocale } from '../context/LocaleContext'
import { getGovernanceEnv, scenarioEvolveIdle } from '../api'

const cardStyle = { padding: 20, marginBottom: 16, borderRadius: 12, background: 'var(--card-bg)', border: '1px solid var(--border)' }

export default function Governance() {
  const { t, locale } = useLocale()
  const [env, setEnv] = useState(null)
  const [loading, setLoading] = useState(false)
  const [result, setResult] = useState(null)
  const [error, setError] = useState(null)

  useEffect(() => {
    let cancelled = false
    getGovernanceEnv()
      .then((data) => { if (!cancelled) setEnv(data) })
      .catch((e) => { if (!cancelled) setError(e.message) })
    return () => { cancelled = true }
  }, [])

  const onRun = async () => {
    setLoading(true)
    setError(null)
    setResult(null)
    try {
      const data = await scenarioEvolveIdle()
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
        <h2>{t('governanceEnvTitle')}</h2>
        <p style={{ color: 'var(--text-muted)', marginBottom: 0 }}>{t('governanceEnvIntro')}</p>
      </div>

      {error && <p style={{ color: 'var(--danger)', marginBottom: 16 }}>{error}</p>}

      {env && (
        <div className="card" style={cardStyle}>
          <h3 style={{ margin: '0 0 12px 0' }}>{t('governanceEnvRule')}</h3>
          <p style={{ margin: 0, fontSize: 14, color: 'var(--text-muted)' }}>{desc}</p>
          <p style={{ margin: '12px 0 0 0', fontSize: 13, color: 'var(--text-muted)' }}>{hint}</p>
          <div style={{ marginTop: 12 }}>
            <Link to="/runtime" style={{ marginRight: 12 }}>{t('counterfactualGoRuntime')}</Link>
            <Link to="/scenarios">{t('counterfactualGoScenarios')}</Link>
          </div>
        </div>
      )}

      <div className="card" style={cardStyle}>
        <h3 style={{ margin: '0 0 12px 0' }}>{t('governanceRunTitle')}</h3>
        <button className="primary" onClick={onRun} disabled={loading} style={{ padding: '10px 20px' }}>
          {loading ? t('commonLoading') : t('scenario5Run')}
        </button>

        {result && (
          <div style={{ marginTop: 20 }}>
            <h4 style={{ margin: '0 0 8px 0', fontSize: 14 }}>{t('governanceResultTitle')}</h4>
            <p><strong>{t('vizSteps')}</strong>: {result.steps ?? 0}</p>
            <p><strong>{t('governanceConsumed')}</strong>: {result.consumed_count ?? 0}</p>
            <p style={{ color: 'var(--text-muted)', fontSize: 13 }}>{t('logicClock')}: {result.state?.TS ?? 0}, {t('knowledgeCount')}: {result.state?.K?.length ?? 0}</p>
            <details style={{ marginTop: 12 }}>
              <summary style={{ cursor: 'pointer', fontSize: 13 }}>{t('vizRawJson')}</summary>
              <pre style={{ margin: '8px 0 0 0', fontSize: 11, maxHeight: 180, overflow: 'auto' }}>{JSON.stringify({ steps: result.steps, consumed_count: result.consumed_count }, null, 2)}</pre>
            </details>
          </div>
        )}
      </div>
    </div>
  )
}
