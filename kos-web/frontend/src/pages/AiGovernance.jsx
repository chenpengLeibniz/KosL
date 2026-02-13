import { useState, useEffect } from 'react'
import { Link } from 'react-router-dom'
import { useLocale } from '../context/LocaleContext'
import { getAiGovernanceEnv, scenarioVerifyAi } from '../api'

const cardStyle = { padding: 20, marginBottom: 16, borderRadius: 12, background: 'var(--card-bg)', border: '1px solid var(--border)' }

export default function AiGovernance() {
  const { t, locale } = useLocale()
  const [env, setEnv] = useState(null)
  const [loading, setLoading] = useState(false)
  const [result, setResult] = useState(null)
  const [error, setError] = useState(null)
  const [termExpr, setTermExpr] = useState('Prop P')
  const [typeExpr, setTypeExpr] = useState('Prop P')

  useEffect(() => {
    let cancelled = false
    getAiGovernanceEnv()
      .then((data) => { if (!cancelled) { setEnv(data); setTermExpr(data.default_term ?? 'Prop P'); setTypeExpr(data.default_type ?? 'Prop P') } })
      .catch((e) => { if (!cancelled) setError(e.message) })
    return () => { cancelled = true }
  }, [])

  const onRun = async () => {
    setLoading(true)
    setError(null)
    setResult(null)
    try {
      const data = await scenarioVerifyAi(termExpr, typeExpr)
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
        <h2>{t('aiGovernanceEnvTitle')}</h2>
        <p style={{ color: 'var(--text-muted)', marginBottom: 0 }}>{t('aiGovernanceEnvIntro')}</p>
      </div>

      {error && <p style={{ color: 'var(--danger)', marginBottom: 16 }}>{error}</p>}

      {env && (
        <div className="card" style={cardStyle}>
          <h3 style={{ margin: '0 0 12px 0' }}>{t('aiGovernanceEnvRule')}</h3>
          <p style={{ margin: 0, fontSize: 14, color: 'var(--text-muted)' }}>{desc}</p>
          <p style={{ margin: '12px 0 0 0', fontSize: 13, color: 'var(--text-muted)' }}>{hint}</p>
          <Link to="/scenarios" style={{ display: 'inline-block', marginTop: 8 }}>{t('counterfactualGoScenarios')}</Link>
        </div>
      )}

      <div className="card" style={cardStyle}>
        <h3 style={{ margin: '0 0 12px 0' }}>{t('aiGovernanceRunTitle')}</h3>
        <div style={{ marginBottom: 12 }}>
          <label style={{ display: 'block', marginBottom: 4, fontSize: 13 }}>term</label>
          <input type="text" value={termExpr} onChange={(e) => setTermExpr(e.target.value)} style={{ width: '100%', maxWidth: 400, padding: '6px 10px', fontFamily: 'var(--font-mono)' }} />
        </div>
        <div style={{ marginBottom: 12 }}>
          <label style={{ display: 'block', marginBottom: 4, fontSize: 13 }}>type</label>
          <input type="text" value={typeExpr} onChange={(e) => setTypeExpr(e.target.value)} style={{ width: '100%', maxWidth: 400, padding: '6px 10px', fontFamily: 'var(--font-mono)' }} />
        </div>
        <button className="primary" onClick={onRun} disabled={loading} style={{ padding: '10px 20px' }}>
          {loading ? t('commonLoading') : t('scenario6Run')}
        </button>

        {result && (
          <div style={{ marginTop: 20 }}>
            <h4 style={{ margin: '0 0 8px 0', fontSize: 14 }}>{t('aiGovernanceResultTitle')}</h4>
            <p>
              <strong>{result.compliant ? t('vizCompliant') : t('vizReject')}</strong>
              {result.simulated && <span style={{ fontSize: 12, color: 'var(--text-muted)' }}> [模拟]</span>}
            </p>
            {(result.output || result.error) && <pre style={{ margin: '8px 0 0 0', fontSize: 12, overflow: 'auto' }}>{result.output || result.error}</pre>}
          </div>
        )}
      </div>
    </div>
  )
}
