import { useState, useEffect } from 'react'
import { Link } from 'react-router-dom'
import { useLocale } from '../context/LocaleContext'
import { getComplianceEnv, scenarioComplianceCheck, generateSignal } from '../api'

const cardStyle = { padding: 20, marginBottom: 16, borderRadius: 12, background: 'var(--card-bg)', border: '1px solid var(--border)' }

export default function Compliance() {
  const { t, locale } = useLocale()
  const [env, setEnv] = useState(null)
  const [loading, setLoading] = useState(null)
  const [result, setResult] = useState(null)
  const [error, setError] = useState(null)
  const [lastSignal, setLastSignal] = useState(null)

  useEffect(() => {
    let cancelled = false
    getComplianceEnv()
      .then((data) => { if (!cancelled) setEnv(data) })
      .catch((e) => { if (!cancelled) setError(e.message) })
    return () => { cancelled = true }
  }, [])

  const runCheck = async (kind) => {
    setLoading(kind)
    setError(null)
    setResult(null)
    setLastSignal(null)
    try {
      const { signal } = await generateSignal(kind)
      setLastSignal(signal)
      const data = await scenarioComplianceCheck(signal)
      setResult(data)
    } catch (e) {
      setError(e.message)
    } finally {
      setLoading(null)
    }
  }

  const isZh = locale === 'zh'
  const desc = env && (isZh ? env.description_zh : env.description_en)
  const hintCompliant = env && (isZh ? env.compliant_hint_zh : env.compliant_hint_en)
  const hintNonCompliant = env && (isZh ? env.non_compliant_hint_zh : env.non_compliant_hint_en)

  return (
    <div>
      <div className="card" style={{ marginBottom: 24 }}>
        <h2>{t('complianceEnvTitle')}</h2>
        <p style={{ color: 'var(--text-muted)', marginBottom: 0 }}>{t('complianceEnvIntro')}</p>
      </div>

      {error && <p style={{ color: 'var(--danger)', marginBottom: 16 }}>{error}</p>}

      {env && (
        <>
          <div className="card" style={cardStyle}>
            <h3 style={{ margin: '0 0 12px 0' }}>{t('complianceEnvRule')}</h3>
            <p style={{ margin: 0, fontSize: 14, color: 'var(--text-muted)' }}>{desc}</p>
            <p style={{ margin: '12px 0 0 0', fontSize: 14 }}>
              <strong>{t('complianceValidKinds')}</strong> {env.valid_kinds?.join(', ')}
            </p>
            <p style={{ margin: '8px 0 0 0', fontSize: 13, color: 'var(--text-muted)' }}>{hintCompliant}</p>
            <p style={{ margin: '8px 0 0 0', fontSize: 13, color: 'var(--text-muted)' }}>{hintNonCompliant}</p>
          </div>

          <div className="card" style={cardStyle}>
            <h3 style={{ margin: '0 0 12px 0' }}>{t('complianceBuildSignals')}</h3>
            <p style={{ color: 'var(--text-muted)', fontSize: 14, marginBottom: 12 }}>{t('complianceBuildIntro')}</p>
            <Link to="/runtime" style={{ display: 'inline-block', marginRight: 12 }}>{t('counterfactualGoRuntime')}</Link>
            <Link to="/scenarios">{t('counterfactualGoScenarios')}</Link>
          </div>
        </>
      )}

      <div className="card" style={cardStyle}>
        <h3 style={{ margin: '0 0 12px 0' }}>{t('complianceRunTitle')}</h3>
        <p style={{ color: 'var(--text-muted)', fontSize: 14, marginBottom: 16 }}>{t('complianceRunIntro')}</p>
        <div style={{ display: 'flex', gap: 8, flexWrap: 'wrap', marginBottom: 16 }}>
          {(env?.valid_kinds || []).map((k) => (
            <button
              key={k}
              className="primary"
              onClick={() => runCheck(k)}
              disabled={!!loading}
              style={{ padding: '8px 16px' }}
            >
              {loading === k ? t('commonLoading') : `${t('complianceCheckWith')} ${k}`}
            </button>
          ))}
          {(env?.invalid_kinds_example || []).slice(0, 2).map((k) => (
            <button
              key={k}
              onClick={() => runCheck(k)}
              disabled={!!loading}
              style={{ padding: '8px 16px' }}
            >
              {loading === k ? t('commonLoading') : `${t('complianceCheckWith')} ${k}`}
            </button>
          ))}
        </div>

        {result && (
          <div className="scenario-viz" style={{ marginTop: 16 }}>
            <h4 style={{ margin: '0 0 8px 0', fontSize: 14 }}>{t('vizComplianceTitle')}</h4>
            <div style={{ display: 'flex', alignItems: 'center', gap: 8, flexWrap: 'wrap', marginBottom: 8 }}>
              <span className="scenario-node proc" style={{ padding: '4px 10px', borderRadius: 6 }}>
                {lastSignal?.kind ?? 'Signal'}
              </span>
              <span>→</span>
              <span style={{ color: 'var(--text-muted)' }}>{t('vizTypeCheck')}</span>
              <span>→</span>
              <span
                className={`scenario-node ${result.compliant ? 'success' : 'fail'}`}
                style={{
                  padding: '4px 10px',
                  borderRadius: 6,
                  background: result.compliant ? 'var(--success)22' : 'var(--danger)22',
                  border: `1px solid ${result.compliant ? 'var(--success)' : 'var(--danger)'}`,
                }}
              >
                {result.compliant ? t('vizCompliant') : t('vizReject')}
              </span>
            </div>
            <p style={{ margin: 0, fontWeight: 500 }}>{result.reason}</p>
            {lastSignal && (
              <details style={{ marginTop: 12 }}>
                <summary style={{ cursor: 'pointer', fontSize: 13 }}>{t('complianceSignalJson')}</summary>
                <pre style={{ margin: '8px 0 0 0', fontSize: 11, overflow: 'auto', maxHeight: 200 }}>{JSON.stringify(lastSignal, null, 2)}</pre>
              </details>
            )}
          </div>
        )}
      </div>
    </div>
  )
}
