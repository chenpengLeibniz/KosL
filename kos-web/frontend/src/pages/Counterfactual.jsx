import { useState, useEffect } from 'react'
import { Link } from 'react-router-dom'
import { useLocale } from '../context/LocaleContext'
import { getCounterfactualEnv, scenarioCounterfactual } from '../api'

const cardStyle = { padding: 20, marginBottom: 16, borderRadius: 12, background: 'var(--card-bg)', border: '1px solid var(--border)' }

export default function Counterfactual() {
  const { t, locale } = useLocale()
  const [env, setEnv] = useState(null)
  const [loading, setLoading] = useState(false)
  const [result, setResult] = useState(null)
  const [error, setError] = useState(null)

  useEffect(() => {
    let cancelled = false
    getCounterfactualEnv()
      .then((data) => { if (!cancelled) setEnv(data) })
      .catch((e) => { if (!cancelled) setError(e.message) })
    return () => { cancelled = true }
  }, [])

  const onRun = async () => {
    setLoading(true)
    setError(null)
    setResult(null)
    try {
      const data = await scenarioCounterfactual()
      setResult(data)
    } catch (e) {
      setError(e.message)
    } finally {
      setLoading(false)
    }
  }

  const isZh = locale === 'zh'
  const descFactual = env?.factual && (isZh ? env.factual.description_zh : env.factual.description_en)
  const descCounter = env?.counterfactual && (isZh ? env.counterfactual.description_zh : env.counterfactual.description_en)
  const conclusion = env && (isZh ? env.conclusion_zh : env.conclusion_en)
  const hintFactual = env?.factual && (isZh ? env.factual.signal_hint_zh : env.factual.signal_hint_en)
  const hintCounter = env?.counterfactual && (isZh ? env.counterfactual.signal_hint_zh : env.counterfactual.signal_hint_en)

  return (
    <div>
      <div className="card" style={{ marginBottom: 24 }}>
        <h2>{t('counterfactualEnvTitle')}</h2>
        <p style={{ color: 'var(--text-muted)', marginBottom: 0 }}>{t('counterfactualEnvIntro')}</p>
      </div>

      {error && <p style={{ color: 'var(--danger)', marginBottom: 16 }}>{error}</p>}

      {env && (
        <>
          <div style={{ display: 'grid', gridTemplateColumns: '1fr 1fr', gap: 16, marginBottom: 24 }}>
            <div style={{ ...cardStyle, borderColor: 'var(--accent)', background: 'var(--accent)08' }}>
              <h3 style={{ margin: '0 0 12px 0', fontSize: 16 }}>{t('vizFactual')}</h3>
              <p style={{ margin: 0, fontSize: 14, color: 'var(--text-muted)' }}>{descFactual}</p>
              <p style={{ margin: '12px 0 0 0', fontSize: 13 }}><strong>{t('counterfactualGoal')}</strong> {env.goal}</p>
              <p style={{ margin: '8px 0 0 0', fontSize: 12, color: 'var(--text-muted)' }}>
                {t('counterfactualSignals')}: {env.factual.signals?.join(' → ')}
              </p>
              {hintFactual && <p style={{ margin: '8px 0 0 0', fontSize: 12 }}>{hintFactual}</p>}
            </div>
            <div style={{ ...cardStyle, borderColor: 'var(--danger)', background: 'var(--danger)08' }}>
              <h3 style={{ margin: '0 0 12px 0', fontSize: 16 }}>{t('vizCounterfactual')}</h3>
              <p style={{ margin: 0, fontSize: 14, color: 'var(--text-muted)' }}>{descCounter}</p>
              <p style={{ margin: '12px 0 0 0', fontSize: 13 }}><strong>{t('counterfactualGoal')}</strong> {env.goal}</p>
              <p style={{ margin: '8px 0 0 0', fontSize: 12, color: 'var(--text-muted)' }}>
                {t('counterfactualSignals')}: {env.counterfactual.signals?.join(' → ')}
              </p>
              {hintCounter && <p style={{ margin: '8px 0 0 0', fontSize: 12 }}>{hintCounter}</p>}
            </div>
          </div>

          <div className="card" style={{ marginBottom: 24 }}>
            <h3 style={{ margin: '0 0 8px 0' }}>{t('counterfactualBuildSignals')}</h3>
            <p style={{ color: 'var(--text-muted)', fontSize: 14, marginBottom: 12 }}>
              {t('counterfactualBuildSignalsIntro')}
            </p>
            <ul style={{ margin: '0 0 12px 0', paddingLeft: 20 }}>
              <li>{t('counterfactualBuildFactual')}</li>
              <li>{t('counterfactualBuildCounter')}</li>
            </ul>
            <Link to="/runtime" className="primary" style={{ display: 'inline-block', padding: '8px 16px', borderRadius: 8 }}>{t('counterfactualGoRuntime')}</Link>
            <span style={{ marginLeft: 12 }}><Link to="/scenarios">{t('counterfactualGoScenarios')}</Link></span>
          </div>
        </>
      )}

      <div className="card" style={cardStyle}>
        <h3 style={{ margin: '0 0 12px 0' }}>{t('counterfactualRunTitle')}</h3>
        <p style={{ color: 'var(--text-muted)', fontSize: 14, marginBottom: 16 }}>{conclusion}</p>
        <button className="primary" onClick={onRun} disabled={loading} style={{ padding: '10px 20px' }}>
          {loading ? t('commonLoading') : t('scenario2Run')}
        </button>

        {result && (
          <div className="scenario-viz" style={{ marginTop: 20 }}>
            <h4 style={{ margin: '0 0 12px 0', fontSize: 14 }}>{t('vizCounterfactualTitle')}</h4>
            <div style={{ display: 'grid', gridTemplateColumns: '1fr 1fr', gap: 12 }}>
              <div className="scenario-panel factual" style={{ padding: 16, borderRadius: 8, background: 'var(--success)11', border: '1px solid var(--success)' }}>
                <strong>{t('vizFactual')}</strong>
                <p style={{ margin: '8px 0 0 0', fontSize: 13 }}>ctx 含 anomalyEx</p>
                <p style={{ margin: 8, color: 'var(--success)', fontWeight: 600 }}>
                  RootCauseReport {result.factual_provable ? '✓ 可证' : '✗ 不可证'}
                </p>
                {result.factual_output && <pre style={{ marginTop: 8, fontSize: 11, overflow: 'auto', maxHeight: 80 }}>{result.factual_output}</pre>}
              </div>
              <div className="scenario-panel counterfactual" style={{ padding: 16, borderRadius: 8, background: 'var(--danger)11', border: '1px solid var(--danger)' }}>
                <strong>{t('vizCounterfactual')}</strong>
                <p style={{ margin: '8px 0 0 0', fontSize: 13 }}>ctx 排除 anomalyEx</p>
                <p style={{ margin: 8, color: 'var(--danger)', fontWeight: 600 }}>
                  RootCauseReport {result.counterfactual_provable ? '✓ 可证' : '✗ 不可证'}
                </p>
                {result.counterfactual_output && <pre style={{ marginTop: 8, fontSize: 11, overflow: 'auto', maxHeight: 80 }}>{result.counterfactual_output}</pre>}
              </div>
            </div>
            <div style={{ marginTop: 16, padding: 12, background: 'var(--bg)', borderRadius: 8, borderLeft: '4px solid var(--accent)' }}>
              <strong>{result.excluded_necessary ? t('vizNecessary') : t('vizNotNecessary')}</strong>
              <p style={{ margin: '8px 0 0 0', fontWeight: 600 }}>{result.summary}</p>
              {result.simulated && <span style={{ fontSize: 12, color: 'var(--text-muted)' }}> [模拟]</span>}
            </div>
          </div>
        )}
      </div>
    </div>
  )
}
