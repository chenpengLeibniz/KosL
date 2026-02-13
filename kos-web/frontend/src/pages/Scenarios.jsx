import { useState } from 'react'
import { useLocale } from '../context/LocaleContext'
import {
  scenarioRootCause,
  scenarioCounterfactual,
  scenarioAudit,
  scenarioEvolveIdle,
  scenarioVerifyAi,
  scenarioComplianceCheck,
  generateSignal,
} from '../api'

const cardStyle = { padding: 16, marginBottom: 12, borderRadius: 8, background: 'var(--card-bg)', border: '1px solid var(--border)' }
const btnStyle = { padding: '8px 16px', marginRight: 8, marginBottom: 8 }

export default function Scenarios() {
  const { t } = useLocale()
  const [loading, setLoading] = useState(null)
  const [result, setResult] = useState(null)
  const [error, setError] = useState(null)

  const run = async (key, fn) => {
    setLoading(key)
    setError(null)
    setResult(null)
    try {
      const r = await fn()
      setResult({ key, data: r })
    } catch (e) {
      setError(e.message)
    } finally {
      setLoading(null)
    }
  }

  const onRootCause = () => run('root_cause', scenarioRootCause)
  const onCounterfactual = () => run('counterfactual', scenarioCounterfactual)
  const onAudit = () => run('audit', scenarioAudit)
  const onEvolveIdle = () => run('evolve_idle', scenarioEvolveIdle)
  const onVerifyAi = () => run('verify_ai', () => scenarioVerifyAi('Prop P', 'Prop P'))
  const onCompliance = async () => {
    setLoading('compliance')
    setError(null)
    setResult(null)
    try {
      const { signal } = await generateSignal('proc_step')
      const r = await scenarioComplianceCheck(signal)
      setResult({ key: 'compliance', data: r, signal })
    } catch (e) {
      setError(e.message)
    } finally {
      setLoading(null)
    }
  }

  return (
    <div>
      <div className="card" style={{ marginBottom: 24 }}>
        <h2>{t('scenariosTitle')}</h2>
        <p style={{ color: 'var(--text-muted)', marginBottom: 0 }}>{t('scenariosIntro')}</p>
      </div>

      {error && <p style={{ color: 'var(--danger)', marginBottom: 16 }}>{error}</p>}

      <div style={{ display: 'grid', gap: 16 }}>
        {/* 1. 根因追溯 */}
        <div style={cardStyle}>
          <h3 style={{ marginTop: 0 }}>{t('scenario1Title')}</h3>
          <p style={{ color: 'var(--text-muted)', fontSize: 14 }}>{t('scenario1Desc')}</p>
          <button className="primary" onClick={onRootCause} disabled={!!loading} style={btnStyle}>
            {loading === 'root_cause' ? t('commonLoading') : t('scenario1Run')}
          </button>
          {result?.key === 'root_cause' && (
            <div className="scenario-viz">
              <h4 style={{ margin: '0 0 8px 0', fontSize: 14 }}>{t('vizRootCauseTitle')}</h4>
              <div className="scenario-flow">
                <span className="scenario-node proc">ProcStep 08:00-09:30 M_03</span>
                <span className="scenario-arrow">→</span>
                <span className="scenario-node anomaly">{t('vizAnomaly')} 08:15 Voltage_Drop M_03</span>
                <span className="scenario-arrow">→</span>
                <span className="scenario-node fail">FailEvt 10:00 HARD_ERR</span>
              </div>
              <div style={{ marginTop: 12 }}>
                <div style={{ fontSize: 12, color: 'var(--text-muted)', marginBottom: 4 }}>{t('vizTimeline')}</div>
                <div className="scenario-timeline">
                  <span>08:00</span>
                  <span className="dot" style={{ background: 'var(--accent)' }} />
                  <span>08:15</span>
                  <span className="dot" style={{ background: 'var(--warn)' }} />
                  <span>09:30</span>
                  <span className="dot" style={{ background: 'var(--text-muted)' }} />
                  <span>10:00</span>
                  <span className="dot" style={{ background: 'var(--danger)' }} />
                </div>
              </div>
              <p style={{ margin: '12px 0 0 0', fontWeight: 600 }}>{result.data.summary}</p>
              {result.data.proof_term && (
                <pre style={{ margin: '8px 0 0 0', fontSize: 11, overflow: 'auto' }}>{result.data.proof_term}</pre>
              )}
            </div>
          )}
        </div>

        {/* 2. 反事实推理 */}
        <div style={cardStyle}>
          <h3 style={{ marginTop: 0 }}>{t('scenario2Title')}</h3>
          <p style={{ color: 'var(--text-muted)', fontSize: 14 }}>{t('scenario2Desc')}</p>
          <button className="primary" onClick={onCounterfactual} disabled={!!loading} style={btnStyle}>
            {loading === 'counterfactual' ? t('commonLoading') : t('scenario2Run')}
          </button>
          {result?.key === 'counterfactual' && (
            <div className="scenario-viz">
              <h4 style={{ margin: '0 0 8px 0', fontSize: 14 }}>{t('vizCounterfactualTitle')}</h4>
              <div className="scenario-sidebyside">
                <div className="scenario-panel factual">
                  <strong>{t('vizFactual')}</strong>
                  <p style={{ margin: '8px 0 0 0', fontSize: 13 }}>ctx 含 anomalyEx</p>
                  <p style={{ margin: 8, color: 'var(--success)', fontWeight: 600 }}>
                    RootCauseReport {result.data.factual_provable ? '✓ 可证' : '✗ 不可证'}
                  </p>
                </div>
                <div className="scenario-panel counterfactual">
                  <strong>{t('vizCounterfactual')}</strong>
                  <p style={{ margin: '8px 0 0 0', fontSize: 13 }}>ctx 排除 anomalyEx</p>
                  <p style={{ margin: 8, color: 'var(--danger)', fontWeight: 600 }}>
                    RootCauseReport {result.data.counterfactual_provable ? '✓ 可证' : '✗ 不可证'}
                  </p>
                </div>
              </div>
              <div className="scenario-flow" style={{ marginTop: 16 }}>
                <span className="scenario-node" style={{ background: 'var(--success)22', border: '1px solid var(--success)' }}>
                  {result.data.excluded_necessary ? t('vizNecessary') : t('vizNotNecessary')}
                </span>
              </div>
              <p style={{ margin: '8px 0 0 0', fontWeight: 600 }}>{result.data.summary}</p>
            </div>
          )}
        </div>

        {/* 3. 合规性决策 */}
        <div style={cardStyle}>
          <h3 style={{ marginTop: 0 }}>{t('scenario3Title')}</h3>
          <p style={{ color: 'var(--text-muted)', fontSize: 14 }}>{t('scenario3Desc')}</p>
          <button className="primary" onClick={onCompliance} disabled={!!loading} style={btnStyle}>
            {loading === 'compliance' ? t('commonLoading') : t('scenario3Run')}
          </button>
          {result?.key === 'compliance' && (
            <div className="scenario-viz">
              <h4 style={{ margin: '0 0 8px 0', fontSize: 14 }}>{t('vizComplianceTitle')}</h4>
              <div className="scenario-flow">
                <span className="scenario-node proc">{result.signal?.kind || 'Signal'}</span>
                <span className="scenario-arrow">→</span>
                <span>{t('vizTypeCheck')}</span>
                <span className="scenario-arrow">→</span>
                <span className={`scenario-node ${result.data.compliant ? 'success' : 'fail'}`}>
                  {result.data.compliant ? t('vizCompliant') : t('vizReject')}
                </span>
              </div>
              <p style={{ margin: '8px 0 0 0' }}>{result.data.reason}</p>
              {result.signal && (
                <pre style={{ margin: '8px 0 0 0', fontSize: 11 }}>{JSON.stringify(result.signal, null, 2)}</pre>
              )}
            </div>
          )}
        </div>

        {/* 4. 审计与问责 */}
        <div style={cardStyle}>
          <h3 style={{ marginTop: 0 }}>{t('scenario4Title')}</h3>
          <p style={{ color: 'var(--text-muted)', fontSize: 14 }}>{t('scenario4Desc')}</p>
          <button className="primary" onClick={onAudit} disabled={!!loading} style={btnStyle}>
            {loading === 'audit' ? t('commonLoading') : t('scenario4Run')}
          </button>
          {result?.key === 'audit' && (
            <div className="scenario-viz">
              <h4 style={{ margin: '0 0 8px 0', fontSize: 14 }}>{t('vizAuditTitle')}</h4>
              <div className="scenario-audit-timeline">
                {(result.data.audit?.trace || []).map((s, i) => (
                  <div key={i} className="scenario-audit-step">
                    <span style={{ color: 'var(--text-muted)', fontSize: 12 }}>
                      {t('step')} {s.step_index} · TS={s.ts_after}
                    </span>
                    <div style={{ marginTop: 4 }}>{s.event_label || JSON.stringify(s.event_pair)}</div>
                  </div>
                ))}
              </div>
              {(!result.data.audit?.trace || result.data.audit.trace.length === 0) && (
                <p style={{ color: 'var(--text-muted)' }}>{t('traceEmpty')}</p>
              )}
              <details style={{ marginTop: 12 }}>
                <summary style={{ cursor: 'pointer', fontSize: 13 }}>{t('vizRawJson')}</summary>
                <pre style={{ margin: '8px 0 0 0', fontSize: 11, maxHeight: 200, overflow: 'auto' }}>
                  {JSON.stringify(result.data.audit, null, 2)}
                </pre>
              </details>
              <button
                type="button"
                style={{ marginTop: 8 }}
                onClick={() => {
                  const blob = new Blob([JSON.stringify(result.data.audit, null, 2)], { type: 'application/json' })
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

        {/* 5. 复杂系统治理 */}
        <div style={cardStyle}>
          <h3 style={{ marginTop: 0 }}>{t('scenario5Title')}</h3>
          <p style={{ color: 'var(--text-muted)', fontSize: 14 }}>{t('scenario5Desc')}</p>
          <button className="primary" onClick={onEvolveIdle} disabled={!!loading} style={btnStyle}>
            {loading === 'evolve_idle' ? t('commonLoading') : t('scenario5Run')}
          </button>
          {result?.key === 'evolve_idle' && (
            <div className="scenario-viz">
              <h4 style={{ margin: '0 0 8px 0', fontSize: 14 }}>{t('vizEvolveTitle')}</h4>
              <div className="scenario-flow">
                <span>{t('vizSignalsQueue')}</span>
                <span className="scenario-arrow">→</span>
                <span className="scenario-node proc">evolve_until_idle</span>
                <span className="scenario-arrow">→</span>
                <span className="scenario-node success">
                  {result.data.steps} {t('vizSteps')} · {result.data.consumed_count} {t('vizConsumed')}
                </span>
              </div>
              <div style={{ marginTop: 12, fontSize: 13 }}>
                <span>{t('logicClock')}: {result.data.state?.TS ?? 0}</span>
                <span style={{ marginLeft: 16 }}>{t('knowledgeCount')}: {result.data.state?.K?.length ?? 0}</span>
              </div>
              {result.data.state?.trace?.length > 0 && (
                <details style={{ marginTop: 8 }}>
                  <summary style={{ cursor: 'pointer', fontSize: 13 }}>{t('traceRecent')}</summary>
                  <ul style={{ listStyle: 'none', padding: 0, margin: '8px 0 0 0', fontSize: 12 }}>
                    {result.data.state.trace.slice(-6).map((s, i) => (
                      <li key={i} style={{ padding: '4px 0', borderBottom: '1px solid var(--border)' }}>
                        {t('step')} {s.step_index} TS={s.ts_after} {s.event_label}
                      </li>
                    ))}
                  </ul>
                </details>
              )}
            </div>
          )}
        </div>

        {/* 6. AI 治理 */}
        <div style={cardStyle}>
          <h3 style={{ marginTop: 0 }}>{t('scenario6Title')}</h3>
          <p style={{ color: 'var(--text-muted)', fontSize: 14 }}>{t('scenario6Desc')}</p>
          <button className="primary" onClick={onVerifyAi} disabled={!!loading} style={btnStyle}>
            {loading === 'verify_ai' ? t('commonLoading') : t('scenario6Run')}
          </button>
          {result?.key === 'verify_ai' && (
            <div className="scenario-viz">
              <h4 style={{ margin: '0 0 8px 0', fontSize: 14 }}>{t('vizAiTitle')}</h4>
              <div className="scenario-flow-box">
                <code>Prop P</code>
                <span>:</span>
                <code>Prop P</code>
                <span className="scenario-arrow">→</span>
                <span>check-term</span>
                <span className="scenario-arrow">→</span>
                <span className={`scenario-node ${result.data.compliant ? 'success' : 'fail'}`}>
                  {result.data.compliant ? t('vizCompliant') : t('vizReject')}
                </span>
              </div>
              <p style={{ margin: '8px 0 0 0' }}>{result.data.output || result.data.error}</p>
            </div>
          )}
        </div>
      </div>
    </div>
  )
}
