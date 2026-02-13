import { useState, useEffect } from 'react'
import { Link } from 'react-router-dom'
import { useLocale } from '../context/LocaleContext'
import { getCoreTypes, getKernelState, getSignalsLatest, runtimeLogList } from '../api'

export default function Dashboard() {
  const { t } = useLocale()
  const [types, setTypes] = useState([])
  const [state, setState] = useState(null)
  const [signals, setSignals] = useState([])
  const [logsCount, setLogsCount] = useState(0)
  const [loading, setLoading] = useState(true)

  useEffect(() => {
    let cancelled = false
    async function load() {
      try {
        const [tRes, sRes, sigRes, logRes] = await Promise.all([
          getCoreTypes(),
          getKernelState(),
          getSignalsLatest(5),
          runtimeLogList().catch(() => ({ logs: [] })),
        ])
        if (!cancelled) {
          setTypes(tRes.types || [])
          setState(sRes)
          setSignals(sigRes.signals || [])
          setLogsCount((logRes.logs || []).length)
        }
      } catch (e) {
        if (!cancelled) console.error(e)
      } finally {
        if (!cancelled) setLoading(false)
      }
    }
    load()
    return () => { cancelled = true }
  }, [])

  if (loading) return <div className="card">{t('commonLoading')}</div>

  return (
    <div>
      <div className="card">
        <h2>{t('dashboardTitle')}</h2>
        <p style={{ color: 'var(--text-muted)', marginBottom: 20 }}>{t('dashboardIntro')}</p>
        {t('dashboardRecommendedFlow') && (
          <p style={{ marginBottom: 16, padding: 12, background: 'var(--bg)', borderRadius: 8, fontSize: 14 }}>
            {t('dashboardRecommendedFlow')}
          </p>
        )}
        <div className="grid2">
          <div className="card">
            <h2>{t('dashboardCore')}</h2>
            <p>{t('typesCount')}: <strong>{types.length}</strong></p>
            <p style={{ marginTop: 8 }}>
              <Link to="/core">{t('viewTypesPredicates')}</Link>
              {' · '}
              <Link to="/core-load">{t('navCoreLoad')}</Link>
            </p>
          </div>
          <div className="card">
            <h2>{t('dashboardRuntime')}</h2>
            <p>{t('signalsCount')}: <strong>{signals.length}</strong> · {t('dashboardLogsCount')}: <strong>{logsCount}</strong></p>
            <Link to="/runtime">{t('sensorSim')}</Link>
          </div>
          <div className="card">
            <h2>{t('dashboardKernel')}</h2>
            <p>{t('logicClock')}: <strong>{state?.TS ?? '-'}</strong>, {t('knowledgeCount')}: <strong>{state?.K?.length ?? 0}</strong>, {t('traceSteps')}: <strong>{state?.trace_length ?? 0}</strong>, {t('queueLength')}: <strong>{state?.queue_length ?? 0}</strong></p>
            <Link to="/kernel">{t('stateEvolve')}</Link>
          </div>
          <div className="card">
            <h2>{t('dashboardView')}</h2>
            <p><Link to="/graph">{t('knowledgeGraph')}</Link> · <Link to="/trace">{t('trace')}</Link></p>
          </div>
          <div className="card">
            <h2>{t('navScenarios')}</h2>
            <p>{t('scenariosTitle')}</p>
            <Link to="/scenarios">{t('scenario1Run')} / … →</Link>
          </div>
        </div>
      </div>
    </div>
  )
}
