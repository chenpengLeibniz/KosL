import { useState, useEffect } from 'react'
import { Link } from 'react-router-dom'
import { useLocale } from '../context/LocaleContext'
import { getCoreTypes, getCorePredicates, getCoreConstructors } from '../api'

export default function Core() {
  const { t } = useLocale()
  const [types, setTypes] = useState([])
  const [predicates, setPredicates] = useState([])
  const [constructors, setConstructors] = useState([])
  const [loading, setLoading] = useState(true)

  const load = async () => {
    setLoading(true)
    try {
      const [tr, pr, cr] = await Promise.all([
        getCoreTypes(),
        getCorePredicates(),
        getCoreConstructors(),
      ])
      setTypes(tr.types || [])
      setPredicates(pr.predicates || [])
      setConstructors(cr.constructors || [])
    } catch (e) {
      console.error(e)
    } finally {
      setLoading(false)
    }
  }

  useEffect(() => {
    load()
  }, [])

  if (loading) return <div className="card">{t('commonLoading')}</div>

  return (
    <div>
      <div className="card">
        <h2>{t('coreTitle')}</h2>
        <p style={{ color: 'var(--text-muted)', marginBottom: 16 }}>{t('coreIntro')}</p>
        <p style={{ fontSize: 13, color: 'var(--text-muted)', marginBottom: 12 }}>
          {t('coreNoPreload')} <Link to="/core-load">{t('navCoreLoad')}</Link> {t('coreNoPreload2')}
        </p>
        <button type="button" onClick={load} style={{ marginRight: 8 }}>{t('refresh')}</button>
        <Link to="/core-load"><button type="button" className="primary">{t('loadToCore')}</button></Link>
      </div>
      <div className="card">
        <h2>{t('typesInGamma')}</h2>
        {types.length === 0 ? (
          <p style={{ color: 'var(--text-muted)' }}>{t('coreEmptyTypes')}</p>
        ) : (
        <ul style={{ listStyle: 'none', padding: 0, margin: 0 }}>
          {types.map((x) => (
            <li key={x.id} style={{ padding: '8px 0', borderBottom: '1px solid var(--border)' }}>
              <strong>{x.name}</strong>
              {x.category && <span style={{ marginLeft: 8, color: 'var(--text-muted)' }}>({x.category})</span>}
              {x.definition && <div style={{ marginTop: 4, fontFamily: 'var(--font-mono)', fontSize: 13 }}>{x.definition}</div>}
              {x.description && <div style={{ marginTop: 4, color: 'var(--text-muted)' }}>{x.description}</div>}
            </li>
          ))}
        </ul>
        )}
      </div>
      <div className="card">
        <h2>{t('predicates')}</h2>
        {predicates.length === 0 ? (
          <p style={{ color: 'var(--text-muted)' }}>{t('coreEmptyPredicates')}</p>
        ) : (
        <ul style={{ listStyle: 'none', padding: 0, margin: 0 }}>
          {predicates.map((x) => (
            <li key={x.id} style={{ padding: '8px 0', borderBottom: '1px solid var(--border)' }}>
              <strong>{x.name}</strong>
              {x.description && <span style={{ marginLeft: 8, color: 'var(--text-muted)' }}> — {x.description}</span>}
            </li>
          ))}
        </ul>
        )}
      </div>
      <div className="card">
        <h2>{t('constructors')}</h2>
        {constructors.length === 0 ? (
          <p style={{ color: 'var(--text-muted)' }}>{t('coreEmptyConstructors')}</p>
        ) : (
        <ul style={{ listStyle: 'none', padding: 0, margin: 0 }}>
          {constructors.map((x) => (
            <li key={x.id} style={{ padding: '8px 0', borderBottom: '1px solid var(--border)' }}>
              <strong>{x.name}</strong> → {x.target}
              {x.signature && <div style={{ marginTop: 4, fontFamily: 'var(--font-mono)', fontSize: 12 }}>{x.signature}</div>}
            </li>
          ))}
        </ul>
        )}
      </div>
    </div>
  )
}
