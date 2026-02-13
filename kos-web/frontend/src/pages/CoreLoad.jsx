import { useState, useRef } from 'react'
import { Link } from 'react-router-dom'
import { useLocale } from '../context/LocaleContext'
import { loadKos, loadKosFile, clearCoreDynamic } from '../api'

const KOS_EXAMPLE = `-- 示例：KOS 格式类型与谓词（可修改后加载）
type ID : U0
type TIME : U0
type BatchID : Type
FailEvt : Σ(b:BatchID).Σ(err:ErrorCode).Σ(t:TIME).Proof(t∈Shift_QA)
predicate InRoute(b, m) : 批次b是否允许在机器m上加工
pred TimeOK(a,e,f) : (a.t∈e.dur)∧(e.dur.end<f.t)
`

export default function CoreLoad() {
  const { t } = useLocale()
  const [kosSource, setKosSource] = useState(KOS_EXAMPLE)
  const [result, setResult] = useState(null)
  const [error, setError] = useState(null)
  const [busy, setBusy] = useState(false)
  const fileInputRef = useRef(null)

  const onSubmit = async () => {
    setBusy(true)
    setError(null)
    setResult(null)
    try {
      const r = await loadKos(kosSource)
      setResult(r)
    } catch (e) {
      setError(e.message)
    } finally {
      setBusy(false)
    }
  }

  const onFileSelect = async (e) => {
    const file = e.target.files?.[0]
    if (!file) return
    setBusy(true)
    setError(null)
    setResult(null)
    try {
      const r = await loadKosFile(file)
      setResult(r)
      setKosSource((prev) => (prev ? prev + '\n\n' : '') + '-- 已从文件加载: ' + file.name + '\n-- 类型 ' + (r.added?.types ?? 0) + ' 个, 谓词 ' + (r.added?.predicates ?? 0) + ' 个, 构造函数 ' + (r.added?.constructors ?? 0) + ' 个')
    } catch (e) {
      setError(e.message || String(e))
    } finally {
      setBusy(false)
    }
    e.target.value = ''
  }

  const onClear = async () => {
    setBusy(true)
    setError(null)
    setResult(null)
    try {
      await clearCoreDynamic()
      setResult({ ok: true, message: t('clearedDynamic') })
    } catch (e) {
      setError(e.message)
    } finally {
      setBusy(false)
    }
  }

  return (
    <div>
      <div className="card">
        <h2>{t('coreLoadTitle')}</h2>
        <p style={{ color: 'var(--text-muted)', marginBottom: 16 }}>{t('coreLoadIntro')}</p>
        <p style={{ fontSize: 13, color: 'var(--text-muted)', marginBottom: 12 }}>
          {t('coreLoadFormat')}<code style={{ background: 'var(--bg)', padding: '2px 6px', borderRadius: 4 }}>type Id : U0</code>、
          <code style={{ background: 'var(--bg)', padding: '2px 6px', borderRadius: 4, marginLeft: 4 }}>Name : TypeExpr</code>、
          <code style={{ background: 'var(--bg)', padding: '2px 6px', borderRadius: 4, marginLeft: 4 }}>predicate Name(args) : …</code>
        </p>
        {error && <p style={{ color: 'var(--danger)', marginBottom: 12 }}>{error}</p>}
        {result && result.ok && (
          <p style={{ color: 'var(--success)', marginBottom: 12 }}>
            {t('loadSuccess', { types: result.added?.types ?? 0, predicates: result.added?.predicates ?? 0, constructors: result.added?.constructors ?? 0 })}
            {result.message && ` ${result.message}`}
            {' '}<Link to="/core">{t('viewCoreTypes')}</Link> {t('coreSyncKernel')}
          </p>
        )}
        <div style={{ display: 'flex', gap: 12, marginBottom: 12, flexWrap: 'wrap', alignItems: 'center' }}>
          <input type="file" ref={fileInputRef} accept=".kos,.txt" onChange={onFileSelect} style={{ display: 'none' }} />
          <button type="button" onClick={() => fileInputRef.current?.click()} disabled={busy} className="primary">{t('importKosFile')}</button>
          <span style={{ fontSize: 13, color: 'var(--text-muted)' }}>{t('importKosFileHint')}</span>
        </div>
        <textarea
          value={kosSource}
          onChange={(e) => setKosSource(e.target.value)}
          placeholder={t('coreLoadPlaceholder')}
          style={{
            width: '100%',
            minHeight: 220,
            fontFamily: 'var(--font-mono)',
            fontSize: 13,
            padding: 12,
            borderRadius: 8,
            background: 'var(--bg)',
            border: '1px solid var(--border)',
            color: 'var(--text)',
            resize: 'vertical',
          }}
        />
        <div style={{ display: 'flex', gap: 12, marginTop: 16, flexWrap: 'wrap' }}>
          <button onClick={onSubmit} disabled={busy} className="primary">{t('loadToCore')}</button>
          <button onClick={onClear} disabled={busy}>{t('clearDynamic')}</button>
          <Link to="/core"><button type="button">{t('viewCoreTypes')}</button></Link>
        </div>
      </div>
    </div>
  )
}
