import { useState, useEffect, useRef } from 'react'
import { useLocale } from '../context/LocaleContext'
import { getGraph, getTrajectoryMeta, kernelReset } from '../api'

function getVis() {
  return typeof window !== 'undefined' ? window.vis : null
}

/* 标签可读性：深色字 + 白底 + 描边，避免在任意节点背景上看不清 */
const LABEL_FONT = {
  color: '#1a1a1a',
  size: 14,
  face: 'Noto Sans SC',
  background: 'rgba(255,255,255,0.92)',
  strokeWidth: 2,
  strokeColor: '#333333',
}
const GROUP_STYLES = {
  atom: { color: { background: '#bbdefb', border: '#1565c0' }, shape: 'ellipse', size: 26, font: { ...LABEL_FONT } },
  composite: { color: { background: '#c8e6c9', border: '#2e7d32' }, shape: 'box', size: 30, font: { ...LABEL_FONT } },
  predicate: { color: { background: '#ffe0b2', border: '#e65100' }, shape: 'diamond', size: 26, font: { ...LABEL_FONT } },
  constructor: { color: { background: '#e1bee7', border: '#6a1b9a' }, shape: 'triangle', size: 26, font: { ...LABEL_FONT } },
  term: { color: { background: '#ffcdd2', border: '#c62828' }, shape: 'circle', size: 22, font: { ...LABEL_FONT, size: 13 } },
}
export default function KnowledgeGraph() {
  const { t } = useLocale()
  const containerRef = useRef(null)
  const networkRef = useRef(null)
  const [step, setStep] = useState(0)
  const [maxStep, setMaxStep] = useState(0)
  const [steps, setSteps] = useState([])
  const [graph, setGraph] = useState({ nodes: [], edges: [] })
  const [loading, setLoading] = useState(true)
  const [error, setError] = useState(null)
  const [visReady, setVisReady] = useState(() => !!getVis())

  const loadMeta = async () => {
    try {
      const r = await getTrajectoryMeta()
      setMaxStep(r.max_step ?? 0)
      setSteps(r.steps ?? [])
    } catch (e) {
      setError(e.message)
    }
  }

  const loadGraph = async (s) => {
    try {
      const r = await getGraph(s)
      setGraph({ nodes: r.nodes || [], edges: r.edges || [] })
      setError(null)
    } catch (e) {
      setError(e.message)
    } finally {
      setLoading(false)
    }
  }

  const handleReset = async () => {
    try {
      setLoading(true)
      setError(null)
      await kernelReset()
      setStep(0)
      await loadMeta()
      await loadGraph(0)
    } catch (e) {
      setError(e.message)
    } finally {
      setLoading(false)
    }
  }

  useEffect(() => {
    loadMeta()
  }, [])

  useEffect(() => {
    loadGraph(step)
  }, [step])

  // 等待 CDN 上的 vis-network 加载完成（可能晚于 React 模块）
  useEffect(() => {
    if (getVis()) {
      setVisReady(true)
      return
    }
    const t = setInterval(() => {
      if (getVis()) {
        setVisReady(true)
        clearInterval(t)
      }
    }, 200)
    return () => clearInterval(t)
  }, [])

  useEffect(() => {
    const vis = getVis()
    if (!containerRef.current || !vis || !visReady) return
    const nodes = new Map(
      graph.nodes.map((n) => [
        n.id,
        {
          id: n.id,
          label: n.label || n.id,
          group: n.group || 'atom',
          title: [n.title, n.description].filter(Boolean).join('\n'),
        },
      ])
    )
    const edges = graph.edges.map((e, i) => ({
      id: `e${i}`,
      from: e.from,
      to: e.to,
      color: e.color ? { color: e.color } : undefined,
      dashes: e.dashes || false,
      arrows: 'to',
    }))
    const data = {
      nodes: Array.from(nodes.values()),
      edges,
    }
    const options = {
      nodes: {
        font: {
          size: 14,
          face: 'Noto Sans SC',
          color: '#1a1a1a',
          background: 'rgba(255,255,255,0.92)',
          strokeWidth: 2,
          strokeColor: '#333333',
        },
        borderWidth: 3,
        shadow: true,
      },
      edges: {
        width: 2,
        smooth: { type: 'continuous', roundness: 0.5 },
        arrows: { to: { enabled: true, scaleFactor: 0.8 } },
        shadow: true,
      },
      groups: GROUP_STYLES,
      physics: {
        enabled: true,
        stabilization: { iterations: 150 },
        barnesHut: {
          gravitationalConstant: -2000,
          centralGravity: 0.1,
          springLength: 200,
          springConstant: 0.04,
          damping: 0.09,
        },
      },
      interaction: {
        hover: true,
        tooltipDelay: 100,
        zoomSpeed: 0.45,
      },
    }
    const net = new vis.Network(containerRef.current, data, options)
    networkRef.current = net
    return () => {
      net.destroy()
      networkRef.current = null
    }
  }, [graph, visReady])

  return (
    <div>
      <div className="card">
        <h2>{t('graphTitle')}</h2>
        <p style={{ color: 'var(--text-muted)', marginBottom: 8 }}>{t('graphIntro')}</p>
        <p style={{ color: 'var(--text-muted)', marginBottom: 16, fontSize: 13 }}>{t('graphExampleNote')}</p>
        {error && <p style={{ color: 'var(--danger)' }}>{error}</p>}
        <div style={{ display: 'flex', alignItems: 'center', gap: 12, flexWrap: 'wrap', marginBottom: 12 }}>
          <label style={{ display: 'flex', alignItems: 'center', gap: 8 }}>
            <span>{t('stateStep')}</span>
            <input
              type="range"
              min={0}
              max={maxStep}
              value={step}
              onChange={(e) => setStep(Number(e.target.value))}
              style={{ width: 200 }}
            />
            <span>{step}</span>
          </label>
          <button onClick={() => setStep(Math.max(0, step - 1))} disabled={step <= 0}>{t('commonPrev')}</button>
          <button onClick={() => setStep(Math.min(maxStep, step + 1))} disabled={step >= maxStep}>{t('commonNext')}</button>
          <button onClick={handleReset} disabled={loading}>{t('commonReset')}</button>
          <span style={{ color: 'var(--text-muted)' }}>
            {steps[step]?.event_label || (step === 0 ? t('initialState') : '')}
          </span>
        </div>
      </div>
      <div className="card" style={{ padding: 8 }}>
        <div
          ref={containerRef}
          className="kg-container"
          style={{ width: '100%', height: 600 }}
        />
        {loading && <p style={{ marginTop: 8 }}>{t('commonLoading')}</p>}
        <div style={{ marginTop: 16, fontSize: 13, color: 'var(--text-muted)' }}>
          <span style={{ display: 'inline-block', width: 20, height: 14, background: '#bbdefb', border: '2px solid #1565c0', marginRight: 6 }} /> {t('legendAtom')}
          <span style={{ display: 'inline-block', width: 20, height: 14, background: '#c8e6c9', border: '2px solid #2e7d32', marginRight: 6, marginLeft: 12 }} /> {t('legendComposite')}
          <span style={{ display: 'inline-block', width: 20, height: 14, background: '#ffe0b2', border: '2px solid #e65100', marginRight: 6, marginLeft: 12 }} /> {t('legendPredicate')}
          <span style={{ display: 'inline-block', width: 20, height: 14, background: '#e1bee7', border: '2px solid #6a1b9a', marginRight: 6, marginLeft: 12 }} /> {t('legendConstructor')}
          <span style={{ display: 'inline-block', width: 20, height: 14, background: '#ffcdd2', border: '2px solid #c62828', marginRight: 6, marginLeft: 12 }} /> {t('legendTerm')}
        </div>
      </div>
    </div>
  )
}
