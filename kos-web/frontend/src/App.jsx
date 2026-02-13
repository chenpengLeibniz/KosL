import { useState } from 'react'
import { Routes, Route, NavLink } from 'react-router-dom'
import { useLocale } from './context/LocaleContext'
import { useTheme } from './context/ThemeContext'
import Dashboard from './pages/Dashboard'
import Core from './pages/Core'
import CoreLoad from './pages/CoreLoad'
import Runtime from './pages/Runtime'
import Kernel from './pages/Kernel'
import KnowledgeGraph from './pages/KnowledgeGraph'
import Trace from './pages/Trace'
import Scenarios from './pages/Scenarios'
import Counterfactual from './pages/Counterfactual'
import Compliance from './pages/Compliance'
import Audit from './pages/Audit'
import Governance from './pages/Governance'
import AiGovernance from './pages/AiGovernance'

function NavDropdown({ label, children }) {
  const [open, setOpen] = useState(false)
  return (
    <div className="nav-dropdown" onMouseEnter={() => setOpen(true)} onMouseLeave={() => setOpen(false)}>
      <button type="button" className="nav-dropdown-trigger" onClick={() => setOpen(!open)}>{label} ▾</button>
      {open && <div className="nav-dropdown-menu" onClick={() => setOpen(false)}>{children}</div>}
    </div>
  )
}

export default function App() {
  const { locale, setLocale, t } = useLocale()
  const { theme, setTheme } = useTheme()
  return (
    <div className="app">
      <header className="app-header">
        <h1>{t('appTitle')}</h1>
        <nav>
          <NavLink to="/" end className={({ isActive }) => isActive ? 'active' : ''}>{t('navOverview')}</NavLink>
          <NavDropdown label={t('navCore')}>
            <NavLink to="/core" className={({ isActive }) => isActive ? 'active' : ''}>{t('navCoreSubTypes')}</NavLink>
            <NavLink to="/core-load" className={({ isActive }) => isActive ? 'active' : ''}>{t('navCoreSubLoad')}</NavLink>
          </NavDropdown>
          <NavLink to="/runtime" className={({ isActive }) => isActive ? 'active' : ''}>{t('navRuntime')}</NavLink>
          <NavLink to="/kernel" className={({ isActive }) => isActive ? 'active' : ''}>{t('navKernel')}</NavLink>
          <NavLink to="/graph" className={({ isActive }) => isActive ? 'active' : ''}>{t('navGraph')}</NavLink>
          <NavLink to="/trace" className={({ isActive }) => isActive ? 'active' : ''}>{t('navTrace')}</NavLink>
          <NavDropdown label={t('navScenarios')}>
            <NavLink to="/scenarios">{t('navScenariosSubOverview')}</NavLink>
            <NavLink to="/counterfactual">{t('navCounterfactual')}</NavLink>
            <NavLink to="/compliance">{t('navCompliance')}</NavLink>
            <NavLink to="/audit">{t('navScenariosSubAudit')}</NavLink>
            <NavLink to="/governance">{t('navScenariosSubGovernance')}</NavLink>
            <NavLink to="/ai-governance">{t('navScenariosSubAiGovernance')}</NavLink>
          </NavDropdown>
        </nav>
        <div className="app-header-actions">
          <span>
            <button type="button" className={locale === 'zh' ? 'active' : ''} onClick={() => setLocale('zh')}>{t('langZh')}</button>
            <button type="button" className={locale === 'en' ? 'active' : ''} onClick={() => setLocale('en')}>{t('langEn')}</button>
          </span>
          <span>
            <button type="button" className={theme === 'dark' ? 'active' : ''} onClick={() => setTheme('dark')}>{t('themeDark')}</button>
            <button type="button" className={theme === 'light' ? 'active' : ''} onClick={() => setTheme('light')}>{t('themeLight')}</button>
          </span>
        </div>
      </header>
      <main className="app-main">
        <Routes>
          <Route path="/" element={<Dashboard />} />
          <Route path="/core" element={<Core />} />
          <Route path="/core-load" element={<CoreLoad />} />
          <Route path="/runtime" element={<Runtime />} />
          <Route path="/kernel" element={<Kernel />} />
          <Route path="/graph" element={<KnowledgeGraph />} />
          <Route path="/trace" element={<Trace />} />
          <Route path="/scenarios" element={<Scenarios />} />
          <Route path="/counterfactual" element={<Counterfactual />} />
          <Route path="/compliance" element={<Compliance />} />
          <Route path="/audit" element={<Audit />} />
          <Route path="/governance" element={<Governance />} />
          <Route path="/ai-governance" element={<AiGovernance />} />
        </Routes>
      </main>
    </div>
  )
}
