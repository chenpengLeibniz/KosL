import { createContext, useContext, useState, useEffect } from 'react'
import { translations, getStoredLocale, setStoredLocale } from '../i18n/translations'

const LocaleContext = createContext(null)

export function useLocale() {
  const ctx = useContext(LocaleContext)
  if (!ctx) throw new Error('useLocale must be used within LocaleProvider')
  return ctx
}

/** t(key) or t(key, { types: 1, predicates: 2 }) for interpolation */
export function useT() {
  const { locale, t } = useLocale()
  return t
}

export function LocaleProvider({ children }) {
  const [locale, setLocaleState] = useState(getStoredLocale)

  useEffect(() => {
    setStoredLocale(locale)
  }, [locale])

  const setLocale = (next) => {
    if (next === 'zh' || next === 'en') setLocaleState(next)
  }

  const t = (key, vars) => {
    let s = translations[locale]?.[key] ?? key
    if (vars && typeof s === 'string') {
      Object.keys(vars).forEach((k) => {
        s = s.replace(new RegExp(`\\{${k}\\}`, 'g'), String(vars[k]))
      })
    }
    return s
  }

  return (
    <LocaleContext.Provider value={{ locale, setLocale, t }}>
      {children}
    </LocaleContext.Provider>
  )
}
