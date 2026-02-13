import { createContext, useContext, useState, useEffect } from 'react'

const STORAGE_KEY = 'kos-web-theme'

function getStoredTheme() {
  try {
    const v = localStorage.getItem(STORAGE_KEY)
    if (v === 'dark' || v === 'light') return v
  } catch (_) {}
  return 'dark'
}

const ThemeContext = createContext(null)

export function useTheme() {
  const ctx = useContext(ThemeContext)
  if (!ctx) throw new Error('useTheme must be used within ThemeProvider')
  return ctx
}

export function ThemeProvider({ children }) {
  const [theme, setThemeState] = useState(getStoredTheme)

  useEffect(() => {
    try {
      localStorage.setItem(STORAGE_KEY, theme)
    } catch (_) {}
    document.documentElement.setAttribute('data-theme', theme)
  }, [theme])

  const setTheme = (next) => {
    if (next === 'dark' || next === 'light') setThemeState(next)
  }

  return (
    <ThemeContext.Provider value={{ theme, setTheme }}>
      {children}
    </ThemeContext.Provider>
  )
}
