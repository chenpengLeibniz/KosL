import React from 'react'
import ReactDOM from 'react-dom/client'
import { BrowserRouter } from 'react-router-dom'
import { ThemeProvider } from './context/ThemeContext'
import { LocaleProvider } from './context/LocaleContext'
import App from './App'
import './index.css'

;(function () {
  try {
    const v = localStorage.getItem('kos-web-theme')
    document.documentElement.setAttribute('data-theme', v === 'light' ? 'light' : 'dark')
  } catch (_) {}
})()

ReactDOM.createRoot(document.getElementById('root')).render(
  <React.StrictMode>
    <BrowserRouter>
      <ThemeProvider>
        <LocaleProvider>
          <App />
        </LocaleProvider>
      </ThemeProvider>
    </BrowserRouter>
  </React.StrictMode>,
)
