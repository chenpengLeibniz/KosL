import React from 'react';
import ReactDOM from 'react-dom/client';
import App from './App';

console.log('[Index] ========== Initializing React App ==========');
console.log('[Index] window object:', typeof window !== 'undefined' ? 'available' : 'NOT AVAILABLE');
console.log('[Index] window.electronAPI:', (window as any).electronAPI ? 'available' : 'NOT AVAILABLE');
console.log('[Index] window.ipcRenderer:', (window as any).ipcRenderer ? 'available' : 'NOT AVAILABLE');

const rootElement = document.getElementById('root');
if (!rootElement) {
    console.error('[Index] Root element not found!');
    throw new Error('Root element not found');
}

console.log('[Index] Root element found, creating React root...');
const root = ReactDOM.createRoot(rootElement);
console.log('[Index] Rendering App component...');
root.render(
    <React.StrictMode>
        <App />
    </React.StrictMode>
);
console.log('[Index] App component rendered');



