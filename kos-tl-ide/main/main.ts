import { app, BrowserWindow, Menu } from 'electron';
import * as path from 'path';
import { spawn, ChildProcess } from 'child_process';
import './ipc-handlers';
import { createMenu } from './menu';

let mainWindow: BrowserWindow | null = null;
let compilerProcess: ChildProcess | null = null as ChildProcess | null;

function createWindow(): BrowserWindow | null {
    mainWindow = new BrowserWindow({
        width: 1400,
        height: 900,
        minWidth: 800,
        minHeight: 600,
        webPreferences: {
            nodeIntegration: false,
            contextIsolation: true,
            preload: path.join(__dirname, 'preload.js'),
            webSecurity: false // 开发时允许加载本地资源
        },
        titleBarStyle: 'default',
        show: false
    });

    // 加载应用
    const isDev = process.env.NODE_ENV === 'development' || !app.isPackaged;
    if (isDev) {
        mainWindow.loadURL('http://localhost:3000');
        mainWindow.webContents.openDevTools();
    } else {
        // 生产环境：加载打包后的 HTML 文件
        const htmlPath = path.join(__dirname, '../dist/renderer/index.html');
        console.log('Loading HTML from:', htmlPath);
        mainWindow.loadFile(htmlPath);
    }

    mainWindow.once('ready-to-show', () => {
        mainWindow?.show();
    });

    mainWindow.on('closed', () => {
        mainWindow = null;
    });

    return mainWindow;
}

// 启动编译器服务
function startCompilerService(): void {
    // 这里启动KOS-TL编译器作为语言服务器
    const compilerPath = path.join(__dirname, '../../compiler/build/bin/kos-tl-compiler');
    // TODO: 实现编译器服务进程
}

app.whenReady().then(() => {
    const window = createWindow();
    if (window) {
        const menu = createMenu(window);
        Menu.setApplicationMenu(menu);
    }
    startCompilerService();

    app.on('activate', () => {
        if (BrowserWindow.getAllWindows().length === 0) {
            const window = createWindow();
            if (window) {
                const menu = createMenu(window);
                Menu.setApplicationMenu(menu);
            }
        }
    });
});

app.on('window-all-closed', () => {
    if (compilerProcess) {
        try {
            compilerProcess.kill();
        } catch (e) {
            // Ignore errors
        }
    }
    if (process.platform !== 'darwin') {
        app.quit();
    }
});

app.on('before-quit', () => {
    if (compilerProcess) {
        try {
            compilerProcess.kill();
        } catch (e) {
            // Ignore errors
        }
    }
});

