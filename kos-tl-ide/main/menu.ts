import { Menu, MenuItem, app, shell, dialog } from 'electron';
import { BrowserWindow } from 'electron';

export function createMenu(mainWindow: BrowserWindow): Menu {
    const template: Electron.MenuItemConstructorOptions[] = [
        {
            label: '文件',
            submenu: [
                {
                    label: '新建文件',
                    accelerator: 'CmdOrCtrl+N',
                    click: () => {
                        mainWindow.webContents.send('menu-new-file');
                    }
                },
                {
                    label: '打开文件',
                    accelerator: 'CmdOrCtrl+O',
                    click: async () => {
                        const result = await dialog.showOpenDialog(mainWindow, {
                            properties: ['openFile'],
                            filters: [
                                { name: 'KOS-TL Files', extensions: ['kos'] },
                                { name: 'All Files', extensions: ['*'] }
                            ]
                        });
                        if (!result.canceled && result.filePaths.length > 0) {
                            mainWindow.webContents.send('menu-open-file', result.filePaths[0]);
                        }
                    }
                },
                {
                    label: '打开工作区',
                    accelerator: 'CmdOrCtrl+K CmdOrCtrl+O',
                    click: async () => {
                        const result = await dialog.showOpenDialog(mainWindow, {
                            properties: ['openDirectory']
                        });
                        if (!result.canceled && result.filePaths.length > 0) {
                            mainWindow.webContents.send('menu-open-workspace', result.filePaths[0]);
                        }
                    }
                },
                {
                    label: '打开最近的文件',
                    submenu: [
                        {
                            label: '暂无最近文件',
                            enabled: false
                        }
                    ]
                },
                { type: 'separator' },
                {
                    label: '保存',
                    accelerator: 'CmdOrCtrl+S',
                    click: () => {
                        mainWindow.webContents.send('menu-save');
                    }
                },
                {
                    label: '另存为',
                    accelerator: 'CmdOrCtrl+Shift+S',
                    click: () => {
                        mainWindow.webContents.send('menu-save-as');
                    }
                },
                { type: 'separator' },
                {
                    label: '退出',
                    accelerator: process.platform === 'darwin' ? 'Cmd+Q' : 'Ctrl+Q',
                    click: () => {
                        app.quit();
                    }
                }
            ]
        },
        {
            label: '编辑',
            submenu: [
                {
                    label: '撤销',
                    accelerator: 'CmdOrCtrl+Z',
                    role: 'undo'
                },
                {
                    label: '重做',
                    accelerator: 'Shift+CmdOrCtrl+Z',
                    role: 'redo'
                },
                { type: 'separator' },
                {
                    label: '剪切',
                    accelerator: 'CmdOrCtrl+X',
                    role: 'cut'
                },
                {
                    label: '复制',
                    accelerator: 'CmdOrCtrl+C',
                    role: 'copy'
                },
                {
                    label: '粘贴',
                    accelerator: 'CmdOrCtrl+V',
                    role: 'paste'
                },
                {
                    label: '全选',
                    accelerator: 'CmdOrCtrl+A',
                    role: 'selectAll'
                },
                { type: 'separator' },
                {
                    label: '查找',
                    accelerator: 'CmdOrCtrl+F',
                    click: () => {
                        mainWindow.webContents.send('menu-find');
                    }
                },
                {
                    label: '替换',
                    accelerator: 'CmdOrCtrl+H',
                    click: () => {
                        mainWindow.webContents.send('menu-replace');
                    }
                }
            ]
        },
        {
            label: '视图',
            submenu: [
                {
                    label: '重新加载',
                    accelerator: 'CmdOrCtrl+R',
                    click: (item, focusedWindow) => {
                        if (focusedWindow) {
                            focusedWindow.reload();
                        }
                    }
                },
                {
                    label: '强制重新加载',
                    accelerator: 'CmdOrCtrl+Shift+R',
                    click: (item, focusedWindow) => {
                        if (focusedWindow) {
                            focusedWindow.webContents.reloadIgnoringCache();
                        }
                    }
                },
                {
                    label: '切换开发者工具',
                    accelerator: process.platform === 'darwin' ? 'Alt+Cmd+I' : 'Ctrl+Shift+I',
                    click: (item, focusedWindow) => {
                        if (focusedWindow) {
                            focusedWindow.webContents.toggleDevTools();
                        }
                    }
                },
                { type: 'separator' },
                {
                    label: '实际大小',
                    accelerator: 'CmdOrCtrl+0',
                    role: 'resetZoom'
                },
                {
                    label: '放大',
                    accelerator: 'CmdOrCtrl+Plus',
                    role: 'zoomIn'
                },
                {
                    label: '缩小',
                    accelerator: 'CmdOrCtrl+-',
                    role: 'zoomOut'
                },
                { type: 'separator' },
                {
                    label: '切换全屏',
                    accelerator: process.platform === 'darwin' ? 'Ctrl+Cmd+F' : 'F11',
                    role: 'togglefullscreen'
                }
            ]
        },
        {
            label: '运行',
            submenu: [
                {
                    label: '编译',
                    accelerator: 'F5',
                    click: () => {
                        mainWindow.webContents.send('menu-compile');
                    }
                },
                {
                    label: '类型检查',
                    accelerator: 'F6',
                    click: () => {
                        mainWindow.webContents.send('menu-type-check');
                    }
                },
                { type: 'separator' },
                {
                    label: '运行',
                    accelerator: 'F9',
                    click: () => {
                        mainWindow.webContents.send('menu-run');
                    }
                },
                {
                    label: '调试',
                    accelerator: 'F10',
                    click: () => {
                        mainWindow.webContents.send('menu-debug');
                    }
                }
            ]
        },
        {
            label: '工具',
            submenu: [
                {
                    label: '命令面板',
                    accelerator: 'CmdOrCtrl+P',
                    click: () => {
                        mainWindow.webContents.send('menu-command-palette');
                    }
                },
                {
                    label: '设置',
                    accelerator: 'CmdOrCtrl+,',
                    click: () => {
                        mainWindow.webContents.send('menu-settings');
                    }
                },
                { type: 'separator' },
                {
                    label: '格式化文档',
                    accelerator: 'Shift+Alt+F',
                    click: () => {
                        mainWindow.webContents.send('menu-format');
                    }
                }
            ]
        },
        {
            label: '帮助',
            submenu: [
                {
                    label: '关于 KOS-TL IDE',
                    click: () => {
                        dialog.showMessageBox(mainWindow, {
                            type: 'info',
                            title: '关于 KOS-TL IDE',
                            message: 'KOS-TL IDE',
                            detail: '版本 0.1.0\nKOS-TL 语言的集成开发环境'
                        });
                    }
                },
                { type: 'separator' },
                {
                    label: '学习更多',
                    click: () => {
                        shell.openExternal('https://github.com/kos-tl/kos-tl');
                    }
                }
            ]
        }
    ];

    // macOS 特殊处理
    if (process.platform === 'darwin') {
        template.unshift({
            label: app.getName(),
            submenu: [
                {
                    label: '关于 ' + app.getName(),
                    role: 'about'
                },
                { type: 'separator' },
                {
                    label: '服务',
                    role: 'services',
                    submenu: []
                },
                { type: 'separator' },
                {
                    label: '隐藏 ' + app.getName(),
                    accelerator: 'Command+H',
                    role: 'hide'
                },
                {
                    label: '隐藏其他',
                    accelerator: 'Command+Shift+H',
                    role: 'hideOthers'
                },
                {
                    label: '显示全部',
                    role: 'unhide'
                },
                { type: 'separator' },
                {
                    label: '退出',
                    accelerator: 'Command+Q',
                    click: () => {
                        app.quit();
                    }
                }
            ]
        });
    }

    const menu = Menu.buildFromTemplate(template);
    return menu;
}

