import { contextBridge, ipcRenderer } from 'electron';

// 暴露安全的API给渲染进程
contextBridge.exposeInMainWorld('electronAPI', {
    // 文件操作 - 通过IPC调用主进程
    readFile: (filePath: string): Promise<string> => {
        return ipcRenderer.invoke('readFile', filePath);
    },
    
    writeFile: (filePath: string, content: string): Promise<void> => {
        return ipcRenderer.invoke('writeFile', filePath, content);
    },
    
    // 编译器操作
    compile: (filePath: string, outputPath?: string): Promise<{ success: boolean; output?: string; errors?: string }> => {
        return ipcRenderer.invoke('compile', filePath, outputPath);
    },
    
    typeCheck: (filePath: string): Promise<{ success: boolean; errors?: any[] }> => {
        return ipcRenderer.invoke('typeCheck', filePath);
    },
    
    // 文件系统
    openFile: (): Promise<string | null> => {
        return ipcRenderer.invoke('openFile');
    },
    
    saveFile: (content: string): Promise<string | null> => {
        return ipcRenderer.invoke('saveFile', content);
    },
    
    // 工作区
    getWorkspacePath: (): Promise<string | null> => {
        return ipcRenderer.invoke('getWorkspacePath');
    },
    
    // 文件系统
    readDirectory: (dirPath: string): Promise<any[]> => {
        return ipcRenderer.invoke('readDirectory', dirPath);
    }
});

// 暴露 ipcRenderer 用于菜单事件监听
contextBridge.exposeInMainWorld('ipcRenderer', {
    on: (channel: string, callback: (...args: any[]) => void) => {
        ipcRenderer.on(channel, (event, ...args) => callback(...args));
    },
    removeAllListeners: (channel: string) => {
        ipcRenderer.removeAllListeners(channel);
    }
});

