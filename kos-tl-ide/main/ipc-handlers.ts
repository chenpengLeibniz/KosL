import { ipcMain, dialog } from 'electron';
import { spawn } from 'child_process';
import * as path from 'path';
import * as fs from 'fs';
import { readDirectory, FileNode } from './file-system';

// 编译器路径（需要根据实际路径调整）
// 在打包的应用中，编译器应该在 resources/compiler/kos-tl-compiler
// 在开发环境中，编译器在 ../../compiler/build/bin/kos-tl-compiler
const getCompilerPath = (): string => {
    const isDev = process.env.NODE_ENV === 'development' || !require('electron').app.isPackaged;
    let compilerPath: string;
    
    if (isDev) {
        // 开发环境
        compilerPath = path.join(__dirname, '../../compiler/build/bin/kos-tl-compiler');
    } else {
        // 打包环境
        compilerPath = path.join(process.resourcesPath, 'compiler', 'kos-tl-compiler');
    }
    
    // Windows 上添加 .exe 扩展名（如果不存在）
    if (process.platform === 'win32' && !compilerPath.endsWith('.exe')) {
        const exePath = compilerPath + '.exe';
        if (fs.existsSync(exePath)) {
            return exePath;
        }
    }
    
    return compilerPath;
};

// 不再使用全局 COMPILER_PATH，每次都动态获取

// 编译文件
ipcMain.handle('compile', async (event, filePath: string, outputPath?: string) => {
    return new Promise((resolve) => {
        const compilerPath = getCompilerPath();
        console.log('Compiler path:', compilerPath);
        console.log('Compiler exists:', fs.existsSync(compilerPath));
        
        // 检查编译器是否存在
        if (!fs.existsSync(compilerPath)) {
            console.error('Compiler not found at:', compilerPath);
            resolve({ 
                success: false, 
                errors: `编译器未找到: ${compilerPath}\n请确保编译器已构建，或在设置中配置正确的编译器路径。\n\n提示：编译器应该在 compiler/build/bin/kos-tl-compiler${process.platform === 'win32' ? '.exe' : ''}` 
            });
            return;
        }

        // 检查文件是否存在
        if (!fs.existsSync(filePath)) {
            resolve({ 
                success: false, 
                errors: `文件不存在: ${filePath}` 
            });
            return;
        }

        const output = outputPath || filePath.replace('.kos', '.c');
        console.log('Compiling:', filePath, '->', output);
        console.log('Using compiler:', compilerPath);
        
        const proc = spawn(compilerPath, [filePath, '-o', output], {
            cwd: path.dirname(filePath)
        });
        
        let stdout = '';
        let stderr = '';
        
        proc.stdout.on('data', (data) => {
            stdout += data.toString();
        });
        
        proc.stderr.on('data', (data) => {
            stderr += data.toString();
        });
        
        proc.on('error', (error) => {
            resolve({ 
                success: false, 
                errors: `编译失败: ${error.message}` 
            });
        });
        
        proc.on('close', (code) => {
            if (code === 0) {
                resolve({ success: true, output: stdout || '编译成功！' });
            } else {
                resolve({ success: false, errors: stderr || `编译失败，退出码: ${code}` });
            }
        });
    });
});

// 类型检查
ipcMain.handle('typeCheck', async (event, filePath: string) => {
    return new Promise((resolve) => {
        const compilerPath = getCompilerPath();
        console.log('Type checking:', filePath);
        console.log('Using compiler:', compilerPath);
        
        // 检查编译器是否存在
        if (!fs.existsSync(compilerPath)) {
            console.warn('Compiler not found, skipping type check');
            resolve({ success: true, errors: [] }); // 编译器不存在时跳过类型检查
            return;
        }

        // 检查文件是否存在
        if (!fs.existsSync(filePath)) {
            resolve({ success: false, errors: [] });
            return;
        }

        const proc = spawn(compilerPath, [filePath, '-t'], {
            cwd: path.dirname(filePath)
        });
        
        let stderr = '';
        
        proc.stderr.on('data', (data) => {
            stderr += data.toString();
        });
        
        proc.on('error', (error) => {
            // 编译器错误时，不显示错误（可能是编译器未配置）
            resolve({ success: true, errors: [] });
        });
        
        proc.on('close', (code) => {
            if (code === 0) {
                resolve({ success: true, errors: [] });
            } else {
                // 解析错误信息
                const errors = parseErrors(stderr);
                resolve({ success: false, errors });
            }
        });
    });
});

// 打开文件对话框
ipcMain.handle('openFile', async () => {
    const result = await dialog.showOpenDialog({
        properties: ['openFile'],
        filters: [
            { name: 'KOS-TL Files', extensions: ['kos'] },
            { name: 'All Files', extensions: ['*'] }
        ]
    });
    
    if (!result.canceled && result.filePaths.length > 0) {
        return result.filePaths[0];
    }
    return null;
});

// 保存文件对话框
ipcMain.handle('saveFile', async (event, content: string) => {
    const result = await dialog.showSaveDialog({
        filters: [
            { name: 'KOS-TL Files', extensions: ['kos'] },
            { name: 'All Files', extensions: ['*'] }
        ]
    });
    
    if (!result.canceled && result.filePath) {
        fs.writeFileSync(result.filePath, content, 'utf-8');
        return result.filePath;
    }
    return null;
});

// 获取工作区路径
ipcMain.handle('getWorkspacePath', async () => {
    const result = await dialog.showOpenDialog({
        properties: ['openDirectory']
    });
    
    if (!result.canceled && result.filePaths.length > 0) {
        return result.filePaths[0];
    }
    return null;
});

// 读取目录内容
ipcMain.handle('readDirectory', async (event, dirPath: string): Promise<FileNode[]> => {
    return readDirectory(dirPath);
});

// 读取文件内容
ipcMain.handle('readFile', async (event, filePath: string): Promise<string> => {
    try {
        return fs.readFileSync(filePath, 'utf-8');
    } catch (error) {
        throw new Error(`Failed to read file: ${error}`);
    }
});

// 写入文件内容
ipcMain.handle('writeFile', async (event, filePath: string, content: string): Promise<void> => {
    try {
        fs.writeFileSync(filePath, content, 'utf-8');
    } catch (error) {
        throw new Error(`Failed to write file: ${error}`);
    }
});

// 解析错误信息（简化实现）
function parseErrors(errorOutput: string): any[] {
    const errors: any[] = [];
    const lines = errorOutput.split('\n');
    
    for (const line of lines) {
        // 解析格式：file.kos:line:column: error: message
        const match = line.match(/(.+):(\d+):(\d+):\s*(error|warning):\s*(.+)/);
        if (match) {
            errors.push({
                file: match[1],
                line: parseInt(match[2]) - 1, // 0-based
                column: parseInt(match[3]) - 1,
                severity: match[4] === 'error' ? 'error' : 'warning',
                message: match[5]
            });
        }
    }
    
    return errors;
}
