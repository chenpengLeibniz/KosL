import React, { useState, useEffect } from 'react';
import Editor from './components/Editor';
import Sidebar from './components/Sidebar';
import StatusBar from './components/StatusBar';
import TypePanel from './components/TypePanel';
import ErrorPanel from './components/ErrorPanel';
import CompilePanel from './components/CompilePanel';
import CommandPalette from './components/CommandPalette';
import SettingsPanel from './components/SettingsPanel';
import WelcomeScreen from './components/WelcomeScreen';
import { useDebounce } from './hooks/useDebounce';
import './App.css';

declare global {
    interface Window {
        electronAPI: {
            readFile: (path: string) => Promise<string>;
            writeFile: (path: string, content: string) => Promise<void>;
            compile: (path: string, outputPath?: string) => Promise<any>;
            typeCheck: (path: string) => Promise<any>;
            openFile: () => Promise<string | null>;
            saveFile: (content: string) => Promise<string | null>;
            getWorkspacePath: () => Promise<string | null>;
            readDirectory: (dirPath: string) => Promise<any[]>;
        };
        ipcRenderer?: {
            on: (channel: string, callback: (...args: any[]) => void) => void;
            removeAllListeners: (channel: string) => void;
        };
    }
}

interface FileInfo {
    path: string;
    content: string;
    modified: boolean;
}

// 解析编译器错误信息（纯函数，移到组件外部）
function parseCompileErrors(errorString: string): any[] {
    const errors: any[] = [];
    const lines = errorString.split('\n');
    
    for (const line of lines) {
        // 解析格式：file.kos:line:column: error: message
        const match = line.match(/(.+):(\d+):(\d+):\s*(error|warning):\s*(.+)/);
        if (match) {
            errors.push({
                file: match[1],
                line: parseInt(match[2]) - 1,
                column: parseInt(match[3]) - 1,
                severity: match[4] === 'error' ? 'error' : 'warning',
                message: match[5]
            });
        }
    }
    
    return errors;
}

function App() {
    const [currentFile, setCurrentFile] = useState<FileInfo | null>(null);
    const [errors, setErrors] = useState<any[]>([]);
    const [typeInfo, setTypeInfo] = useState<any>(null);
    const [workspacePath, setWorkspacePath] = useState<string | null>(null);
    const [compileOutput, setCompileOutput] = useState<string>('');
    const [compileErrors, setCompileErrors] = useState<string[]>([]);
    const [isCompiling, setIsCompiling] = useState(false);
    const [showCommandPalette, setShowCommandPalette] = useState(false);
    const [showSettings, setShowSettings] = useState(false);
    const [compilerPath, setCompilerPath] = useState<string>('');

    // 初始化检查 - 只运行一次
    useEffect(() => {
        console.log('[App] ========== Initializing App ==========');
        console.log('[App] window.electronAPI:', window.electronAPI ? 'available' : 'NOT AVAILABLE');
        console.log('[App] window.ipcRenderer:', window.ipcRenderer ? 'available' : 'NOT AVAILABLE');
        
        // 检查 electronAPI 是否可用
        if (!window.electronAPI) {
            console.error('[App] electronAPI is not available. Make sure preload script is loaded.');
            alert('警告：electronAPI 不可用，某些功能可能无法正常工作。请检查 preload 脚本是否正确加载。');
            return;
        }
        
        console.log('[App] electronAPI is available:', Object.keys(window.electronAPI));
        
        // 检查 ipcRenderer 是否可用
        if (!window.ipcRenderer) {
            console.error('[App] ipcRenderer is not available. Menu events will not work.');
            alert('警告：ipcRenderer 不可用，菜单功能可能无法正常工作。');
            return;
        }
        
        console.log('[App] ipcRenderer is available');
    }, []); // 只在组件挂载时运行一次

    // 使用 useRef 存储最新的函数引用，避免闭包问题
    const currentFileRef = React.useRef(currentFile);
    const workspacePathRef = React.useRef(workspacePath);
    
    // 更新 refs
    useEffect(() => {
        currentFileRef.current = currentFile;
    }, [currentFile]);
    
    useEffect(() => {
        workspacePathRef.current = workspacePath;
    }, [workspacePath]);

    // 监听菜单事件 - 只运行一次
    useEffect(() => {
        console.log('[App] Setting up menu event listeners...');
        
        if (!window.ipcRenderer) {
            console.error('[App] Cannot set up menu listeners: ipcRenderer not available');
            return;
        }
        
        const ipcRenderer = window.ipcRenderer;
        
        // 新建文件
        const handleNewFile = () => {
            console.log('[App] Menu event: menu-new-file');
            setCurrentFile({
                path: 'untitled.kos',
                content: '',
                modified: false
            });
        };
        
        // 打开文件
        const handleOpenFile = (event: any, filePath: string) => {
            console.log('[App] Menu event: menu-open-file, filePath:', filePath);
            handleFileSelect(filePath);
        };
        
        // 打开工作区
        const handleOpenWorkspace = async (event: any, workspacePath: string) => {
            console.log('[App] Menu event: menu-open-workspace, workspacePath:', workspacePath);
            setWorkspacePath(workspacePath);
            // 尝试打开工作区中的第一个 .kos 文件（如果有）
            try {
                const files = await window.electronAPI.readDirectory(workspacePath);
                const findFirstKosFile = (nodes: any[]): string | null => {
                    for (const node of nodes) {
                        if (!node.isDirectory && node.path.endsWith('.kos')) {
                            return node.path;
                        }
                        if (node.isDirectory && node.children) {
                            const found = findFirstKosFile(node.children);
                            if (found) return found;
                        }
                    }
                    return null;
                };
                const firstKosFile = findFirstKosFile(files);
                if (firstKosFile) {
                    handleFileSelect(firstKosFile);
                }
            } catch (error) {
                console.error('[App] Failed to load workspace files:', error);
            }
        };
        
        // 保存
        const handleSave = () => {
            console.log('[App] Menu event: menu-save');
            const file = currentFileRef.current;
            if (file) {
                window.electronAPI.writeFile(file.path, file.content).then(() => {
                    setCurrentFile({ ...file, modified: false });
                }).catch((error) => {
                    console.error('[App] Failed to save file:', error);
                    alert(`保存文件失败: ${error}`);
                });
            }
        };
        
        // 另存为
        const handleSaveAs = async () => {
            console.log('[App] Menu event: menu-save-as');
            const file = currentFileRef.current;
            if (file) {
                try {
                    const newPath = await window.electronAPI.saveFile(file.content);
                    if (newPath) {
                        setCurrentFile({ ...file, path: newPath, modified: false });
                    }
                } catch (error) {
                    console.error('[App] Failed to save file as:', error);
                    alert(`另存为失败: ${error}`);
                }
            }
        };
        
        // 编译
        const handleCompile = async () => {
            console.log('[App] Menu event: menu-compile');
            const file = currentFileRef.current;
            if (!file) {
                alert('请先打开一个文件');
                return;
            }

            if (!file.path.endsWith('.kos')) {
                alert('只能编译 .kos 文件');
                return;
            }

            console.log('[App] Compiling file:', file.path);

            // 先保存文件
            try {
                await window.electronAPI.writeFile(file.path, file.content);
                setCurrentFile({ ...file, modified: false });
                console.log('[App] File saved before compile');
            } catch (error) {
                console.error('[App] Failed to save file before compile:', error);
                alert('保存文件失败，无法编译');
                return;
            }

            setIsCompiling(true);
            setCompileOutput('');
            setCompileErrors([]);
            
            try {
                console.log('[App] Calling compiler...');
                const result = await window.electronAPI.compile(file.path);
                console.log('[App] Compile result:', result);
                
                if (result && result.success) {
                    setCompileOutput(result.output || '编译成功！');
                    setCompileErrors([]);
                    setErrors([]);
                    alert('编译成功！');
                } else {
                    setCompileOutput('');
                    const errorMsg = result?.errors || '编译失败';
                    setCompileErrors(Array.isArray(errorMsg) ? errorMsg : [errorMsg]);
                    if (typeof errorMsg === 'string') {
                        setErrors(parseCompileErrors(errorMsg));
                    }
                    alert(`编译失败: ${typeof errorMsg === 'string' ? errorMsg : JSON.stringify(errorMsg)}`);
                }
            } catch (error: any) {
                setCompileOutput('');
                const errorMsg = `编译错误: ${error?.message || error}`;
                setCompileErrors([errorMsg]);
                console.error('[App] Compile error:', error);
                alert(errorMsg);
            } finally {
                setIsCompiling(false);
            }
        };
        
        // 类型检查
        const handleTypeCheck = async () => {
            console.log('[App] Menu event: menu-type-check');
            const file = currentFileRef.current;
            if (file) {
                try {
                    const result = await window.electronAPI.typeCheck(file.path);
                    if (result.errors) {
                        setErrors(result.errors);
                    } else {
                        setErrors([]);
                    }
                } catch (error) {
                    console.error('[App] Type check error:', error);
                }
            }
        };
        
        // 运行
        const handleRun = () => {
            console.log('[App] Menu event: menu-run');
            alert('运行功能待实现');
        };
        
        // 调试
        const handleDebug = () => {
            console.log('[App] Menu event: menu-debug');
            alert('调试功能待实现');
        };
        
        // 命令面板
        const handleCommandPalette = () => {
            console.log('[App] Menu event: menu-command-palette');
            setShowCommandPalette(true);
        };
        
        // 设置
        const handleSettings = () => {
            console.log('[App] Menu event: menu-settings');
            setShowSettings(true);
        };
        
        // 查找
        const handleFind = () => {
            console.log('[App] Menu event: menu-find');
            // 查找功能通过编辑器内置功能实现（Ctrl+F）
        };
        
        // 替换
        const handleReplace = () => {
            console.log('[App] Menu event: menu-replace');
            // 替换功能通过编辑器内置功能实现（Ctrl+H）
        };
        
        // 格式化
        const handleFormat = () => {
            console.log('[App] Menu event: menu-format');
            alert('格式化功能待实现');
        };
        
        // 注册所有事件监听器
        ipcRenderer.on('menu-new-file', handleNewFile);
        ipcRenderer.on('menu-open-file', handleOpenFile);
        ipcRenderer.on('menu-open-workspace', handleOpenWorkspace);
        ipcRenderer.on('menu-save', handleSave);
        ipcRenderer.on('menu-save-as', handleSaveAs);
        ipcRenderer.on('menu-compile', handleCompile);
        ipcRenderer.on('menu-type-check', handleTypeCheck);
        ipcRenderer.on('menu-run', handleRun);
        ipcRenderer.on('menu-debug', handleDebug);
        ipcRenderer.on('menu-command-palette', handleCommandPalette);
        ipcRenderer.on('menu-settings', handleSettings);
        ipcRenderer.on('menu-find', handleFind);
        ipcRenderer.on('menu-replace', handleReplace);
        ipcRenderer.on('menu-format', handleFormat);
        
        console.log('[App] Menu event listeners registered');
        
        return () => {
            console.log('[App] Cleaning up menu event listeners...');
            ipcRenderer.removeAllListeners('menu-new-file');
            ipcRenderer.removeAllListeners('menu-open-file');
            ipcRenderer.removeAllListeners('menu-open-workspace');
            ipcRenderer.removeAllListeners('menu-save');
            ipcRenderer.removeAllListeners('menu-save-as');
            ipcRenderer.removeAllListeners('menu-compile');
            ipcRenderer.removeAllListeners('menu-type-check');
            ipcRenderer.removeAllListeners('menu-run');
            ipcRenderer.removeAllListeners('menu-debug');
            ipcRenderer.removeAllListeners('menu-command-palette');
            ipcRenderer.removeAllListeners('menu-settings');
            ipcRenderer.removeAllListeners('menu-find');
            ipcRenderer.removeAllListeners('menu-replace');
            ipcRenderer.removeAllListeners('menu-format');
        };
    }, []); // 只在组件挂载时运行一次

    // 快捷键处理
    useEffect(() => {
        const handleKeyDown = (e: KeyboardEvent) => {
            // Ctrl+P / Cmd+P: 命令面板
            if ((e.ctrlKey || e.metaKey) && e.key === 'p') {
                e.preventDefault();
                setShowCommandPalette(true);
            }
            // Ctrl+S / Cmd+S: 保存
            if ((e.ctrlKey || e.metaKey) && e.key === 's') {
                e.preventDefault();
                if (currentFile && currentFile.modified) {
                    saveFile();
                }
            }
            // Ctrl+O / Cmd+O: 打开文件
            if ((e.ctrlKey || e.metaKey) && e.key === 'o') {
                e.preventDefault();
                openFile();
            }
            // F5: 编译
            if (e.key === 'F5') {
                e.preventDefault();
                compile();
            }
        };

        window.addEventListener('keydown', handleKeyDown);
        return () => window.removeEventListener('keydown', handleKeyDown);
    }, [currentFile]);

    const loadWorkspace = async () => {
        const path = await window.electronAPI.getWorkspacePath();
        if (path) {
            setWorkspacePath(path);
        }
    };

    const handleFileSelect = async (filePath: string) => {
        try {
            console.log('[App] ========== Opening file ==========');
            console.log('[App] File path:', filePath);
            
            if (!window.electronAPI) {
                console.error('[App] electronAPI is not available');
                alert('无法打开文件：electronAPI 不可用');
                return;
            }
            
            console.log('[App] Reading file content...');
            const content = await window.electronAPI.readFile(filePath);
            console.log('[App] File content loaded successfully');
            console.log('[App] Content length:', content.length);
            console.log('[App] Content preview (first 200 chars):', content.substring(0, 200));
            
            // 如果还没有设置工作区，尝试从文件路径推断工作区
            if (!workspacePath) {
                console.log('[App] No workspace set, trying to auto-detect...');
                const pathParts = filePath.split(/[/\\]/);
                // 尝试找到包含 .kos 文件的目录作为工作区
                for (let i = pathParts.length - 1; i > 0; i--) {
                    const potentialWorkspace = pathParts.slice(0, i).join('/');
                    try {
                        const files = await window.electronAPI.readDirectory(potentialWorkspace);
                        if (files.length > 0) {
                            setWorkspacePath(potentialWorkspace);
                            console.log('[App] Auto-detected workspace:', potentialWorkspace);
                            break;
                        }
                    } catch (e) {
                        // 继续尝试
                    }
                }
            }
            
            console.log('[App] Creating new file object...');
            const newFile = {
                path: filePath,
                content: content,
                modified: false
            };
            
            console.log('[App] Setting current file state...');
            console.log('[App] File object:', {
                path: newFile.path,
                contentLength: newFile.content.length,
                modified: newFile.modified,
                contentPreview: newFile.content.substring(0, 50)
            });
            
            setCurrentFile(newFile);
            
            // 使用 setTimeout 确保状态更新后再次检查
            setTimeout(() => {
                console.log('[App] State updated, currentFile:', currentFile ? {
                    path: currentFile.path,
                    contentLength: currentFile.content.length
                } : 'null');
            }, 100);
            
            // 自动类型检查
            if (filePath.endsWith('.kos')) {
                console.log('[App] Starting type check...');
                checkTypes(filePath);
            }
            
            console.log('[App] ========== File opening complete ==========');
        } catch (error: any) {
            console.error('[App] Failed to open file:', error);
            console.error('[App] Error details:', {
                message: error?.message,
                stack: error?.stack,
                name: error?.name
            });
            alert(`无法打开文件: ${error?.message || error}`);
        }
    };

    const openFile = async () => {
        try {
            console.log('[App] Opening file dialog...');
            if (!window.electronAPI) {
                console.error('[App] electronAPI is not available');
                alert('无法打开文件：electronAPI 不可用');
                return;
            }
            console.log('[App] Calling window.electronAPI.openFile()...');
            const filePath = await window.electronAPI.openFile();
            console.log('[App] File dialog result:', filePath);
            if (filePath) {
                console.log('[App] File selected:', filePath);
                await handleFileSelect(filePath);
            } else {
                console.log('[App] No file selected (user cancelled)');
            }
        } catch (error: any) {
            console.error('[App] Failed to open file:', error);
            console.error('[App] Error details:', {
                message: error?.message,
                stack: error?.stack,
                name: error?.name
            });
            alert(`无法打开文件: ${error?.message || error}`);
        }
    };

    const saveFile = async () => {
        if (currentFile) {
            await window.electronAPI.writeFile(currentFile.path, currentFile.content);
            setCurrentFile({ ...currentFile, modified: false });
        }
    };

    const saveAsFile = async () => {
        if (currentFile) {
            const newPath = await window.electronAPI.saveFile(currentFile.content);
            if (newPath) {
                setCurrentFile({ ...currentFile, path: newPath, modified: false });
            }
        }
    };

    const checkTypes = async (filePath: string) => {
        const result = await window.electronAPI.typeCheck(filePath);
        if (result.errors) {
            setErrors(result.errors);
        } else {
            setErrors([]);
        }
    };

    const compile = async () => {
        if (!currentFile) {
            alert('请先打开一个文件');
            return;
        }

        if (!currentFile.path.endsWith('.kos')) {
            alert('只能编译 .kos 文件');
            return;
        }

        console.log('Compiling file:', currentFile.path);

        // 先保存文件
        try {
            await window.electronAPI.writeFile(currentFile.path, currentFile.content);
            setCurrentFile({ ...currentFile, modified: false });
            console.log('File saved before compile');
        } catch (error) {
            console.error('Failed to save file before compile:', error);
            alert('保存文件失败，无法编译');
            return;
        }

        setIsCompiling(true);
        setCompileOutput('');
        setCompileErrors([]);
        
        try {
            console.log('Calling compiler...');
            const result = await window.electronAPI.compile(currentFile.path);
            console.log('Compile result:', result);
            
            if (result && result.success) {
                setCompileOutput(result.output || '编译成功！');
                setCompileErrors([]);
                setErrors([]);
                alert('编译成功！');
            } else {
                setCompileOutput('');
                const errorMsg = result?.errors || '编译失败';
                setCompileErrors(Array.isArray(errorMsg) ? errorMsg : [errorMsg]);
                // 同时更新错误列表
                if (typeof errorMsg === 'string') {
                    setErrors(parseCompileErrors(errorMsg));
                }
                alert(`编译失败: ${typeof errorMsg === 'string' ? errorMsg : JSON.stringify(errorMsg)}`);
            }
        } catch (error: any) {
            setCompileOutput('');
            const errorMsg = `编译错误: ${error?.message || error}`;
            setCompileErrors([errorMsg]);
            console.error('Compile error:', error);
            alert(errorMsg);
        } finally {
            setIsCompiling(false);
        }
    };


    const handleContentChange = (content: string) => {
        if (currentFile) {
            setCurrentFile({ ...currentFile, content, modified: true });
        }
    };

    // 防抖的内容用于实时类型检查
    const debouncedContent = useDebounce(currentFile?.content || '', 500);

    useEffect(() => {
        if (currentFile && debouncedContent && currentFile.path.endsWith('.kos')) {
            // 保存到临时文件并检查类型
            checkTypesDebounced(currentFile.path, debouncedContent);
        }
    }, [debouncedContent]);

    const checkTypesDebounced = async (filePath: string, content: string) => {
        // 如果文件已保存，直接检查原文件
        // 否则创建临时文件进行类型检查
        try {
            // 先保存到临时文件
            const tempPath = filePath + '.tmp';
            await window.electronAPI.writeFile(tempPath, content);
            
            const result = await window.electronAPI.typeCheck(tempPath);
            if (result && result.errors && result.errors.length > 0) {
                setErrors(result.errors);
            } else {
                setErrors([]);
            }
            
            // 清理临时文件
            try {
                await window.electronAPI.writeFile(tempPath, ''); // 清空临时文件
            } catch (e) {
                // 忽略清理错误
            }
        } catch (error) {
            console.error('Type check error:', error);
            // 类型检查失败不影响编辑
        }
    };

    // 命令列表
    const commands = [
        {
            id: 'open-file',
            label: '打开文件',
            category: '文件',
            action: openFile
        },
        {
            id: 'save-file',
            label: '保存文件',
            category: '文件',
            action: saveFile
        },
        {
            id: 'save-as',
            label: '另存为',
            category: '文件',
            action: saveAsFile
        },
        {
            id: 'compile',
            label: '编译',
            category: '构建',
            action: compile
        },
        {
            id: 'open-workspace',
            label: '打开工作区',
            category: '文件',
            action: loadWorkspace
        },
        {
            id: 'settings',
            label: '设置',
            category: '首选项',
            action: () => setShowSettings(true)
        }
    ];

    return (
        <div className="app">
            <CommandPalette
                commands={commands}
                isOpen={showCommandPalette}
                onClose={() => setShowCommandPalette(false)}
            />
            <div className="app-header">
                <div className="app-title">KOS-TL IDE</div>
                <div className="app-menu">
                    <button onClick={openFile}>打开文件</button>
                    <button onClick={saveFile} disabled={!currentFile || !currentFile.modified}>
                        保存
                    </button>
                    <button onClick={saveAsFile} disabled={!currentFile}>
                        另存为
                    </button>
                    <button onClick={compile} disabled={!currentFile}>
                        编译
                    </button>
                    <button onClick={() => setShowCommandPalette(true)}>
                        命令面板 (Ctrl+P)
                    </button>
                </div>
            </div>
            <div className="app-body">
                <Sidebar
                    workspacePath={workspacePath}
                    onFileSelect={handleFileSelect}
                    onWorkspaceOpen={loadWorkspace}
                />
                <div className="app-main">
                    {currentFile ? (
                        <Editor
                            key={currentFile.path}
                            value={currentFile.content}
                            onChange={handleContentChange}
                            errors={errors}
                            filePath={currentFile.path}
                            onHover={(position) => {
                                // TODO: 获取悬停位置的类型信息
                                // setTypeInfo(...);
                            }}
                        />
                    ) : (
                        <WelcomeScreen
                            onOpenFile={openFile}
                            onOpenWorkspace={loadWorkspace}
                        />
                    )}
                    {currentFile && (
                        <>
                            <div className="app-panels">
                                <TypePanel typeInfo={typeInfo} />
                                <ErrorPanel errors={errors} />
                            </div>
                            <CompilePanel
                                compileOutput={compileOutput}
                                compileErrors={compileErrors}
                                isCompiling={isCompiling}
                            />
                        </>
                    )}
                </div>
            </div>
            <StatusBar
                filePath={currentFile?.path}
                modified={currentFile?.modified || false}
                errors={errors.length}
                onSettingsClick={() => setShowSettings(true)}
            />
            {showSettings && (
                <SettingsPanel
                    compilerPath={compilerPath}
                    onCompilerPathChange={setCompilerPath}
                    onClose={() => setShowSettings(false)}
                />
            )}
        </div>
    );
}

export default App;

