import React from 'react';
import './WelcomeScreen.css';

interface WelcomeScreenProps {
    onOpenFile: () => void;
    onOpenWorkspace: () => void;
}

export default function WelcomeScreen({ onOpenFile, onOpenWorkspace }: WelcomeScreenProps) {
    return (
        <div className="welcome-screen">
            <div className="welcome-content">
                <h1>欢迎使用 KOS-TL IDE</h1>
                <p>KOS-TL 语言的集成开发环境</p>
                
                <div className="welcome-actions">
                    <button className="welcome-button primary" onClick={onOpenFile}>
                        <span className="icon">📄</span>
                        <span>打开文件</span>
                    </button>
                    <button className="welcome-button" onClick={onOpenWorkspace}>
                        <span className="icon">📁</span>
                        <span>打开工作区</span>
                    </button>
                </div>

                <div className="welcome-shortcuts">
                    <h3>快捷键</h3>
                    <ul>
                        <li><kbd>Ctrl+O</kbd> / <kbd>Cmd+O</kbd> - 打开文件</li>
                        <li><kbd>Ctrl+S</kbd> / <kbd>Cmd+S</kbd> - 保存文件</li>
                        <li><kbd>Ctrl+P</kbd> / <kbd>Cmd+P</kbd> - 命令面板</li>
                        <li><kbd>F5</kbd> - 编译</li>
                    </ul>
                </div>
            </div>
        </div>
    );
}





