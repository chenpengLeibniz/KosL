import React, { useState } from 'react';
import FileTree from './FileTree';
import './Sidebar.css';

interface SidebarProps {
    workspacePath: string | null;
    onFileSelect: (filePath: string) => void;
    onWorkspaceOpen: () => void;
}

export default function Sidebar({ workspacePath, onFileSelect, onWorkspaceOpen }: SidebarProps) {
    const [activeTab, setActiveTab] = useState<'files' | 'types' | 'search'>('files');

    return (
        <div className="sidebar">
            <div className="sidebar-header">
                <h3>资源管理器</h3>
            </div>
            <div className="sidebar-content">
                {workspacePath ? (
                    <>
                        {activeTab === 'files' && (
                            <FileTree workspacePath={workspacePath} onFileSelect={onFileSelect} />
                        )}
                        {activeTab === 'types' && (
                            <div className="sidebar-panel">
                                <p>类型视图</p>
                                {/* TODO: 实现类型视图 */}
                            </div>
                        )}
                        {activeTab === 'search' && (
                            <div className="sidebar-panel">
                                <p>搜索</p>
                                {/* TODO: 实现搜索功能 */}
                            </div>
                        )}
                    </>
                ) : (
                    <div className="no-workspace">
                        <p>未打开工作区</p>
                        <button onClick={onWorkspaceOpen}>打开工作区</button>
                    </div>
                )}
            </div>
            <div className="sidebar-footer">
                <div className="sidebar-tabs">
                    <button
                        className={activeTab === 'files' ? 'active' : ''}
                        onClick={() => setActiveTab('files')}
                    >
                        文件
                    </button>
                    <button
                        className={activeTab === 'types' ? 'active' : ''}
                        onClick={() => setActiveTab('types')}
                    >
                        类型
                    </button>
                    <button
                        className={activeTab === 'search' ? 'active' : ''}
                        onClick={() => setActiveTab('search')}
                    >
                        搜索
                    </button>
                </div>
            </div>
        </div>
    );
}

