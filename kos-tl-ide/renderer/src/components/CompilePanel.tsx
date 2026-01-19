import React, { useState } from 'react';
import './CompilePanel.css';

interface CompilePanelProps {
    compileOutput?: string;
    compileErrors?: string[];
    isCompiling: boolean;
}

export default function CompilePanel({ compileOutput, compileErrors, isCompiling }: CompilePanelProps) {
    const [activeTab, setActiveTab] = useState<'output' | 'errors'>('output');

    return (
        <div className="compile-panel">
            <div className="panel-header">
                <div className="panel-tabs">
                    <button
                        className={activeTab === 'output' ? 'active' : ''}
                        onClick={() => setActiveTab('output')}
                    >
                        输出
                    </button>
                    <button
                        className={activeTab === 'errors' ? 'active' : ''}
                        onClick={() => setActiveTab('errors')}
                    >
                        错误
                    </button>
                </div>
            </div>
            <div className="panel-content">
                {isCompiling ? (
                    <div className="compile-status">
                        <p>正在编译...</p>
                    </div>
                ) : (
                    <>
                        {activeTab === 'output' && (
                            <div className="compile-output">
                                {compileOutput ? (
                                    <pre>{compileOutput}</pre>
                                ) : (
                                    <p className="no-output">没有输出</p>
                                )}
                            </div>
                        )}
                        {activeTab === 'errors' && (
                            <div className="compile-errors">
                                {compileErrors && compileErrors.length > 0 ? (
                                    <ul>
                                        {compileErrors.map((error, index) => (
                                            <li key={index}>{error}</li>
                                        ))}
                                    </ul>
                                ) : (
                                    <p className="no-errors">✓ 编译成功</p>
                                )}
                            </div>
                        )}
                    </>
                )}
            </div>
        </div>
    );
}





