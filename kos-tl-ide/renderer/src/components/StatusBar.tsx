import React from 'react';
import './StatusBar.css';

interface StatusBarProps {
    filePath?: string;
    modified: boolean;
    errors: number;
    onSettingsClick?: () => void;
}

export default function StatusBar({ filePath, modified, errors, onSettingsClick }: StatusBarProps) {
    return (
        <div className="status-bar">
            <div className="status-left">
                {filePath && (
                    <span className="status-item">
                        {filePath} {modified && '*'}
                    </span>
                )}
            </div>
            <div className="status-right">
                {errors > 0 && (
                    <span className="status-item status-error">
                        错误: {errors}
                    </span>
                )}
                {onSettingsClick && (
                    <span className="status-item status-clickable" onClick={onSettingsClick}>
                        ⚙️ 设置
                    </span>
                )}
                <span className="status-item">KOS-TL IDE</span>
            </div>
        </div>
    );
}

