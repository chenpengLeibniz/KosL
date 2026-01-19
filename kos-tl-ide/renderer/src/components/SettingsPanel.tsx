import React, { useState } from 'react';
import './SettingsPanel.css';

interface SettingsPanelProps {
    compilerPath: string;
    onCompilerPathChange: (path: string) => void;
    onClose: () => void;
}

export default function SettingsPanel({ compilerPath, onCompilerPathChange, onClose }: SettingsPanelProps) {
    const [localPath, setLocalPath] = useState(compilerPath);

    const handleSave = () => {
        onCompilerPathChange(localPath);
        onClose();
    };

    return (
        <div className="settings-panel-overlay" onClick={onClose}>
            <div className="settings-panel" onClick={(e) => e.stopPropagation()}>
                <div className="settings-header">
                    <h3>设置</h3>
                    <button className="settings-close" onClick={onClose}>×</button>
                </div>
                <div className="settings-content">
                    <div className="settings-item">
                        <label>编译器路径</label>
                        <input
                            type="text"
                            value={localPath}
                            onChange={(e) => setLocalPath(e.target.value)}
                            placeholder="输入编译器可执行文件路径"
                        />
                    </div>
                </div>
                <div className="settings-footer">
                    <button onClick={handleSave}>保存</button>
                    <button onClick={onClose}>取消</button>
                </div>
            </div>
        </div>
    );
}





