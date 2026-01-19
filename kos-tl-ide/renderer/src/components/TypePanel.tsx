import React from 'react';
import './TypePanel.css';

interface TypePanelProps {
    typeInfo: any;
}

export default function TypePanel({ typeInfo }: TypePanelProps) {
    return (
        <div className="type-panel">
            <div className="panel-header">
                <h4>类型信息</h4>
            </div>
            <div className="panel-content">
                {typeInfo ? (
                    <div className="type-info">
                        <pre>{JSON.stringify(typeInfo, null, 2)}</pre>
                    </div>
                ) : (
                    <div className="no-type-info">
                        <p>将鼠标悬停在标识符上查看类型信息</p>
                    </div>
                )}
            </div>
        </div>
    );
}





