import React from 'react';
import './ErrorPanel.css';

interface ErrorPanelProps {
    errors: any[];
}

export default function ErrorPanel({ errors }: ErrorPanelProps) {
    return (
        <div className="error-panel">
            <div className="panel-header">
                <h4>问题 ({errors.length})</h4>
            </div>
            <div className="panel-content">
                {errors.length > 0 ? (
                    <ul className="error-list">
                        {errors.map((error, index) => (
                            <li key={index} className={`error-item error-${error.severity}`}>
                                <div className="error-location">
                                    {error.file}:{error.line + 1}:{error.column + 1}
                                </div>
                                <div className="error-message">{error.message}</div>
                            </li>
                        ))}
                    </ul>
                ) : (
                    <div className="no-errors">
                        <p>✓ 没有错误</p>
                    </div>
                )}
            </div>
        </div>
    );
}





