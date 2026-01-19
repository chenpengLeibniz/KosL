import React, { useState, useEffect } from 'react';
import './CommandPalette.css';

interface Command {
    id: string;
    label: string;
    category: string;
    action: () => void;
}

interface CommandPaletteProps {
    commands: Command[];
    isOpen: boolean;
    onClose: () => void;
}

export default function CommandPalette({ commands, isOpen, onClose }: CommandPaletteProps) {
    const [search, setSearch] = useState('');
    const [selectedIndex, setSelectedIndex] = useState(0);

    const filteredCommands = commands.filter(cmd =>
        cmd.label.toLowerCase().includes(search.toLowerCase()) ||
        cmd.category.toLowerCase().includes(search.toLowerCase())
    );

    useEffect(() => {
        if (!isOpen) {
            setSearch('');
            setSelectedIndex(0);
        }
    }, [isOpen]);

    useEffect(() => {
        if (!isOpen) return;

        const handleKeyDown = (e: KeyboardEvent) => {
            if (e.key === 'Escape') {
                onClose();
            } else if (e.key === 'ArrowDown') {
                e.preventDefault();
                setSelectedIndex(prev => Math.min(prev + 1, filteredCommands.length - 1));
            } else if (e.key === 'ArrowUp') {
                e.preventDefault();
                setSelectedIndex(prev => Math.max(prev - 1, 0));
            } else if (e.key === 'Enter') {
                e.preventDefault();
                if (filteredCommands[selectedIndex]) {
                    filteredCommands[selectedIndex].action();
                    onClose();
                }
            }
        };

        window.addEventListener('keydown', handleKeyDown);
        return () => window.removeEventListener('keydown', handleKeyDown);
    }, [isOpen, selectedIndex, filteredCommands, onClose]);

    if (!isOpen) return null;

    return (
        <div className="command-palette-overlay" onClick={onClose}>
            <div className="command-palette" onClick={(e) => e.stopPropagation()}>
                <input
                    type="text"
                    className="command-palette-input"
                    placeholder="输入命令名称..."
                    value={search}
                    onChange={(e) => {
                        setSearch(e.target.value);
                        setSelectedIndex(0);
                    }}
                    autoFocus
                />
                <div className="command-palette-list">
                    {filteredCommands.length > 0 ? (
                        filteredCommands.map((cmd, index) => (
                            <div
                                key={cmd.id}
                                className={`command-palette-item ${index === selectedIndex ? 'selected' : ''}`}
                                onClick={() => {
                                    cmd.action();
                                    onClose();
                                }}
                            >
                                <span className="command-label">{cmd.label}</span>
                                <span className="command-category">{cmd.category}</span>
                            </div>
                        ))
                    ) : (
                        <div className="command-palette-empty">没有找到命令</div>
                    )}
                </div>
            </div>
        </div>
    );
}





