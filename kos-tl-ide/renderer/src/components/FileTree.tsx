import React, { useState, useEffect } from 'react';
import './FileTree.css';

interface FileNode {
    name: string;
    path: string;
    isDirectory: boolean;
    children?: FileNode[];
    expanded?: boolean;
}

interface FileTreeProps {
    workspacePath: string | null;
    onFileSelect: (filePath: string) => void;
}

export default function FileTree({ workspacePath, onFileSelect }: FileTreeProps) {
    const [files, setFiles] = useState<FileNode[]>([]);
    const [expandedPaths, setExpandedPaths] = useState<Set<string>>(new Set());

    useEffect(() => {
        if (workspacePath) {
            loadFiles(workspacePath);
        }
    }, [workspacePath]);

    const loadFiles = async (dirPath: string) => {
        try {
            const fileNodes = await window.electronAPI.readDirectory(dirPath);
            setFiles(fileNodes);
        } catch (error) {
            console.error('Failed to load files:', error);
            setFiles([]);
        }
    };

    const toggleExpand = (path: string) => {
        const newExpanded = new Set(expandedPaths);
        if (newExpanded.has(path)) {
            newExpanded.delete(path);
        } else {
            newExpanded.add(path);
        }
        setExpandedPaths(newExpanded);
    };

    const renderNode = (node: FileNode, level: number = 0): React.ReactNode => {
        const isExpanded = expandedPaths.has(node.path);
        const hasChildren = node.isDirectory && node.children && node.children.length > 0;

        return (
            <div key={node.path}>
                <div
                    className={`file-tree-node ${node.isDirectory ? 'directory' : 'file'}`}
                    style={{ paddingLeft: `${level * 16}px` }}
                    onClick={() => {
                        if (node.isDirectory) {
                            toggleExpand(node.path);
                        } else {
                            onFileSelect(node.path);
                        }
                    }}
                >
                    {node.isDirectory && (
                        <span className="file-tree-icon">
                            {isExpanded ? '📂' : '📁'}
                        </span>
                    )}
                    {!node.isDirectory && (
                        <span className="file-tree-icon">📄</span>
                    )}
                    <span className="file-tree-name">{node.name}</span>
                </div>
                {node.isDirectory && isExpanded && hasChildren && (
                    <div className="file-tree-children">
                        {node.children!.map(child => renderNode(child, level + 1))}
                    </div>
                )}
            </div>
        );
    };

    if (!workspacePath) {
        return (
            <div className="file-tree-empty">
                <p>未打开工作区</p>
            </div>
        );
    }

    return (
        <div className="file-tree">
            {files.map(node => renderNode(node))}
        </div>
    );
}

