import * as fs from 'fs';
import * as path from 'path';
import { dialog } from 'electron';

export interface FileNode {
    name: string;
    path: string;
    isDirectory: boolean;
    children?: FileNode[];
}

// 读取目录内容
export function readDirectory(dirPath: string): FileNode[] {
    try {
        const entries = fs.readdirSync(dirPath, { withFileTypes: true });
        const nodes: FileNode[] = [];

        for (const entry of entries) {
            // 忽略隐藏文件和node_modules
            if (entry.name.startsWith('.') || entry.name === 'node_modules') {
                continue;
            }

            const fullPath = path.join(dirPath, entry.name);
            const node: FileNode = {
                name: entry.name,
                path: fullPath,
                isDirectory: entry.isDirectory()
            };

            if (entry.isDirectory()) {
                try {
                    node.children = readDirectory(fullPath);
                } catch (err) {
                    // 忽略无法访问的目录
                }
            }

            nodes.push(node);
        }

        return nodes.sort((a, b) => {
            // 目录在前，文件在后
            if (a.isDirectory && !b.isDirectory) return -1;
            if (!a.isDirectory && b.isDirectory) return 1;
            return a.name.localeCompare(b.name);
        });
    } catch (error) {
        console.error(`Error reading directory ${dirPath}:`, error);
        return [];
    }
}

// 检查文件是否为KOS-TL文件
export function isKOSTLFile(filePath: string): boolean {
    return path.extname(filePath).toLowerCase() === '.kos';
}

// 获取文件统计信息
export function getFileStats(filePath: string): { size: number; modified: Date } | null {
    try {
        const stats = fs.statSync(filePath);
        return {
            size: stats.size,
            modified: stats.mtime
        };
    } catch (error) {
        return null;
    }
}





