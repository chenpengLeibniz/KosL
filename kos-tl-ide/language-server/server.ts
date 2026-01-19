// language-server/server.ts
// KOS-TL 语言服务器实现

import {
    createConnection,
    TextDocuments,
    Diagnostic,
    DiagnosticSeverity,
    InitializeResult,
    TextDocumentSyncKind,
    CompletionItem,
    CompletionItemKind,
    Hover,
    Position,
    Range,
    DocumentSymbol,
    SymbolKind,
    Location,
    DefinitionParams,
    ReferencesParams
} from 'vscode-languageserver/node';
import { TextDocument } from 'vscode-languageserver-textdocument';
import { spawn } from 'child_process';
import * as path from 'path';
import * as fs from 'fs';

// 创建连接
const connection = createConnection();

// 文档管理器
const documents = new TextDocuments(TextDocument);

// 编译器路径（需要配置）
let compilerPath: string = '';

// 初始化
connection.onInitialize((params): InitializeResult => {
    return {
        capabilities: {
            textDocumentSync: TextDocumentSyncKind.Incremental,
            completionProvider: {
                triggerCharacters: ['.', ':', '→', '-', 'λ', 'Σ', 'Π']
            },
            hoverProvider: true,
            definitionProvider: true,
            referencesProvider: true,
            documentSymbolProvider: true,
            diagnosticProvider: {
                interFileDependencies: false,
                workspaceDiagnostics: false
            }
        }
    };
});

// 文档变更时进行诊断
documents.onDidChangeContent(async (change) => {
    await validateDocument(change.document);
});

// 验证文档
async function validateDocument(textDocument: TextDocument): Promise<void> {
    const diagnostics: Diagnostic[] = [];

    // 如果配置了编译器路径，使用编译器进行类型检查
    if (compilerPath && fs.existsSync(compilerPath)) {
        const tempFile = path.join(__dirname, 'temp.kos');
        
        // 写入临时文件
        fs.writeFileSync(tempFile, textDocument.getText(), 'utf-8');
        
        try {
            // 调用编译器进行类型检查
            const result = await runCompiler([tempFile, '-t']);
            
            if (result.stderr) {
                // 解析错误
                const errors = parseErrors(result.stderr, textDocument);
                diagnostics.push(...errors);
            }
            
            // 清理临时文件
            try {
                fs.unlinkSync(tempFile);
            } catch (e) {
                // 忽略清理错误
            }
        } catch (error) {
            console.error('Type check error:', error);
        }
    } else {
        // 简单的语法检查
        const syntaxErrors = checkSyntax(textDocument);
        diagnostics.push(...syntaxErrors);
    }

    // 发送诊断
    connection.sendDiagnostics({
        uri: textDocument.uri,
        diagnostics
    });
}

// 运行编译器
function runCompiler(args: string[]): Promise<{ stdout: string; stderr: string }> {
    return new Promise((resolve, reject) => {
        const process = spawn(compilerPath, args);
        let stdout = '';
        let stderr = '';

        process.stdout.on('data', (data) => {
            stdout += data.toString();
        });

        process.stderr.on('data', (data) => {
            stderr += data.toString();
        });

        process.on('close', (code) => {
            resolve({ stdout, stderr });
        });

        process.on('error', (error) => {
            reject(error);
        });
    });
}

// 解析错误
function parseErrors(errorOutput: string, document: TextDocument): Diagnostic[] {
    const diagnostics: Diagnostic[] = [];
    const lines = errorOutput.split('\n');

    for (const line of lines) {
        // 解析格式：file.kos:line:column: error: message
        const match = line.match(/(.+):(\d+):(\d+):\s*(error|warning):\s*(.+)/);
        if (match) {
            const lineNum = parseInt(match[2]) - 1;
            const colNum = parseInt(match[3]) - 1;
            const severity = match[4] === 'error' ? DiagnosticSeverity.Error : DiagnosticSeverity.Warning;
            const message = match[5];

            diagnostics.push({
                range: Range.create(lineNum, colNum, lineNum, colNum + 1),
                severity,
                message,
                source: 'kos-tl'
            });
        }
    }

    return diagnostics;
}

// 简单语法检查
function checkSyntax(document: TextDocument): Diagnostic[] {
    const diagnostics: Diagnostic[] = [];
    const text = document.getText();
    const lines = text.split('\n');

    // 检查括号匹配
    let openBraces = 0;
    let openParens = 0;

    for (let i = 0; i < lines.length; i++) {
        const line = lines[i];
        for (let j = 0; j < line.length; j++) {
            const char = line[j];
            switch (char) {
                case '{':
                    openBraces++;
                    break;
                case '}':
                    openBraces--;
                    if (openBraces < 0) {
                        diagnostics.push({
                            range: Range.create(i, j, i, j + 1),
                            severity: DiagnosticSeverity.Error,
                            message: 'Unmatched closing brace',
                            source: 'kos-tl'
                        });
                    }
                    break;
                case '(':
                    openParens++;
                    break;
                case ')':
                    openParens--;
                    if (openParens < 0) {
                        diagnostics.push({
                            range: Range.create(i, j, i, j + 1),
                            severity: DiagnosticSeverity.Error,
                            message: 'Unmatched closing parenthesis',
                            source: 'kos-tl'
                        });
                    }
                    break;
            }
        }
    }

    return diagnostics;
}

// 代码补全
connection.onCompletion((params): CompletionItem[] => {
    const items: CompletionItem[] = [
        // 关键字
        { label: 'module', kind: CompletionItemKind.Keyword },
        { label: 'type', kind: CompletionItemKind.Keyword },
        { label: 'def', kind: CompletionItemKind.Keyword },
        { label: 'let', kind: CompletionItemKind.Keyword },
        { label: 'λ', kind: CompletionItemKind.Function },
        { label: 'Σ', kind: CompletionItemKind.Class },
        { label: 'Π', kind: CompletionItemKind.Class },
        { label: 'Prop', kind: CompletionItemKind.TypeParameter },
        { label: 'U₁', kind: CompletionItemKind.TypeParameter },
        { label: 'Type₁', kind: CompletionItemKind.TypeParameter }
    ];

    return items;
});

// 悬停提示
connection.onHover((params): Hover | null => {
    const document = documents.get(params.textDocument.uri);
    if (!document) return null;

    const position = params.position;
    const wordRange = document.getWordRangeAtPosition(position);
    if (!wordRange) return null;

    const word = document.getText(wordRange);

    // 关键字提示
    const keywordDocs: { [key: string]: string } = {
        'module': 'Module declaration',
        'type': 'Type declaration',
        'def': 'Function definition',
        'λ': 'Lambda abstraction',
        'Σ': 'Dependent sum type',
        'Π': 'Dependent product type'
    };

    if (keywordDocs[word]) {
        return {
            contents: {
                kind: 'markdown',
                value: `**${word}**: ${keywordDocs[word]}`
            }
        };
    }

    return null;
});

// 定义跳转
connection.onDefinition((params): Location[] => {
    // TODO: 实现定义跳转
    return [];
});

// 引用查找
connection.onReferences((params): Location[] => {
    // TODO: 实现引用查找
    return [];
});

// 文档符号
connection.onDocumentSymbol((params): DocumentSymbol[] => {
    const document = documents.get(params.textDocument.uri);
    if (!document) return [];

    const symbols: DocumentSymbol[] = [];
    const text = document.getText();
    const lines = text.split('\n');

    // 查找模块声明
    const moduleMatch = text.match(/module\s+(\w+)/);
    if (moduleMatch) {
        symbols.push({
            name: moduleMatch[1],
            kind: SymbolKind.Module,
            range: Range.create(0, 0, lines.length - 1, 0),
            selectionRange: Range.create(0, 0, 0, moduleMatch[0].length)
        });
    }

    // 查找类型声明
    const typeRegex = /type\s+(\w+)/g;
    let match;
    while ((match = typeRegex.exec(text)) !== null) {
        const lineNum = text.substring(0, match.index).split('\n').length - 1;
        symbols.push({
            name: match[1],
            kind: SymbolKind.Class,
            range: Range.create(lineNum, 0, lineNum, match[0].length),
            selectionRange: Range.create(lineNum, match.index - text.lastIndexOf('\n', match.index) - 1, lineNum, match.index - text.lastIndexOf('\n', match.index) - 1 + match[0].length)
        });
    }

    // 查找函数定义
    const defRegex = /def\s+(\w+)/g;
    while ((match = defRegex.exec(text)) !== null) {
        const lineNum = text.substring(0, match.index).split('\n').length - 1;
        symbols.push({
            name: match[1],
            kind: SymbolKind.Function,
            range: Range.create(lineNum, 0, lineNum, match[0].length),
            selectionRange: Range.create(lineNum, match.index - text.lastIndexOf('\n', match.index) - 1, lineNum, match.index - text.lastIndexOf('\n', match.index) - 1 + match[0].length)
        });
    }

    return symbols;
});

// 监听文档
documents.listen(connection);

// 启动服务器
connection.listen();





