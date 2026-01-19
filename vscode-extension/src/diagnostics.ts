import * as vscode from 'vscode';
import * as path from 'path';

export class KOSTLDiagnosticProvider {
    private diagnosticCollection: vscode.DiagnosticCollection;

    constructor() {
        this.diagnosticCollection = vscode.languages.createDiagnosticCollection('kos-tl');
    }

    public updateDiagnostics(document: vscode.TextDocument): void {
        if (document.languageId !== 'kos-tl') {
            return;
        }

        // 获取编译器路径配置
        const config = vscode.workspace.getConfiguration('kos-tl');
        const compilerPath = config.get<string>('compilerPath', '');
        const enableTypeChecking = config.get<boolean>('enableTypeChecking', true);

        if (!enableTypeChecking) {
            this.diagnosticCollection.clear();
            return;
        }

        // 如果没有配置编译器路径，使用简单的语法检查
        if (!compilerPath) {
            this.checkSyntax(document);
            return;
        }

        // 调用编译器进行类型检查
        this.checkWithCompiler(document, compilerPath);
    }

    private checkSyntax(document: vscode.TextDocument): void {
        const diagnostics: vscode.Diagnostic[] = [];
        const text = document.getText();

        // 简单的语法检查
        // 检查未匹配的括号
        let openBraces = 0;
        let openParens = 0;
        let openBrackets = 0;

        for (let i = 0; i < text.length; i++) {
            const char = text[i];
            const line = document.positionAt(i).line;
            const column = document.positionAt(i).character;

            switch (char) {
                case '{':
                    openBraces++;
                    break;
                case '}':
                    openBraces--;
                    if (openBraces < 0) {
                        diagnostics.push({
                            range: new vscode.Range(line, column, line, column + 1),
                            message: 'Unmatched closing brace',
                            severity: vscode.DiagnosticSeverity.Error,
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
                            range: new vscode.Range(line, column, line, column + 1),
                            message: 'Unmatched closing parenthesis',
                            severity: vscode.DiagnosticSeverity.Error,
                            source: 'kos-tl'
                        });
                    }
                    break;
                case '[':
                    openBrackets++;
                    break;
                case ']':
                    openBrackets--;
                    if (openBrackets < 0) {
                        diagnostics.push({
                            range: new vscode.Range(line, column, line, column + 1),
                            message: 'Unmatched closing bracket',
                            severity: vscode.DiagnosticSeverity.Error,
                            source: 'kos-tl'
                        });
                    }
                    break;
            }
        }

        // 检查未关闭的括号
        if (openBraces > 0) {
            const lastLine = document.lineCount - 1;
            diagnostics.push({
                range: new vscode.Range(lastLine, 0, lastLine, 0),
                message: `${openBraces} unclosed brace(s)`,
                severity: vscode.DiagnosticSeverity.Warning,
                source: 'kos-tl'
            });
        }
        if (openParens > 0) {
            const lastLine = document.lineCount - 1;
            diagnostics.push({
                range: new vscode.Range(lastLine, 0, lastLine, 0),
                message: `${openParens} unclosed parenthesis/parentheses`,
                severity: vscode.DiagnosticSeverity.Warning,
                source: 'kos-tl'
            });
        }
        if (openBrackets > 0) {
            const lastLine = document.lineCount - 1;
            diagnostics.push({
                range: new vscode.Range(lastLine, 0, lastLine, 0),
                message: `${openBrackets} unclosed bracket(s)`,
                severity: vscode.DiagnosticSeverity.Warning,
                source: 'kos-tl'
            });
        }

        this.diagnosticCollection.set(document.uri, diagnostics);
    }

    private checkWithCompiler(document: vscode.TextDocument, compilerPath: string): void {
        // TODO: 调用编译器进行类型检查
        // 使用 -t 选项进行类型检查
        const tempFile = path.join(__dirname, 'temp.kos');
        // 这里应该调用编译器并解析输出
        // 暂时使用语法检查
        this.checkSyntax(document);
    }

    public dispose(): void {
        this.diagnosticCollection.dispose();
    }
}

