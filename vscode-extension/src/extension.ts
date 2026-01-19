import * as vscode from 'vscode';
import { KOSTLCompletionProvider } from './completion';
import { KOSTLHoverProvider } from './hover';
import { KOSTLDiagnosticProvider } from './diagnostics';

export function activate(context: vscode.ExtensionContext) {
    console.log('KOS-TL extension is now active!');

    // 注册代码补全提供者
    const completionProvider = new KOSTLCompletionProvider();
    const completionDisposable = vscode.languages.registerCompletionItemProvider(
        'kos-tl',
        completionProvider,
        '.', ':', '→', '-'
    );
    context.subscriptions.push(completionDisposable);

    // 注册悬停提示提供者
    const hoverProvider = new KOSTLHoverProvider();
    const hoverDisposable = vscode.languages.registerHoverProvider(
        'kos-tl',
        hoverProvider
    );
    context.subscriptions.push(hoverDisposable);

    // 注册错误诊断提供者
    const diagnosticProvider = new KOSTLDiagnosticProvider();
    context.subscriptions.push(diagnosticProvider);

    // 注册命令
    const checkTypeCommand = vscode.commands.registerCommand('kos-tl.checkType', () => {
        const editor = vscode.window.activeTextEditor;
        if (editor && editor.document.languageId === 'kos-tl') {
            vscode.window.showInformationMessage('Type checking...');
            // TODO: 实现类型检查
        }
    });
    context.subscriptions.push(checkTypeCommand);

    const compileCommand = vscode.commands.registerCommand('kos-tl.compile', () => {
        const editor = vscode.window.activeTextEditor;
        if (editor && editor.document.languageId === 'kos-tl') {
            vscode.window.showInformationMessage('Compiling...');
            // TODO: 实现编译
        }
    });
    context.subscriptions.push(compileCommand);

    // 监听文档变更，更新诊断
    vscode.workspace.onDidChangeTextDocument((e) => {
        if (e.document.languageId === 'kos-tl') {
            diagnosticProvider.updateDiagnostics(e.document);
        }
    }, null, context.subscriptions);

    // 打开文档时进行诊断
    vscode.workspace.textDocuments.forEach((doc) => {
        if (doc.languageId === 'kos-tl') {
            diagnosticProvider.updateDiagnostics(doc);
        }
    });
}

export function deactivate() {
    console.log('KOS-TL extension is now deactivated!');
}





