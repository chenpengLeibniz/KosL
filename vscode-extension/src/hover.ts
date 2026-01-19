import * as vscode from 'vscode';

export class KOSTLHoverProvider implements vscode.HoverProvider {
    provideHover(
        document: vscode.TextDocument,
        position: vscode.Position,
        token: vscode.CancellationToken
    ): vscode.ProviderResult<vscode.Hover> {
        const wordRange = document.getWordRangeAtPosition(position);
        if (!wordRange) {
            return null;
        }

        const word = document.getText(wordRange);
        const line = document.lineAt(position.line);

        // 关键字提示
        const keywordDocs: { [key: string]: string } = {
            'module': 'Module declaration: `module Name where`',
            'type': 'Type declaration: `type Name : U₁`',
            'def': 'Function definition: `def name : Type = expression`',
            'let': 'Let binding: `let x = value in body`',
            'λ': 'Lambda abstraction: `λ(x : Type) → body`',
            'Σ': 'Dependent sum type: `Σ(x : A) × B`',
            'Π': 'Dependent product type: `Π(x : A) → B`',
            'Prop': 'Proposition type (Prop : Type₁)',
            'U₁': 'Computational universe level 1',
            'Type₁': 'Logical universe level 1',
        };

        if (keywordDocs[word]) {
            return new vscode.Hover({
                language: 'kos-tl',
                value: keywordDocs[word]
            });
        }

        // 类型提示（简化实现）
        // TODO: 集成类型检查器获取实际类型信息
        if (word.match(/^[A-Z][a-zA-Z0-9_']*$/)) {
            return new vscode.Hover({
                language: 'kos-tl',
                value: `**Type**: ${word}\n\nType definition`
            });
        }

        // 变量提示
        if (word.match(/^[a-z][a-zA-Z0-9_']*$/)) {
            return new vscode.Hover({
                language: 'kos-tl',
                value: `**Variable**: ${word}\n\nVariable reference`
            });
        }

        return null;
    }
}





