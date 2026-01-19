import * as vscode from 'vscode';

export class KOSTLCompletionProvider implements vscode.CompletionItemProvider {
    provideCompletionItems(
        document: vscode.TextDocument,
        position: vscode.Position,
        token: vscode.CancellationToken,
        context: vscode.CompletionContext
    ): vscode.ProviderResult<vscode.CompletionItem[] | vscode.CompletionList> {
        const items: vscode.CompletionItem[] = [];

        // 关键字补全
        const keywords = [
            { label: 'module', detail: 'Module declaration', kind: vscode.CompletionItemKind.Keyword },
            { label: 'where', detail: 'Module body', kind: vscode.CompletionItemKind.Keyword },
            { label: 'import', detail: 'Import module', kind: vscode.CompletionItemKind.Keyword },
            { label: 'type', detail: 'Type declaration', kind: vscode.CompletionItemKind.Keyword },
            { label: 'def', detail: 'Function definition', kind: vscode.CompletionItemKind.Keyword },
            { label: 'let', detail: 'Let binding', kind: vscode.CompletionItemKind.Keyword },
            { label: 'in', detail: 'Let body', kind: vscode.CompletionItemKind.Keyword },
            { label: 'if', detail: 'If expression', kind: vscode.CompletionItemKind.Keyword },
            { label: 'then', detail: 'Then branch', kind: vscode.CompletionItemKind.Keyword },
            { label: 'else', detail: 'Else branch', kind: vscode.CompletionItemKind.Keyword },
            { label: 'match', detail: 'Pattern matching', kind: vscode.CompletionItemKind.Keyword },
            { label: 'with', detail: 'Match cases', kind: vscode.CompletionItemKind.Keyword },
            { label: 'case', detail: 'Case pattern', kind: vscode.CompletionItemKind.Keyword },
            { label: 'of', detail: 'Case body', kind: vscode.CompletionItemKind.Keyword },
        ];

        keywords.forEach(keyword => {
            const item = new vscode.CompletionItem(keyword.label, keyword.kind);
            item.detail = keyword.detail;
            items.push(item);
        });

        // 类型构造器补全
        const typeConstructors = [
            {
                label: 'Σ',
                detail: 'Dependent sum type',
                kind: vscode.CompletionItemKind.Class,
                insertText: new vscode.SnippetString('Σ(${1:x} : ${2:A}) × ${3:B}')
            },
            {
                label: 'Π',
                detail: 'Dependent product type',
                kind: vscode.CompletionItemKind.Class,
                insertText: new vscode.SnippetString('Π(${1:x} : ${2:A}) → ${3:B}')
            },
            {
                label: 'λ',
                detail: 'Lambda abstraction',
                kind: vscode.CompletionItemKind.Function,
                insertText: new vscode.SnippetString('λ(${1:x} : ${2:Type}) → ${3:body}')
            },
            {
                label: '→',
                detail: 'Function type arrow',
                kind: vscode.CompletionItemKind.Operator,
                insertText: '→'
            },
            {
                label: '×',
                detail: 'Product type',
                kind: vscode.CompletionItemKind.Operator,
                insertText: '×'
            },
        ];

        typeConstructors.forEach(constructor => {
            const item = new vscode.CompletionItem(constructor.label, constructor.kind);
            item.detail = constructor.detail;
            if (constructor.insertText) {
                item.insertText = constructor.insertText;
            }
            items.push(item);
        });

        // Universe类型补全
        const universeTypes = [
            { label: 'U₁', detail: 'Computational universe level 1', kind: vscode.CompletionItemKind.TypeParameter },
            { label: 'U₂', detail: 'Computational universe level 2', kind: vscode.CompletionItemKind.TypeParameter },
            { label: 'Type₁', detail: 'Logical universe level 1', kind: vscode.CompletionItemKind.TypeParameter },
            { label: 'Type₂', detail: 'Logical universe level 2', kind: vscode.CompletionItemKind.TypeParameter },
            { label: 'Prop', detail: 'Proposition type', kind: vscode.CompletionItemKind.TypeParameter },
        ];

        universeTypes.forEach(type => {
            const item = new vscode.CompletionItem(type.label, type.kind);
            item.detail = type.detail;
            items.push(item);
        });

        return items;
    }
}





