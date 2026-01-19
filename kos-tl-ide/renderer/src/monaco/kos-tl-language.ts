import * as monaco from 'monaco-editor';

export function registerKOSTLLanguage() {
    // 检查是否已注册
    if (monaco.languages.getLanguages().find(lang => lang.id === 'kos-tl')) {
        return;
    }

    // 注册语言
    monaco.languages.register({ id: 'kos-tl' });

    // 设置语言配置
    monaco.languages.setLanguageConfiguration('kos-tl', {
        comments: {
            lineComment: '--',
            blockComment: ['{-', '-}']
        },
        brackets: [
            ['{', '}'],
            ['[', ']'],
            ['(', ')']
        ],
        autoClosingPairs: [
            { open: '{', close: '}' },
            { open: '[', close: ']' },
            { open: '(', close: ')' },
            { open: '"', close: '"' }
        ],
        surroundingPairs: [
            { open: '{', close: '}' },
            { open: '[', close: ']' },
            { open: '(', close: ')' },
            { open: '"', close: '"' }
        ]
    });

    // 设置语法高亮
    monaco.languages.setMonarchTokensProvider('kos-tl', {
        keywords: [
            'module', 'where', 'import', 'type', 'def', 'let', 'in',
            'if', 'then', 'else', 'match', 'with', 'case', 'of', 'as'
        ],
        operators: ['→', '->', '=>', 'Σ', 'Π', 'λ', '×', '==', '!=', '<=', '>=', '<', '>'],
        typeKeywords: ['Prop', 'Type', 'U'],
        tokenizer: {
            root: [
                [/--.*$/, 'comment'],
                [/\\{-/, 'comment', '@comment'],
                [/\b(module|where|import|type|def|let|in|if|then|else|match|with|case|of|as)\b/, 'keyword'],
                [/\b(Prop|Type|U)\d*\b/, 'type'],
                [/(→|->|=>|Σ|Π|λ|×)/, 'operator'],
                [/[A-Z][a-zA-Z0-9_']*/, 'type.identifier'],
                [/[a-z][a-zA-Z0-9_']*/, 'identifier'],
                [/"/, 'string', '@string'],
                [/\d+\.?\d*/, 'number']
            ],
            comment: [
                [/[^-]+/, 'comment'],
                [/-}/, 'comment', '@pop'],
                [/./, 'comment']
            ],
            string: [
                [/[^\\"]+/, 'string'],
                [/\\./, 'string.escape'],
                [/"/, 'string', '@pop']
            ]
        }
    });

    // 设置代码补全
    monaco.languages.registerCompletionItemProvider('kos-tl', {
        provideCompletionItems: (model, position) => {
            const word = model.getWordUntilPosition(position);
            const range = {
                startLineNumber: position.lineNumber,
                endLineNumber: position.lineNumber,
                startColumn: word.startColumn,
                endColumn: word.endColumn
            };

            const suggestions: monaco.languages.CompletionItem[] = [
                // 关键字
                { label: 'module', kind: monaco.languages.CompletionItemKind.Keyword, insertText: 'module ${1:Name} where\n\n$0', range },
                { label: 'type', kind: monaco.languages.CompletionItemKind.Keyword, insertText: 'type ${1:Name} : ${2:U₁}', range },
                { label: 'def', kind: monaco.languages.CompletionItemKind.Keyword, insertText: 'def ${1:name} : ${2:Type}\n  = ${3:expression}', range },
                { label: 'let', kind: monaco.languages.CompletionItemKind.Keyword, insertText: 'let ${1:x} = ${2:value}\nin ${3:body}', range },
                { label: 'λ', kind: monaco.languages.CompletionItemKind.Function, insertText: 'λ(${1:x} : ${2:Type}) → ${3:body}', range },
                // 类型构造器
                { label: 'Σ', kind: monaco.languages.CompletionItemKind.Class, insertText: 'Σ(${1:x} : ${2:A}) × ${3:B}', range },
                { label: 'Π', kind: monaco.languages.CompletionItemKind.Class, insertText: 'Π(${1:x} : ${2:A}) → ${3:B}', range },
                // Universe类型
                { label: 'U₁', kind: monaco.languages.CompletionItemKind.TypeParameter, insertText: 'U₁', range },
                { label: 'Type₁', kind: monaco.languages.CompletionItemKind.TypeParameter, insertText: 'Type₁', range },
                { label: 'Prop', kind: monaco.languages.CompletionItemKind.TypeParameter, insertText: 'Prop', range }
            ];

            return { suggestions };
        }
    });
}

