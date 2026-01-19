# KOS-TL VS Code 扩展开发计划

## 1. 扩展概述

开发一个完整的 VS Code 扩展，为 KOS-TL 语言提供：
- 语法高亮
- 代码补全
- 类型提示
- 错误诊断
- 证明辅助
- 代码格式化
- 调试支持

## 2. 技术栈

- **语言**: TypeScript
- **框架**: VS Code Extension API
- **语言服务器**: Language Server Protocol (LSP)
- **解析器**: 复用编译器前端（或独立实现）

## 3. 功能设计

### 3.1 语法高亮

使用 TextMate 语法定义文件 (`syntaxes/kos-tl.tmLanguage.json`)

```json
{
  "scopeName": "source.kos-tl",
  "patterns": [
    {
      "name": "keyword.control.kos-tl",
      "match": "\\b(module|where|import|type|def|let|in|if|then|else|match|with)\\b"
    },
    {
      "name": "keyword.operator.kos-tl",
      "match": "(→|Σ|Π|λ|×|\\+|::|==|!=|<|>|<=|>=)"
    },
    {
      "name": "entity.name.type.kos-tl",
      "match": "\\b[A-Z][a-zA-Z0-9_']*\\b"
    }
  ]
}
```

### 3.2 代码补全

实现 CompletionItemProvider：

```typescript
import * as vscode from 'vscode';

export class KOSTLCompletionProvider implements vscode.CompletionItemProvider {
    provideCompletionItems(
        document: vscode.TextDocument,
        position: vscode.Position,
        token: vscode.CancellationToken
    ): vscode.ProviderResult<vscode.CompletionItem[]> {
        const items: vscode.CompletionItem[] = [];
        
        // 关键字补全
        items.push({
            label: 'module',
            kind: vscode.CompletionItemKind.Keyword,
            detail: 'Module declaration'
        });
        
        // 类型构造器补全
        items.push({
            label: 'Σ',
            kind: vscode.CompletionItemKind.Class,
            detail: 'Dependent sum type',
            insertText: 'Σ(${1:x} : ${2:A}) → ${3:B}'
        });
        
        // 函数补全（基于类型检查结果）
        // ...
        
        return items;
    }
}
```

### 3.3 类型提示 (Hover)

实现 HoverProvider：

```typescript
export class KOSTLHoverProvider implements vscode.HoverProvider {
    provideHover(
        document: vscode.TextDocument,
        position: vscode.Position,
        token: vscode.CancellationToken
    ): vscode.ProviderResult<vscode.Hover> {
        // 获取当前位置的标识符
        const wordRange = document.getWordRangeAtPosition(position);
        if (!wordRange) return null;
        
        const word = document.getText(wordRange);
        
        // 查询类型信息（调用语言服务器）
        const typeInfo = this.getTypeInfo(document, wordRange);
        
        if (typeInfo) {
            return new vscode.Hover({
                language: 'kos-tl',
                value: `**Type**: ${typeInfo.type}\n\n${typeInfo.documentation}`
            });
        }
        
        return null;
    }
}
```

### 3.4 错误诊断

实现 DiagnosticProvider：

```typescript
export class KOSTLDiagnosticProvider {
    private diagnosticCollection: vscode.DiagnosticCollection;
    
    constructor() {
        this.diagnosticCollection = 
            vscode.languages.createDiagnosticCollection('kos-tl');
    }
    
    updateDiagnostics(document: vscode.TextDocument) {
        // 调用编译器进行类型检查
        const diagnostics = this.checkDocument(document);
        this.diagnosticCollection.set(document.uri, diagnostics);
    }
    
    private checkDocument(document: vscode.TextDocument): vscode.Diagnostic[] {
        const diagnostics: vscode.Diagnostic[] = [];
        
        // 解析文档
        const ast = this.parse(document.getText());
        
        // 类型检查
        const errors = this.typeCheck(ast);
        
        // 转换为 VS Code Diagnostic
        for (const error of errors) {
            diagnostics.push({
                range: new vscode.Range(
                    error.line, error.column,
                    error.line, error.column + error.length
                ),
                message: error.message,
                severity: vscode.DiagnosticSeverity.Error,
                source: 'kos-tl'
            });
        }
        
        return diagnostics;
    }
}
```

### 3.5 语言服务器 (LSP)

实现完整的 Language Server：

```typescript
import {
    createConnection,
    TextDocuments,
    Diagnostic,
    DiagnosticSeverity,
    InitializeResult,
    TextDocumentSyncKind
} from 'vscode-languageserver/node';
import { TextDocument } from 'vscode-languageserver-textdocument';

const connection = createConnection();
const documents = new TextDocuments(TextDocument);

connection.onInitialize((params): InitializeResult => {
    return {
        capabilities: {
            textDocumentSync: TextDocumentSyncKind.Incremental,
            completionProvider: {
                triggerCharacters: ['.', ':', '→']
            },
            hoverProvider: true,
            definitionProvider: true,
            referencesProvider: true,
            documentSymbolProvider: true,
            codeActionProvider: true
        }
    };
});

// 文档变更时更新诊断
documents.onDidChangeContent(change => {
    validateDocument(change.document);
});

function validateDocument(textDocument: TextDocument): void {
    // 调用编译器进行类型检查
    const diagnostics: Diagnostic[] = [];
    
    // ... 类型检查逻辑
    
    connection.sendDiagnostics({
        uri: textDocument.uri,
        diagnostics
    });
}

documents.listen(connection);
connection.listen();
```

## 4. 项目结构

```
kos-tl-vscode-extension/
├── package.json              # 扩展配置
├── tsconfig.json            # TypeScript 配置
├── src/
│   ├── extension.ts         # 扩展入口
│   ├── completion.ts        # 代码补全
│   ├── hover.ts             # 悬停提示
│   ├── diagnostics.ts       # 错误诊断
│   ├── languageServer.ts    # 语言服务器
│   └── compiler/
│       ├── lexer.ts         # 词法分析（TypeScript 版本）
│       ├── parser.ts         # 语法分析
│       └── typeChecker.ts   # 类型检查
├── syntaxes/
│   └── kos-tl.tmLanguage.json  # 语法高亮定义
├── snippets/
│   └── kos-tl.json          # 代码片段
├── .vscode/
│   └── launch.json          # 调试配置
└── README.md
```

## 5. package.json 配置

```json
{
  "name": "kos-tl",
  "displayName": "KOS-TL Language Support",
  "description": "Language support for KOS-TL",
  "version": "0.1.0",
  "engines": {
    "vscode": "^1.60.0"
  },
  "categories": ["Programming Languages"],
  "activationEvents": ["onLanguage:kos-tl"],
  "main": "./out/extension.js",
  "contributes": {
    "languages": [{
      "id": "kos-tl",
      "aliases": ["KOS-TL", "kos-tl"],
      "extensions": [".kos"],
      "configuration": "./language-configuration.json"
    }],
    "grammars": [{
      "language": "kos-tl",
      "scopeName": "source.kos-tl",
      "path": "./syntaxes/kos-tl.tmLanguage.json"
    }],
    "snippets": [{
      "language": "kos-tl",
      "path": "./snippets/kos-tl.json"
    }],
    "commands": [{
      "command": "kos-tl.check",
      "title": "Check Type"
    }]
  },
  "scripts": {
    "compile": "tsc -p ./",
    "watch": "tsc -watch -p ./"
  },
  "dependencies": {
    "vscode-languageclient": "^7.0.0",
    "vscode-languageserver": "^7.0.0",
    "vscode-languageserver-textdocument": "^1.0.0"
  },
  "devDependencies": {
    "@types/vscode": "^1.60.0",
    "@types/node": "^16.0.0",
    "typescript": "^4.5.0"
  }
}
```

## 6. 实现步骤

### 阶段 1：基础功能 (2-3 周)

1. 项目初始化
2. 语法高亮
3. 代码片段
4. 基础代码补全

### 阶段 2：语言服务器 (3-4 周)

1. LSP 服务器实现
2. 类型检查集成
3. 错误诊断
4. 悬停提示

### 阶段 3：高级功能 (2-3 周)

1. 代码格式化
2. 重构支持
3. 符号导航
4. 代码操作

### 阶段 4：调试支持 (2-3 周)

1. 调试适配器
2. 断点支持
3. 变量查看
4. 调用栈

## 7. 开发工具

- **VS Code Extension Development Host**: 调试扩展
- **yo code**: 扩展脚手架
- **vscode-extension-tester**: 集成测试

## 8. 发布计划

1. **Alpha 版本** (0.1.0): 基础语法高亮和补全
2. **Beta 版本** (0.5.0): 完整的语言服务器支持
3. **正式版本** (1.0.0): 完整功能 + 文档

## 9. 下一步行动

1. 创建扩展项目结构
2. 实现语法高亮
3. 实现基础补全
4. 集成语言服务器
5. 测试和优化
















