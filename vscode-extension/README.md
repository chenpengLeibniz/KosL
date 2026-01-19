# KOS-TL VS Code 扩展

为 KOS-TL 语言提供完整的 VS Code 支持。

## 功能

- ✅ 语法高亮
- ✅ 代码补全
- ✅ 悬停提示（类型信息）
- ✅ 错误诊断（语法检查）
- ✅ 代码片段
- ⏳ 语言服务器（LSP）
- ⏳ 实时类型检查
- ⏳ 代码格式化
- ⏳ 调试支持

## 安装

### 从源码安装

```bash
cd vscode-extension
npm install
npm run compile
```

然后在 VS Code 中按 `F5` 启动扩展开发主机。

### 打包安装

```bash
npm install -g vsce
vsce package
code --install-extension kos-tl-0.1.0.vsix
```

## 开发

```bash
# 安装依赖
npm install

# 编译
npm run compile

# 监听模式（自动编译）
npm run watch

# 在 VS Code 中按 F5 启动扩展开发主机
```

## 配置

在 VS Code 设置中可以配置：

- `kos-tl.compilerPath`: KOS-TL 编译器可执行文件路径
- `kos-tl.enableTypeChecking`: 是否启用实时类型检查

## 项目结构

```
vscode-extension/
├── src/
│   ├── extension.ts         # 扩展入口
│   ├── completion.ts        # 代码补全
│   ├── hover.ts             # 悬停提示
│   └── diagnostics.ts       # 错误诊断
├── syntaxes/
│   └── kos-tl.tmLanguage.json  # 语法高亮定义
├── snippets/
│   └── kos-tl.json          # 代码片段
├── language-configuration.json  # 语言配置
├── package.json             # 扩展配置
└── tsconfig.json            # TypeScript 配置
```

## 功能说明

### 语法高亮

支持 KOS-TL 语言的关键字、操作符、类型、标识符等的语法高亮。

### 代码补全

提供以下补全：
- 关键字（module, type, def, let, if, match 等）
- 类型构造器（Σ, Π, λ）
- Universe 类型（U₁, Type₁, Prop）
- 操作符（→, ×）

### 悬停提示

鼠标悬停在标识符上时显示：
- 关键字说明
- 类型信息（TODO: 集成类型检查器）
- 变量信息

### 错误诊断

实时检查：
- 括号匹配
- 基本语法错误
- 类型错误（TODO: 集成编译器）

### 代码片段

提供常用代码模板：
- `module` - 模块声明
- `type` - 类型声明
- `def` - 函数定义
- `lambda` - Lambda 表达式
- `pi` - Pi 类型
- `sigma` - Sigma 类型
- `let` - Let 绑定
- `if` - If 表达式
- `match` - 模式匹配

## 命令

- `KOS-TL: Check Type` - 检查当前文件的类型
- `KOS-TL: Compile to C` - 编译当前文件为 C 代码

## 开发状态

- [x] 项目初始化
- [x] 语法高亮
- [x] 代码补全
- [x] 悬停提示
- [x] 错误诊断（基础）
- [x] 代码片段
- [ ] 语言服务器（LSP）
- [ ] 实时类型检查集成
- [ ] 代码格式化
- [ ] 调试支持

## 下一步

1. 集成编译器进行实时类型检查
2. 实现完整的语言服务器（LSP）
3. 添加代码格式化功能
4. 实现调试支持

## 许可证

与主项目相同
