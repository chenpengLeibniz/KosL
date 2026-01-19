# KOS-TL IDE

基于 Electron 和 Monaco Editor 构建的专门针对 KOS-TL 语言的集成开发环境。

## 概述

KOS-TL IDE 是一个完整的、专门为 KOS-TL 语言定制的开发环境，提供：
- ✅ 完整的代码编辑体验（基于 Monaco Editor）
- ✅ 实时类型检查和错误诊断
- ✅ 集成编译器
- ✅ 文件树导航
- ✅ 编译输出面板
- ⏳ 可视化类型系统
- ⏳ 证明辅助工具
- ⏳ 领域特定功能（金融、制造业等）

## 特性

### 与 VS Code 插件的区别

| 特性 | VS Code 插件 | KOS-TL IDE |
|------|-------------|-----------|
| **形式** | 插件 | 独立应用 |
| **安装** | 插件市场 | 独立安装包 |
| **UI定制** | 受限 | 完全定制 |
| **性能** | 受VS Code限制 | 可优化 |
| **集成** | 插件API | 原生集成 |
| **用户体验** | 通用编辑器 | 专门优化 |

### 核心功能

1. **代码编辑**
   - Monaco Editor（VS Code 编辑器核心）
   - 语法高亮
   - 代码补全
   - 智能提示
   - 错误标记

2. **类型系统**
   - 实时类型检查（防抖 500ms）
   - 类型信息面板
   - 错误诊断

3. **编译器集成**
   - 一键编译
   - 编译输出显示
   - 错误定位

4. **项目管理**
   - 工作区支持
   - 文件树导航
   - 多文件编辑

## 快速开始

### 安装依赖

```bash
cd kos-tl-ide
npm install
```

### 开发模式

```bash
npm run dev
```

### 构建

```bash
npm run build
```

### 打包

```bash
npm run package
```

详细说明请参考 [QUICK_START.md](./QUICK_START.md) 和 [BUILD.md](./BUILD.md)。

## 项目结构

```
kos-tl-ide/
├── main/                    # Electron 主进程
│   ├── main.ts             # 主入口
│   ├── preload.ts          # 预加载脚本
│   ├── ipc-handlers.ts     # IPC 处理器
│   └── file-system.ts      # 文件系统操作
├── renderer/                # 渲染进程（Web UI）
│   ├── src/
│   │   ├── App.tsx         # 主应用
│   │   ├── components/     # React 组件
│   │   ├── hooks/          # React Hooks
│   │   └── monaco/         # Monaco Editor 集成
│   └── webpack.config.js   # Webpack 配置
├── package.json
└── README.md
```

## 技术栈

- **Electron**: 桌面应用框架
- **Monaco Editor**: VS Code 编辑器核心
- **React**: UI 框架
- **TypeScript**: 主要开发语言
- **Webpack**: 构建工具

## 功能状态

- [x] 项目基础结构
- [x] Electron 主进程
- [x] React 渲染进程
- [x] Monaco Editor 集成
- [x] 语法高亮和代码补全
- [x] 文件树导航
- [x] 实时类型检查
- [x] 编译面板
- [x] 错误诊断
- [ ] 语言服务器（LSP）
- [ ] 代码格式化
- [ ] 调试支持
- [ ] 证明辅助

## 文档

- [QUICK_START.md](./QUICK_START.md) - 快速开始指南
- [BUILD.md](./BUILD.md) - 构建指南
- [ARCHITECTURE.md](./ARCHITECTURE.md) - 架构设计
- [IDE_FEATURES.md](./IDE_FEATURES.md) - 功能特性

## 开发

### 开发模式

```bash
npm run dev
```

这会启动：
- Webpack 开发服务器（端口 3000）
- TypeScript 编译（watch 模式）
- Electron 应用

### 调试

- **主进程**: 在 VS Code 中按 F5
- **渲染进程**: 在 Electron 窗口中按 Ctrl+Shift+I

## 许可证

与主项目相同
