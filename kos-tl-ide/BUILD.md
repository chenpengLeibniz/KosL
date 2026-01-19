# KOS-TL IDE 构建指南

## 前置要求

- Node.js 16+ 
- npm 或 yarn
- 已构建的 KOS-TL 编译器（在 `../compiler/build/bin/kos-tl-compiler`）

## 安装依赖

```bash
cd kos-tl-ide
npm install
```

## 开发模式

```bash
# 启动开发服务器（自动编译和热重载）
npm run dev
```

这会：
1. 编译主进程 TypeScript 代码（watch 模式）
2. 启动 Webpack 开发服务器（渲染进程）
3. 启动 Electron 应用

## 构建

```bash
# 构建所有代码
npm run build

# 只构建主进程
npm run build:main

# 只构建渲染进程
npm run build:renderer
```

## 打包

```bash
# 打包为当前平台的应用
npm run package

# 打包为 Windows 应用
npm run package:win

# 打包为 macOS 应用
npm run package:mac

# 打包为 Linux 应用
npm run package:linux
```

打包后的应用在 `release/` 目录中。

## 项目结构说明

```
kos-tl-ide/
├── main/                    # Electron 主进程
│   ├── main.ts             # 主入口
│   ├── preload.ts          # 预加载脚本
│   └── ipc-handlers.ts     # IPC 处理器
├── renderer/               # 渲染进程（Web）
│   ├── src/
│   │   ├── App.tsx         # 主应用组件
│   │   ├── components/     # React 组件
│   │   └── monaco/         # Monaco Editor 集成
│   ├── index.html          # HTML 模板
│   └── webpack.config.js   # Webpack 配置
├── dist/                   # 构建输出（gitignore）
└── release/                # 打包输出（gitignore）
```

## 调试

### 主进程调试

在 VS Code 中：
1. 打开 `kos-tl-ide` 目录
2. 按 F5 启动调试
3. 选择 "Electron: Main" 配置

### 渲染进程调试

1. 在 Electron 窗口中按 `Ctrl+Shift+I` (Windows/Linux) 或 `Cmd+Option+I` (macOS)
2. 打开开发者工具进行调试

## 常见问题

### 编译器路径错误

确保编译器已构建，并更新 `main/ipc-handlers.ts` 中的 `COMPILER_PATH`。

### Monaco Editor 加载失败

确保 `monaco-editor` 已正确安装：
```bash
npm install monaco-editor
```

### 样式不生效

检查 Webpack 配置中的 CSS loader 是否正确配置。

## 下一步

1. 完善 UI 组件样式
2. 实现文件树
3. 集成语言服务器
4. 添加更多功能面板





