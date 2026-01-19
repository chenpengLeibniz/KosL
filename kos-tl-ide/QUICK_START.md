# KOS-TL IDE 快速开始

## 安装和运行

### 1. 安装依赖

```bash
cd kos-tl-ide
npm install
```

### 2. 确保编译器已构建

确保 KOS-TL 编译器已构建：
```bash
cd ../compiler
mkdir build
cd build
cmake ..
cmake --build .
```

编译器应该在 `compiler/build/bin/kos-tl-compiler`。

### 3. 启动开发模式

```bash
cd kos-tl-ide
npm run dev
```

这会：
- 启动 Webpack 开发服务器（端口 3000）
- 编译主进程 TypeScript 代码
- 启动 Electron 应用

### 4. 使用 IDE

1. **打开工作区**: 点击"打开工作区"按钮，选择一个包含 `.kos` 文件的目录
2. **打开文件**: 在文件树中点击文件，或使用"打开文件"菜单
3. **编辑代码**: 在编辑器中编写 KOS-TL 代码
4. **类型检查**: 自动进行实时类型检查（防抖 500ms）
5. **编译**: 点击"编译"按钮编译当前文件

## 功能说明

### 编辑器
- **Monaco Editor**: VS Code 的编辑器核心
- **语法高亮**: KOS-TL 语言语法高亮
- **代码补全**: 关键字、类型构造器、Universe 类型
- **错误标记**: 实时显示类型错误和语法错误

### 侧边栏
- **文件树**: 浏览工作区文件
- **类型视图**: 查看类型信息（待实现）
- **搜索**: 搜索功能（待实现）

### 底部面板
- **类型面板**: 显示当前选中标识符的类型信息
- **错误面板**: 显示所有错误和警告
- **编译面板**: 显示编译输出和错误

### 状态栏
- **文件路径**: 当前打开的文件路径
- **修改状态**: 显示文件是否已修改（*）
- **错误计数**: 显示错误数量

## 快捷键

- `Ctrl+S` / `Cmd+S`: 保存文件
- `Ctrl+O` / `Cmd+O`: 打开文件
- `Ctrl+Shift+P` / `Cmd+Shift+P`: 命令面板（待实现）
- `F5`: 编译当前文件（待实现）

## 配置

### 编译器路径

在 `main/ipc-handlers.ts` 中配置编译器路径：
```typescript
const COMPILER_PATH = path.join(__dirname, '../../compiler/build/bin/kos-tl-compiler');
```

### 开发服务器端口

在 `renderer/webpack.config.js` 中配置：
```javascript
devServer: {
    port: 3000,  // 修改这里
    hot: true
}
```

## 故障排除

### 编译器找不到

确保：
1. 编译器已构建
2. `COMPILER_PATH` 配置正确
3. 编译器可执行文件有执行权限

### 编辑器不显示

检查：
1. Monaco Editor 是否正确加载
2. Webpack 开发服务器是否运行
3. 浏览器控制台是否有错误

### 类型检查不工作

检查：
1. 编译器路径是否正确
2. IPC 通信是否正常
3. 临时文件是否创建成功

## 下一步

- [ ] 实现语言服务器（LSP）
- [ ] 添加代码格式化
- [ ] 实现定义跳转
- [ ] 添加调试支持
- [ ] 实现证明辅助功能





