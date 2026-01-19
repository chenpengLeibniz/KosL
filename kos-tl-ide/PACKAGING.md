# KOS-TL IDE 打包指南

## 前置要求

### 必需软件
- **Node.js 16+**: [下载地址](https://nodejs.org/)
- **npm**: 通常随 Node.js 一起安装
- **KOS-TL 编译器**: 需要先构建编译器

### 构建编译器

在打包 IDE 之前，需要先构建 KOS-TL 编译器：

```bash
cd ../compiler
mkdir build
cd build
cmake ..
cmake --build . --config Release
```

编译器应该在 `compiler/build/bin/kos-tl-compiler` (Windows) 或 `compiler/build/bin/kos-tl-compiler` (Linux/macOS)。

## 快速开始

### 1. 安装依赖

```bash
cd kos-tl-ide
npm install
```

### 2. 构建项目

**Windows:**
```bash
build.bat
```

**Linux/macOS:**
```bash
chmod +x build.sh
./build.sh
```

或手动执行：
```bash
npm run build
```

### 3. 打包为可执行文件

**当前平台:**
```bash
npm run package
```

**特定平台:**
```bash
npm run package:win    # Windows
npm run package:mac   # macOS
npm run package:linux # Linux
```

## 构建输出

### 开发构建
- 主进程: `dist/main/`
- 渲染进程: `dist/renderer/`

### 打包输出
- Windows: `release/KOS-TL IDE Setup x.x.x.exe` (NSIS 安装程序)
- macOS: `release/KOS-TL IDE-x.x.x.dmg`
- Linux: `release/KOS-TL IDE-x.x.x.AppImage`

## 打包配置

打包配置在 `package.json` 的 `build` 字段中：

```json
{
  "build": {
    "appId": "com.kos-tl.ide",
    "productName": "KOS-TL IDE",
    "directories": {
      "output": "release"
    },
    "files": [
      "dist/**/*",
      "package.json"
    ],
    "extraFiles": [
      {
        "from": "../compiler/build/bin/kos-tl-compiler",
        "to": "compiler/kos-tl-compiler"
      }
    ]
  }
}
```

## 常见问题

### 1. 构建失败：找不到模块

**问题**: `Cannot find module 'xxx'`

**解决**:
```bash
npm install
```

### 2. TypeScript 编译错误

**问题**: TypeScript 类型错误

**解决**:
- 检查 `tsconfig.json` 配置
- 确保所有依赖已安装
- 检查代码中的类型错误

### 3. Webpack 构建失败

**问题**: Webpack 打包错误

**解决**:
- 检查 `webpack.config.js` 配置
- 确保所有资源文件存在
- 检查 CSS 和图片资源路径

### 4. Electron Builder 打包失败

**问题**: `electron-builder` 错误

**解决**:
- 确保已安装所有依赖: `npm install`
- 检查 `package.json` 中的 `build` 配置
- 确保图标文件存在（如果指定了）

### 5. 找不到编译器

**问题**: 打包的应用找不到编译器

**解决**:
- 确保编译器已构建
- 检查 `extraFiles` 配置中的路径
- 在运行时，编译器路径应该是相对于应用目录的

### 6. Monaco Editor 加载失败

**问题**: 编辑器不显示

**解决**:
- 确保 `monaco-editor` 已安装
- 检查 webpack 配置
- 检查浏览器控制台错误

## 开发 vs 生产

### 开发模式
```bash
npm run dev
```
- 使用开发服务器
- 热重载
- 开发者工具

### 生产构建
```bash
npm run build
```
- 优化代码
- 压缩资源
- 生产环境配置

## 平台特定说明

### Windows
- 需要安装 Visual Studio Build Tools（用于 native 模块）
- 输出: NSIS 安装程序
- 图标: `assets/icon.ico`

### macOS
- 需要在 macOS 上构建
- 输出: DMG 文件
- 图标: `assets/icon.icns`
- 可能需要代码签名

### Linux
- 输出: AppImage
- 图标: `assets/icon.png`
- 需要 `fuse` 来运行 AppImage

## 优化建议

1. **减小包大小**:
   - 排除不必要的依赖
   - 使用 `files` 字段精确控制包含的文件
   - 压缩资源

2. **提高性能**:
   - 启用代码分割
   - 优化 Monaco Editor 加载
   - 使用生产模式构建

3. **安全性**:
   - 确保 `contextIsolation` 启用
   - 使用 `preload` 脚本而非 `nodeIntegration`
   - 验证所有 IPC 通信

## 测试打包的应用

### Windows
```bash
# 安装后运行
release/KOS-TL IDE Setup x.x.x.exe
```

### macOS
```bash
# 挂载 DMG
open release/KOS-TL\ IDE-x.x.x.dmg
# 拖拽应用到 Applications
```

### Linux
```bash
# 添加执行权限
chmod +x release/KOS-TL\ IDE-x.x.x.AppImage
# 运行
./release/KOS-TL\ IDE-x.x.x.AppImage
```

## 下一步

- [ ] 添加自动更新功能
- [ ] 实现代码签名（macOS/Windows）
- [ ] 添加崩溃报告
- [ ] 优化启动时间
- [ ] 减小应用体积





