# KOS-TL IDE 构建和打包指南

## 快速开始

### Windows

```powershell
# 1. 进入项目目录
cd kos-tl-ide

# 2. 安装依赖
npm install

# 3. 构建
npm run build

# 4. 打包
npm run package:win
```

### Linux/macOS

```bash
# 1. 进入项目目录
cd kos-tl-ide

# 2. 安装依赖
npm install

# 3. 构建
npm run build

# 4. 打包
npm run package        # 当前平台
npm run package:win    # Windows
npm run package:mac    # macOS
npm run package:linux  # Linux
```

## 完整流程

### 前置条件

1. **Node.js 16+**: [下载](https://nodejs.org/)
2. **KOS-TL 编译器**: 需要先构建编译器

### 构建编译器

```bash
cd ../compiler
mkdir build && cd build
cmake ..
cmake --build . --config Release
```

### 构建 IDE

```bash
cd kos-tl-ide
npm install
npm run build
```

### 打包

```bash
npm run package:win    # Windows
npm run package:mac    # macOS  
npm run package:linux  # Linux
```

## 输出位置

打包后的文件在 `release/` 目录：
- Windows: `KOS-TL IDE Setup x.x.x.exe`
- macOS: `KOS-TL IDE-x.x.x.dmg`
- Linux: `KOS-TL IDE-x.x.x.AppImage`

## 故障排除

详见 [PACKAGING.md](./PACKAGING.md)





