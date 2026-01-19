#!/bin/bash

# KOS-TL IDE 构建脚本

set -e

echo "开始构建 KOS-TL IDE..."

# 检查 Node.js
if ! command -v node &> /dev/null; then
    echo "错误: 未找到 Node.js，请先安装 Node.js 16+"
    exit 1
fi

# 检查 npm
if ! command -v npm &> /dev/null; then
    echo "错误: 未找到 npm"
    exit 1
fi

echo "1. 安装依赖..."
npm install

echo "2. 清理旧的构建文件..."
npm run clean || true

echo "3. 构建主进程..."
npm run build:main

echo "4. 构建渲染进程..."
npm run build:renderer

echo "5. 检查构建结果..."
if [ ! -d "dist" ]; then
    echo "错误: 构建失败，dist 目录不存在"
    exit 1
fi

echo "构建完成！"
echo ""
echo "要打包为可执行文件，请运行:"
echo "  npm run package        # 当前平台"
echo "  npm run package:win    # Windows"
echo "  npm run package:mac    # macOS"
echo "  npm run package:linux  # Linux"





