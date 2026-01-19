@echo off
REM KOS-TL IDE 构建脚本 (Windows)

echo 开始构建 KOS-TL IDE...

REM 检查 Node.js
where node >nul 2>nul
if %ERRORLEVEL% NEQ 0 (
    echo 错误: 未找到 Node.js，请先安装 Node.js 16+
    exit /b 1
)

REM 检查 npm
where npm >nul 2>nul
if %ERRORLEVEL% NEQ 0 (
    echo 错误: 未找到 npm
    exit /b 1
)

echo 1. 安装依赖...
call npm install
if %ERRORLEVEL% NEQ 0 (
    echo 错误: 依赖安装失败
    exit /b 1
)

echo 2. 清理旧的构建文件...
call npm run clean
if %ERRORLEVEL% NEQ 0 (
    echo 警告: 清理失败，继续构建...
)

echo 3. 构建主进程...
call npm run build:main
if %ERRORLEVEL% NEQ 0 (
    echo 错误: 主进程构建失败
    exit /b 1
)

echo 4. 构建渲染进程...
call npm run build:renderer
if %ERRORLEVEL% NEQ 0 (
    echo 错误: 渲染进程构建失败
    exit /b 1
)

echo 5. 检查构建结果...
if not exist "dist" (
    echo 错误: 构建失败，dist 目录不存在
    exit /b 1
)

echo.
echo 构建完成！
echo.
echo 要打包为可执行文件，请运行:
echo   npm run package        # 当前平台
echo   npm run package:win    # Windows
echo   npm run package:mac    # macOS
echo   npm run package:linux  # Linux

pause





