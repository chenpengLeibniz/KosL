# 提交并推送到 GitHub (KOS-TL/KosL)

因当前有 Git 锁文件占用，请在本机**新开一个终端**（或关闭所有使用本仓库的 Cursor/IDE 后再试），在仓库根目录执行以下命令。

## 1. 若提示 index.lock 存在

先删除锁文件（确认没有其他 Git 或编辑器在使用本仓库）：

```powershell
# PowerShell
Remove-Item -Force D:\post\KOS\KosL\.git\index.lock -ErrorAction SilentlyContinue
```

或手动删除 `D:\post\KOS\KosL\.git\index.lock`。

## 2. 暂存、提交并推送

```powershell
cd D:\post\KOS\KosL

# 暂存全部变更（.gitignore 已包含 coc/target/，不会提交 Rust 构建产物）
git add -A

# 提交（可按需修改提交信息）
git commit -m "feat: kos-web 自动演化、Core 宪法约束、文档中英文更新

- Runtime: 自动演化(auto_evolve)、重放并演化、日志保存与回放
- Kernel: 对象创建必须通过 Γ（Core）；可选 kos-core check-term；RootCauseReport 仅当在 Γ 中才写入 K
- 文档: README/README_EN, CORE_CONSTITUTION, SCENARIOS_RUNTIME_SIGNALS 中英文版；docs 索引
- .gitignore: 忽略 coc/target/"

# 推送到 GitHub
git push origin main
```

## 3. 若 push 需要认证

- **HTTPS**：会提示输入 GitHub 用户名与密码（密码处使用 Personal Access Token）。
- **SSH**：若已配置 `git@github.com:KOS-TL/KosL.git`，可先执行 `git remote set-url origin git@github.com:KOS-TL/KosL.git` 再 `git push origin main`。

仓库地址：<https://github.com/KOS-TL/KosL>。
