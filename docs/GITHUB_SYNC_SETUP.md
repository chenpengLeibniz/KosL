# GitHub 同步设置指南

## ⚠️ 重要安全提示

**GitHub 从 2021 年 8 月起不再支持密码认证！**

必须使用以下方式之一：
1. **Personal Access Token (PAT)** - 推荐用于 HTTPS
2. **SSH 密钥** - 推荐用于长期使用

## 方法 1：使用 Personal Access Token (PAT) - 推荐

### 步骤 1：生成 Personal Access Token

1. 登录 GitHub (https://github.com)
2. 点击右上角头像 → **Settings**
3. 左侧菜单最底部 → **Developer settings**
4. 点击 **Personal access tokens** → **Tokens (classic)**
5. 点击 **Generate new token** → **Generate new token (classic)**
6. 填写信息：
   - **Note**: `KosL Project Sync` (描述用途)
   - **Expiration**: 选择过期时间（建议 90 天或自定义）
   - **Select scopes**: 勾选以下权限：
     - ✅ `repo` (完整仓库访问权限)
     - ✅ `workflow` (如果需要 Actions)
7. 点击 **Generate token**
8. **立即复制 token**（只显示一次！）
   - 格式类似：`ghp_xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx`

### 步骤 2：使用 Token 进行 Git 操作

**方式 A：在 URL 中使用 Token**
```bash
git remote set-url origin https://<TOKEN>@github.com/chenpengLeibniz/KosL.git
```

**方式 B：使用 Git Credential Manager（推荐）**
```bash
# 设置远程仓库
git remote set-url origin https://github.com/chenpengLeibniz/KosL.git

# 推送时会提示输入用户名和密码
# 用户名：chenpengLeibniz
# 密码：粘贴你的 PAT token（不是账户密码）
```

**方式 C：在 Git 配置中保存（Windows）**
```bash
# 使用 Git Credential Manager
git config --global credential.helper manager-core

# 首次推送时会弹出窗口，输入：
# Username: chenpengLeibniz
# Password: <你的 PAT token>
```

## 方法 2：使用 SSH 密钥（长期推荐）

### 步骤 1：生成 SSH 密钥

```bash
# 检查是否已有 SSH 密钥
ls ~/.ssh

# 如果没有，生成新密钥
ssh-keygen -t ed25519 -C "your_email@example.com"

# 按 Enter 使用默认路径
# 设置密码（可选，但推荐）
```

### 步骤 2：添加 SSH 密钥到 GitHub

1. 复制公钥内容：
```bash
# Windows PowerShell
cat ~/.ssh/id_ed25519.pub | clip

# 或手动打开文件复制内容
notepad ~/.ssh/id_ed25519.pub
```

2. 在 GitHub 添加密钥：
   - Settings → **SSH and GPG keys**
   - 点击 **New SSH key**
   - **Title**: `My Computer` (描述)
   - **Key**: 粘贴公钥内容
   - 点击 **Add SSH key**

### 步骤 3：使用 SSH URL

```bash
git remote set-url origin git@github.com:chenpengLeibniz/KosL.git
```

## 当前项目设置步骤

### 1. 创建 GitHub 仓库（如果还没有）

访问：https://github.com/new
- **Repository name**: `KosL`
- **Description**: (可选)
- **Visibility**: Public 或 Private
- **不要**初始化 README、.gitignore 或 license（因为本地已有）
- 点击 **Create repository**

### 2. 更新远程仓库 URL

**使用 HTTPS + PAT：**
```bash
git remote set-url origin https://github.com/chenpengLeibniz/KosL.git
```

**使用 SSH：**
```bash
git remote set-url origin git@github.com:chenpengLeibniz/KosL.git
```

### 3. 添加、提交并推送

```bash
# 添加所有更改
git add .

# 提交（使用英文）
git commit -m "Add compiler, IDE, and documentation"

# 推送到新仓库
git push -u origin main
```

## 安全最佳实践

1. ✅ **使用 PAT 而不是密码**
2. ✅ **定期轮换 PAT**（每 90 天）
3. ✅ **使用最小权限原则**（只授予必要的权限）
4. ✅ **不要将 PAT 提交到代码仓库**
5. ✅ **使用 SSH 密钥进行长期开发**（更安全）

## 故障排除

### 问题：认证失败

**解决方案：**
- 确认使用的是 PAT 而不是密码
- 检查 PAT 是否过期
- 确认 PAT 有 `repo` 权限

### 问题：仓库不存在

**解决方案：**
- 先在 GitHub 网页创建仓库
- 确认仓库名称和用户名正确

### 问题：权限被拒绝

**解决方案：**
- 检查 PAT 权限范围
- 确认账户有仓库访问权限
