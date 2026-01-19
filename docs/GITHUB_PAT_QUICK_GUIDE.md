# GitHub Personal Access Token 快速创建指南 / GitHub PAT Quick Creation Guide

[中文](#中文) | [English](#english)

---

<a name="中文"></a>
## 中文

## ⚠️ 重要：GitHub 不再支持密码认证

从 2021 年 8 月起，GitHub 要求使用 **Personal Access Token (PAT)** 或 **SSH 密钥**进行 Git 操作。

## 快速创建 PAT（5 分钟）

### 步骤 1：登录 GitHub
访问：https://github.com/login

### 步骤 2：进入 Token 设置
1. 点击右上角**头像** → **Settings**
2. 滚动到底部 → **Developer settings**
3. 点击 **Personal access tokens** → **Tokens (classic)**
4. 点击 **Generate new token** → **Generate new token (classic)**

### 步骤 3：配置 Token
- **Note**: `KosL Project` (描述用途)
- **Expiration**: 选择过期时间（建议 90 天或自定义）
- **Select scopes**: 勾选：
  - ✅ **`repo`** (完整仓库访问权限) - **必须**
  - ✅ **`workflow`** (如果需要 GitHub Actions)

### 步骤 4：生成并复制 Token
1. 点击 **Generate token**
2. **立即复制 token**（格式：`ghp_xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx`）
   - ⚠️ **只显示一次！** 关闭页面后无法再次查看
   - 如果丢失，需要重新生成

### 步骤 5：使用 Token 推送代码

**方式 1：在 URL 中嵌入 Token（一次性）**
```bash
git remote set-url origin https://<YOUR_TOKEN>@github.com/chenpengLeibniz/KosL.git
git push -u origin main
```

**方式 2：使用 Git Credential Manager（推荐）**
```bash
# 设置远程仓库（不包含 token）
git remote set-url origin https://github.com/chenpengLeibniz/KosL.git

# 推送时会提示输入：
# Username: chenpengLeibniz
# Password: <粘贴你的 PAT token，不是账户密码>
git push -u origin main
```

**方式 3：配置 Git Credential Helper（Windows）**
```bash
# 配置凭证助手
git config --global credential.helper manager-core

# 推送时会弹出 Windows 凭证管理器
# 输入用户名和 PAT token
git push -u origin main
```

## 安全提示

1. ✅ **不要将 PAT 提交到代码仓库**
2. ✅ **定期轮换 PAT**（每 90 天）
3. ✅ **使用最小权限原则**（只授予必要的权限）
4. ✅ **如果 PAT 泄露，立即撤销并重新生成**

## 撤销 Token

如果需要撤销 Token：
1. Settings → **Developer settings** → **Personal access tokens** → **Tokens (classic)**
2. 找到对应的 Token
3. 点击 **Revoke**（撤销）

## 故障排除

### 问题：认证失败
- 确认使用的是 PAT 而不是密码
- 检查 PAT 是否过期
- 确认 PAT 有 `repo` 权限

### 问题：仓库不存在
- 先在 GitHub 网页创建仓库：https://github.com/new
- 仓库名称：`KosL`
- 不要初始化 README、.gitignore 或 license

---

<a name="english"></a>
## English

# GitHub Personal Access Token Quick Creation Guide

## ⚠️ Important: GitHub No Longer Supports Password Authentication

As of August 2021, GitHub requires **Personal Access Token (PAT)** or **SSH keys** for Git operations.

## Quick PAT Creation (5 minutes)

### Step 1: Log in to GitHub
Visit: https://github.com/login

### Step 2: Access Token Settings
1. Click your profile picture in the top right → **Settings**
2. Scroll to the bottom → **Developer settings**
3. Click **Personal access tokens** → **Tokens (classic)**
4. Click **Generate new token** → **Generate new token (classic)**

### Step 3: Configure Token
- **Note**: `KosL Project` (describe purpose)
- **Expiration**: Choose expiration time (recommend 90 days or custom)
- **Select scopes**: Check:
  - ✅ **`repo`** (full repository access) - **Required**
  - ✅ **`workflow`** (if GitHub Actions needed)

### Step 4: Generate and Copy Token
1. Click **Generate token**
2. **Immediately copy the token** (format: `ghp_xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx`)
   - ⚠️ **Only shown once!** Cannot view again after closing the page
   - If lost, need to regenerate

### Step 5: Use Token to Push Code

**Method 1: Embed Token in URL (one-time)**
```bash
git remote set-url origin https://<YOUR_TOKEN>@github.com/chenpengLeibniz/KosL.git
git push -u origin main
```

**Method 2: Use Git Credential Manager (Recommended)**
```bash
# Set remote repository (without token)
git remote set-url origin https://github.com/chenpengLeibniz/KosL.git

# When pushing, you'll be prompted for:
# Username: chenpengLeibniz
# Password: <paste your PAT token, not account password>
git push -u origin main
```

**Method 3: Configure Git Credential Helper (Windows)**
```bash
# Configure credential helper
git config --global credential.helper manager-core

# Windows Credential Manager will pop up when pushing
# Enter username and PAT token
git push -u origin main
```

## Security Tips

1. ✅ **Do not commit PAT to code repository**
2. ✅ **Rotate PAT regularly** (every 90 days)
3. ✅ **Use principle of least privilege** (only grant necessary permissions)
4. ✅ **If PAT is leaked, immediately revoke and regenerate**

## Revoke Token

If you need to revoke a token:
1. Settings → **Developer settings** → **Personal access tokens** → **Tokens (classic)**
2. Find the corresponding token
3. Click **Revoke**

## Troubleshooting

### Issue: Authentication Failed
- Confirm you're using PAT, not password
- Check if PAT has expired
- Confirm PAT has `repo` permission

### Issue: Repository Not Found
- First create repository on GitHub web: https://github.com/new
- Repository name: `KosL`
- Do not initialize README, .gitignore, or license
