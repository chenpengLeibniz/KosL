# GitHub 双因素认证（2FA）配置指南

## 什么是双因素认证（2FA）？

双因素认证是一种安全机制，要求你在登录时提供两种不同的验证方式：
1. **你知道的**：密码
2. **你拥有的**：手机应用生成的验证码、短信验证码或安全密钥

启用 2FA 后，即使他人获得你的密码，也无法登录你的账户。

## 配置 2FA 的步骤

### 步骤 1：进入安全设置

1. 登录 GitHub
2. 点击右上角头像 → **Settings**（设置）
3. 在左侧菜单中找到并点击 **Password and authentication**（密码和身份验证）
4. 在 "Two-factor authentication"（双因素认证）部分，点击 **Enable two-factor authentication**（启用双因素认证）

### 步骤 2：选择 2FA 方法

GitHub 提供三种方式配置 2FA：

#### 方法 A：使用身份验证器应用（推荐）

**优点：**
- ✅ 最安全
- ✅ 不需要手机信号
- ✅ 支持离线使用

**所需工具：**
- Android/iOS 设备
- 身份验证器应用，如：
  - **Authenticator**（Microsoft）
  - **Google Authenticator**
  - **Authy**
  - **1Password**

**配置步骤：**

1. 点击 **Set up using an app**（使用应用设置）

2. **下载并安装身份验证器应用**（如果还没有）：
   - Android: Google Play 搜索 "Authenticator"
   - iOS: App Store 搜索 "Authenticator"
   - 推荐：Microsoft Authenticator 或 Google Authenticator

3. **扫描二维码**：
   - GitHub 会显示一个二维码
   - 打开你的身份验证器应用
   - 选择 "添加账户" 或 "扫描二维码"
   - 扫描 GitHub 显示的二维码

4. **验证设置**：
   - 身份验证器应用会生成一个 6 位数字验证码
   - 在 GitHub 页面上输入这个验证码
   - 点击 **Continue**（继续）

5. **保存恢复代码**（非常重要！）：
   - GitHub 会显示 8 个恢复代码（Recovery codes）
   - **立即保存这些代码**（打印、下载或复制到安全的地方）
   - 如果丢失手机，可以使用这些代码恢复账户访问

6. **确认完成**：
   - 勾选 "I have saved my recovery codes"（我已保存恢复代码）
   - 点击 **Continue**（继续）

#### 方法 B：使用短信（SMS）

**优点：**
- ✅ 简单易用
- ✅ 不需要额外应用

**缺点：**
- ⚠️ 需要手机信号
- ⚠️ 如果手机丢失，可能无法接收短信
- ⚠️ 安全性低于应用

**配置步骤：**

1. 点击 **Set up using SMS**（使用短信设置）

2. **选择国家/地区**：
   - 选择你的国家或地区代码

3. **输入手机号码**：
   - 输入你的手机号码（不需要输入国家代码）
   - 点击 **Send authentication code**（发送验证码）

4. **输入验证码**：
   - 查看手机短信
   - 输入收到的 6 位数字验证码
   - 点击 **Continue**（继续）

5. **保存恢复代码**：
   - 同样需要保存恢复代码

6. **确认完成**

#### 方法 C：使用安全密钥（Security Key）

**优点：**
- ✅ 非常安全
- ✅ 支持 FIDO2/WebAuthn 标准

**所需工具：**
- USB 安全密钥（如 YubiKey）或支持 WebAuthn 的设备

**配置步骤：**

1. 点击 **Set up using a security key**（使用安全密钥设置）

2. **连接安全密钥**：
   - 将安全密钥插入 USB 端口（或使用 NFC/蓝牙）

3. **注册密钥**：
   - 按照浏览器提示操作
   - 可能需要触摸或按下安全密钥上的按钮

4. **命名密钥**：
   - 给安全密钥起一个名称（如 "我的 YubiKey"）

5. **保存恢复代码**

## 重要提示

### ⚠️ 保存恢复代码

**恢复代码的重要性：**
- 如果你的手机丢失或应用被删除
- 如果无法接收短信
- 恢复代码是唯一访问账户的方式

**保存方式：**
- 下载为文件保存在安全的地方
- 打印出来存放在安全的位置
- 复制到密码管理器

**千万不要：**
- ❌ 只保存在电脑上（可能丢失）
- ❌ 截图后删除原文件
- ❌ 分享给他人

### 🔒 安全建议

1. **使用身份验证器应用**（而不是 SMS）：
   - 更安全
   - 不受 SIM 卡劫持影响

2. **启用多个 2FA 方法**：
   - 可以同时启用应用 + SMS
   - 提供备份选项

3. **定期检查 2FA 设置**：
   - 确保手机应用正常运行
   - 确认手机号码是最新的

4. **使用密码管理器**：
   - 安全存储恢复代码
   - 管理多个账户的 2FA

## 验证 2FA 是否启用

### 方法 1：查看设置页面

1. Settings → **Password and authentication**
2. 在 "Two-factor authentication" 部分应该显示：
   - ✅ **Enabled**（已启用）
   - 当前使用的方法（应用/SMS/安全密钥）
   - 恢复代码下载选项

### 方法 2：尝试登录

1. 登出 GitHub
2. 尝试重新登录
3. 输入密码后，应该会要求输入 2FA 验证码

## 管理 2FA 设置

### 添加额外的 2FA 方法

1. Settings → **Password and authentication** → **Two-factor authentication**
2. 在 "Additional methods"（额外方法）部分
3. 可以添加：
   - 另一个身份验证器应用
   - 另一个手机号码
   - 另一个安全密钥

### 重新生成恢复代码

1. Settings → **Password and authentication** → **Two-factor authentication**
2. 点击 **Recovery codes**（恢复代码）
3. 点击 **Generate new recovery codes**（生成新恢复代码）
4. **重要**：生成新代码后，旧代码将失效

### 禁用 2FA

**不推荐禁用 2FA**，但如果确实需要：

1. Settings → **Password and authentication** → **Two-factor authentication**
2. 滚动到底部
3. 点击 **Disable two-factor authentication**（禁用双因素认证）
4. 输入密码确认

## 常见问题

### Q: 手机丢失了怎么办？

**A:** 使用恢复代码：
1. 登录 GitHub
2. 当提示输入 2FA 验证码时
3. 点击 "Use a recovery code"（使用恢复代码）
4. 输入一个保存的恢复代码

### Q: 恢复代码丢失了怎么办？

**A:** 
1. 如果你仍能使用 2FA 登录：
   - Settings → 生成新的恢复代码
2. 如果无法登录：
   - 联系 GitHub 支持：https://support.github.com/contact

### Q: 可以同时使用多种 2FA 方法吗？

**A:** 是的！可以同时使用：
- 身份验证器应用
- SMS
- 安全密钥

建议至少配置两种方法作为备份。

### Q: 2FA 会影响 Git 操作吗？

**A:** 
- **命令行 Git**：不受影响（使用 SSH 密钥或 Personal Access Token）
- **GitHub Desktop**：不受影响（使用保存的凭证）
- **网页操作**：需要 2FA 验证码

### Q: 如何为命令行 Git 生成 Personal Access Token？

**A:** 
1. Settings → **Developer settings** → **Personal access tokens** → **Tokens (classic)**
2. 点击 **Generate new token**（生成新令牌）
3. 选择所需权限
4. 生成后**立即复制**（只显示一次）
5. 使用这个 token 代替密码进行 Git 操作

## 相关资源

- GitHub 官方文档：https://docs.github.com/en/authentication/securing-your-account-with-two-factor-authentication-2fa
- GitHub 支持：https://support.github.com/
- 密码管理器推荐：1Password, Bitwarden, LastPass

## 总结

配置 2FA 是保护 GitHub 账户安全的重要步骤：

1. ✅ 选择身份验证器应用（最推荐）
2. ✅ 保存恢复代码（非常重要！）
3. ✅ 考虑配置多个 2FA 方法作为备份
4. ✅ 定期检查 2FA 设置

完成配置后，你的账户安全性将大大提升！
