# GitHub 2FA 快速配置指南

## 5 分钟快速配置

### 步骤 1：打开设置页面
```
GitHub → 头像 → Settings → Password and authentication → Enable two-factor authentication
```

### 步骤 2：选择方法（推荐身份验证器应用）

#### 推荐：使用手机应用（最安全）

1. **下载应用**（选择一个）：
   - Microsoft Authenticator（推荐）
   - Google Authenticator
   - Authy

2. **扫描二维码**：
   - 在应用中点击"添加账户"
   - 扫描 GitHub 显示的二维码

3. **输入验证码**：
   - 应用会显示 6 位数字
   - 输入到 GitHub 页面

4. **保存恢复代码** ⚠️ **必须保存！**：
   - GitHub 会显示 8 个恢复代码
   - 下载或复制保存到安全的地方
   - 如果手机丢失，需要这些代码

### 步骤 3：完成
- 勾选 "I have saved my recovery codes"
- 点击 Continue

## 必做事项

- ✅ **保存恢复代码**（最重要！）
- ✅ 测试登录确认 2FA 正常工作
- ✅ 可以考虑同时启用 SMS 作为备份

## 如果手机丢失怎么办？

使用恢复代码登录，然后：
1. Settings → Two-factor authentication
2. 移除丢失的设备
3. 添加新的设备

## 需要详细步骤？

查看完整指南：`docs/GITHUB_2FA_SETUP.md`
