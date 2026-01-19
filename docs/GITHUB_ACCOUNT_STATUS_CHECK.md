# GitHub 账户状态检查指南

## 方法 1：检查账户基本信息

### 步骤 1：登录 GitHub
1. 访问 https://github.com
2. 确保已登录你的账户

### 步骤 2：查看账户设置
1. 点击右上角头像 → **Settings**（设置）
2. 在左侧菜单中，查看以下选项是否可用：
   - **Account settings**（账户设置）
   - **Profile**（个人资料）
   - **Accessibility**（无障碍）
   - 如果这些选项都可见，账户基本正常

### 步骤 3：检查账户邮箱验证状态
1. 在 Settings 中，进入 **Emails**（邮箱）
2. 确认主邮箱旁边有绿色的 "Verified"（已验证）标记
3. 未验证的邮箱可能影响某些功能

### 步骤 4：查看账户限制提示
1. 在 Settings 首页，查看是否有黄色或红色的警告横幅
2. 检查是否有 "Limited"（受限）或 "Suspended"（已暂停）的提示
3. 如果有，说明账户可能被限制了

## 方法 2：测试账户访问权限

### 测试 1：访问自己的用户主页
1. 点击右上角头像 → 点击你的用户名
2. 应该能看到你的个人信息和仓库列表
3. 如果看到错误页面，说明账户可能有问题

### 测试 2：创建或编辑仓库
1. 点击右上角的 "+" 号 → **New repository**
2. 尝试创建一个测试仓库（可以是私有的）
3. 如果无法创建，可能是账户受限

### 测试 3：访问其他公开仓库
1. 尝试访问一个知名的公开仓库，如：
   - https://github.com/microsoft/vscode
   - https://github.com/facebook/react
2. 如果这些都无法访问，可能是网络问题或 GitHub 服务问题

## 方法 3：检查账户状态 API

### 使用 GitHub API 检查账户
```bash
# 替换 YOUR_USERNAME 为你的用户名
curl https://api.github.com/users/YOUR_USERNAME
```

**正常响应应该包含：**
```json
{
  "login": "your-username",
  "id": 12345678,
  "avatar_url": "...",
  "type": "User",
  "site_admin": false
}
```

**如果账户有问题，可能返回：**
- 404 Not Found（账户不存在）
- 403 Forbidden（账户被限制）
- 其他错误代码

## 方法 4：检查邮箱通知

### 查看 GitHub 发送的邮件
1. 检查你的注册邮箱
2. 查看是否有 GitHub 发送的：
   - 账户限制通知
   - 安全警告
   - 账户恢复邮件
3. 如果有异常邮件，说明账户可能有问题

## 方法 5：检查浏览器控制台

### 使用开发者工具
1. 登录 GitHub 后，按 `F12` 打开开发者工具
2. 切换到 **Console**（控制台）标签
3. 查看是否有 JavaScript 错误
4. 切换到 **Network**（网络）标签
5. 刷新页面，查看是否有返回 403、404 或 401 的请求

## 方法 6：检查 GitHub 状态页面

### 查看 GitHub 服务状态
1. 访问 https://www.githubstatus.com/
2. 查看是否有服务中断或故障
3. 如果是 GitHub 服务问题，会影响所有用户

## 常见账户状态问题

### 问题 1：账户被限制（Limited）
**症状：**
- 无法创建新仓库
- 某些功能被禁用
- 公开仓库可能无法被访问

**原因：**
- 违反 GitHub 服务条款
- 发送了垃圾邮件
- 被举报为垃圾账户

**解决方法：**
- 联系 GitHub Support：https://support.github.com/contact

### 问题 2：账户被暂停（Suspended）
**症状：**
- 无法登录
- 所有页面显示错误
- 无法访问任何功能

**原因：**
- 严重违反服务条款
- 安全违规

**解决方法：**
- 查看邮箱中的通知
- 联系 GitHub Support 申诉

### 问题 3：账户被删除
**症状：**
- 无法登录
- 用户名搜索不到
- 所有仓库无法访问

**解决方法：**
- 联系 GitHub Support 恢复账户

## 快速诊断清单

- [ ] 能够登录 GitHub
- [ ] 能够访问 Settings 页面
- [ ] 能够查看自己的用户主页
- [ ] 能够创建新仓库（或已有仓库可见）
- [ ] 邮箱已验证
- [ ] 没有看到限制或暂停的警告
- [ ] API 调用返回正常响应（如使用）

## 如果发现问题

### 联系 GitHub 支持
1. 访问：https://support.github.com/contact
2. 选择相应的问题类型
3. 详细描述问题
4. 附上截图和相关信息

### 提供的信息
- 用户名
- 账户邮箱
- 问题描述
- 错误截图
- 尝试访问的 URL

## 相关链接

- GitHub 状态页面：https://www.githubstatus.com/
- GitHub 支持：https://support.github.com/
- GitHub 社区论坛：https://github.com/orgs/community/discussions
