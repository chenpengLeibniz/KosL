# GitHub API 返回 404 但网页正常访问的原因

## 问题现象

- **网页访问**：`https://github.com/chen-p` 正常
- **API 调用**：`curl https://api.github.com/users/chen-p` 返回 404

## 原因分析

### 1. API 和网页的区别

GitHub API 和网页访问使用不同的机制：

- **网页**：直接访问 HTML 页面，GitHub 会展示公开的账户和仓库信息
- **API**：通过 REST API 获取结构化数据，可能有更严格的权限检查

### 2. 可能的 API 限制原因

#### 原因 A：账户隐私设置
某些账户可能：
- 在 API 中隐藏了用户信息（但网页仍可访问）
- 启用了某些隐私保护功能
- API 访问需要认证 token

#### 原因 B：账户类型或状态
- 新注册的账户可能在 API 中还未完全索引
- 某些账户类型（如受限账户）在 API 中可能不可见
- 企业账户或组织账户可能有不同的 API 行为

#### 原因 C：GitHub API 版本差异
- 某些功能在不同 API 版本中表现不同
- 可能需要使用不同的 API 端点

### 3. 这通常不是问题

**重要**：如果网页访问正常，说明：
- ✅ 账户存在且正常
- ✅ 仓库是公开的
- ✅ 其他人可以通过网页访问你的仓库

API 404 **不影响**网页访问和仓库的正常使用。

## 验证仓库可访问性的正确方法

### 方法 1：直接测试仓库 URL（推荐）

```powershell
# PowerShell 中测试
Invoke-WebRequest -Uri "https://github.com/chen-p/KosL" -Method Head
```

或者使用浏览器：
1. 打开**无痕/隐私窗口**（确保未登录）
2. 访问：`https://github.com/chen-p/KosL`
3. 如果能看到仓库内容，说明仓库是公开可访问的

### 方法 2：使用 API 查询仓库（不查询用户）

```bash
# 直接查询仓库，而不是用户
curl https://api.github.com/repos/chen-p/KosL
```

如果这个也返回 404，可能是：
- 仓库名称大小写不正确（GitHub 区分大小写）
- 仓库确实不存在或已删除

### 方法 3：使用浏览器开发者工具

1. 在浏览器中打开你的仓库页面
2. 按 `F12` 打开开发者工具
3. 切换到 **Network**（网络）标签
4. 刷新页面
5. 查看 HTTP 状态码：
   - 200：正常
   - 404：确实无法访问
   - 403：权限问题

## 解决方案

### 如果网页正常，但他人仍无法访问

可能的原因和解决方法：

#### 1. 检查仓库名称大小写
GitHub **严格区分大小写**：
- `KosL` ≠ `kosl` ≠ `KOSL`

确认仓库 URL 中的大小写完全正确。

#### 2. 检查是否在正确的账户下
- 确认仓库是在 `chen-p` 账户下，还是在某个组织下
- 检查仓库设置中的 "Owner"（所有者）信息

#### 3. 尝试重新设置可见性
在仓库 Settings → General → Danger Zone → Change visibility：
1. 先改为 Private（私有）
2. 保存
3. 再改回 Public（公开）
4. 等待几分钟后测试

#### 4. 清除浏览器缓存
- 使用无痕模式测试
- 或清除浏览器缓存和 Cookie

### 如果确实是 API 问题

#### 使用认证 token（如果需要）
```bash
# 使用 GitHub token（如果有）
curl -H "Authorization: token YOUR_TOKEN" https://api.github.com/users/chen-p
```

但这对于公开仓库通常不需要。

## 总结

**关键结论**：
- 如果 `https://github.com/chen-p/KosL` 网页访问正常，**仓库就是公开可访问的**
- API 返回 404 **不影响**网页访问和实际使用
- API 和网页使用不同的机制，这是正常的

**建议**：
1. 用无痕窗口测试仓库 URL
2. 让朋友/同事在未登录状态下访问
3. 如果网页能访问，就不用担心 API 的问题

## 相关资源

- GitHub API 文档：https://docs.github.com/en/rest
- GitHub 状态页面：https://www.githubstatus.com/
- GitHub 支持：https://support.github.com/
