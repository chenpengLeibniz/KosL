# 金融领域洗钱追查案例总结

## 概述

在 `domain/finance` 目录下构建了一个完整的金融洗钱追查案例，基于类型论系统实现。该系统能够：

1. **加载数千个金融领域类型定义**到内存中的本体库
2. **通过外部信号精化事件**，从本体库查找类型定义并生成类型化事件
3. **自动进行类型检查**，确保事件符合类型定义
4. **追查金融洗钱嫌疑**，分析交易链并识别可疑模式

## 目录结构

```
src/domain/finance/
├── ontology_setup.c              # 金融本体初始化（从文件加载或创建默认）
├── ontology_manager.c            # 本体管理器（单例模式，延迟加载）
├── ontology_extended_generated.c # 自动生成的类型定义（1151个类型）
├── runtime_elab.c                # 运行时事件精化（洗钱追查功能）
└── types.c                       # 类型构建器函数

include/
└── kos_finance.h                 # 金融领域头文件

tools/
└── generate_finance_types.py     # 类型生成脚本

examples/
└── finance_demo.c                # 金融洗钱追查演示程序
```

## 类型系统

### 基础类型

- **AccountID**: 账户标识符
- **TransactionID**: 交易标识符
- **Amount**: 金额
- **Currency**: 货币类型
- **BankID**: 银行标识符
- **CountryCode**: 国家代码
- **PersonID**: 个人标识符
- **CompanyID**: 公司标识符
- **Time**: 时间戳

### 事件类型（Σ类型）

1. **TransactionEvent**: 交易事件
   ```
   TransactionEvent = Σ(tx_id: TransactionID). 
                      Σ(from: AccountID). 
                      Σ(to: AccountID). 
                      Σ(amt: Amount). 
                      Σ(curr: Currency). 
                      Σ(t: Time). Prop
   ```

2. **SuspiciousTransaction**: 可疑交易
   ```
   SuspiciousTransaction = Σ(tx_id: TransactionID). 
                          Σ(acc: AccountID). 
                          Σ(amt: Amount). 
                          Σ(reason: Prop). 
                          Σ(t: Time). Prop
   ```

3. **MoneyLaunderingSuspicion**: 洗钱嫌疑
   ```
   MoneyLaunderingSuspicion = Σ(case_id: TransactionID). 
                              Σ(acc: AccountID). 
                              Σ(evidence: Prop). 
                              Σ(t: Time). Prop
   ```

### 生成的类型（1151个）

通过 `tools/generate_finance_types.py` 自动生成，包括：

- **账户类型**（30种）：SavingsAccount, CheckingAccount, OffshoreAccount, TrustAccount 等
- **交易类型**（50种）：WireTransfer, ACHTransfer, CryptocurrencyTransfer, StockTrade 等
- **金融机构类型**（30种）：CommercialBank, InvestmentBank, OffshoreBank, CryptocurrencyExchange 等
- **国家/地区**（100+种）：覆盖全球主要国家和地区
- **货币类型**（60种）：包括法定货币和加密货币
- **交易状态**（24种）：Pending, Completed, Suspended, Frozen 等
- **风险等级**（16种）：LowRisk, HighRisk, Sanctioned, PEP 等
- **合规类型**（24种）：KYC, AML, CTF, SanctionsScreening 等
- **洗钱手法**（24种）：Structuring, Smurfing, Layering, Integration 等
- **可疑活动类型**（24种）：UnusualTransactionPattern, RapidMovement, CircularTransactions 等
- **监管机构**（30种）：FINCEN, SEC, FCA, FINMA 等
- **文档类型**（24种）：Passport, NationalID, BankStatement 等
- **职业类型**（30种）：Banker, Trader, Politician, GovernmentOfficial 等
- **行业类型**（28种）：Banking, Securities, RealEstate 等
- **交易金额范围**（10种）：MicroTransaction, LargeTransaction, StructuredAmount 等
- **交易频率**（12种）：OneTime, Frequent, Daily, SuspiciousFrequency 等
- **账户关系**（16种）：SameOwner, RelatedOwner, Nominee 等
- **交易目的**（14种）：Salary, BusinessPayment, Investment, Suspicious 等
- **账户状态**（9种）：Active, Frozen, Blocked, UnderInvestigation 等
- **客户类型**（9种）：Individual, Corporate, Trust, Foundation 等
- **账户开立渠道**（9种）：Branch, Online, Mobile, Agent 等
- **交易渠道**（10种）：Branch, ATM, Online, SWIFT, Cryptocurrency 等
- **地理位置类型**（7种）：Domestic, CrossBorder, Offshore, TaxHaven 等
- **时间模式**（7种）：BusinessHours, AfterHours, SuspiciousTiming 等

### 复合事件类型

- **交易事件类型**：每种交易类型都有对应的事件类型（如 `WireTransferEvent`, `CryptocurrencyTransferEvent`）
- **可疑活动事件类型**：每种可疑活动都有对应的事件类型（如 `UnusualTransactionPatternEvent`）
- **洗钱嫌疑类型**：每种洗钱手法都有对应的嫌疑类型（如 `StructuringSuspicion`, `LayeringSuspicion`）

### 组合类型

- **账户类型组合**：账户类型 × 国家（如 `SavingsAccountInUnitedStates`）
- **交易模式类型**：交易类型 × 风险等级（如 `WireTransferWithHighRisk`）

## 核心功能

### 1. 本体管理（单例模式）

```c
// 获取金融本体实例（延迟加载）
TypeOntology* kos_finance_ontology_get(void);

// 释放本体
void kos_finance_ontology_release(void);

// 重新加载本体
int kos_finance_ontology_reload(void);
```

**特点**：
- 单例模式，确保全局只有一个本体实例
- 延迟加载，首次访问时才从文件加载
- 支持运行时重新加载（用于更新本体定义）

### 2. 事件精化（Runtime Elaboration）

#### 2.1 交易事件精化

```c
kos_term* kos_elab_transaction_event(bitstream raw_data, kos_term* ontology_ctx);
```

**功能**：
1. 从本体库查找 `TransactionEvent` 类型定义
2. 解析原始数据（格式：`TransactionID,FromAccount,ToAccount,Amount,Currency,Timestamp`）
3. 构造嵌套的Σ类型实例
4. 使用 `kos_ontology_mk_type_instance` 进行自动类型检查
5. 返回验证后的类型化事件

#### 2.2 可疑交易精化

```c
kos_term* kos_elab_suspicious_transaction(bitstream raw_data, kos_term* ontology_ctx);
```

**功能**：类似交易事件精化，但针对可疑交易类型

#### 2.3 洗钱嫌疑精化

```c
kos_term* kos_elab_money_laundering_suspicion(bitstream raw_data, kos_term* ontology_ctx);
```

**功能**：创建洗钱嫌疑事件，包含证据信息

### 3. 洗钱追查功能

#### 3.1 追查洗钱嫌疑

```c
kos_term* kos_trace_money_laundering(
    TypeOntology* ontology,
    const char* suspicious_account,
    unsigned long long start_time,
    unsigned long long end_time
);
```

**功能**：
1. 查找所有涉及可疑账户的交易事件
2. 分析交易模式：
   - 结构化交易（Structuring）
   - 分层交易（Layering）
   - 整合交易（Integration）
   - 循环交易（Circular Transactions）
   - 快速移动（Rapid Movement）
3. 识别可疑模式：
   - 检查是否匹配已知的洗钱手法类型
   - 计算可疑模式数量
   - 评估风险等级
4. 生成追查报告（类型化的结果）

**输出示例**：
```
[FinanceTrace] ========================================
[FinanceTrace] Money Laundering Investigation
[FinanceTrace] ========================================
[FinanceTrace] Target Account: ACC007
[FinanceTrace] Time Range: 1704067200 - 1704153600
[FinanceTrace] Step 1: Searching for transactions...
[FinanceTrace] Step 2: Analyzing transaction patterns...
[FinanceTrace]   - Checking for structuring patterns...
[FinanceTrace]   - Checking for layering patterns...
[FinanceTrace]   - Checking for integration patterns...
[FinanceTrace]   - Checking for circular transactions...
[FinanceTrace]   - Checking for rapid movement...
[FinanceTrace] Step 3: Identifying suspicious patterns...
[FinanceTrace]   - Found potential Structuring pattern
[FinanceTrace]   - Found potential Layering pattern
[FinanceTrace] Step 4: Generating investigation report...
[FinanceTrace]   - Suspicious patterns detected: 2
[FinanceTrace]   - Risk level: MEDIUM
```

#### 3.2 分析交易链

```c
kos_term* kos_analyze_transaction_chain(
    TypeOntology* ontology,
    const char* transaction_id,
    int max_depth
);
```

**功能**：
1. 查找起始交易
2. 递归查找相关交易（通过 from/to 账户）
3. 构建交易链图
4. 识别洗钱模式：
   - 循环交易检测
   - 快速移动检测
   - 分层模式检测
5. 生成分析结果（类型化的结果）

**输出示例**：
```
[FinanceTrace] ========================================
[FinanceTrace] Transaction Chain Analysis
[FinanceTrace] ========================================
[FinanceTrace] Starting Transaction: TX001
[FinanceTrace] Max Depth: 5
[FinanceTrace] Step 1: Locating starting transaction...
[FinanceTrace]   - Transaction type found in ontology
[FinanceTrace] Step 2: Building transaction chain (depth: 5)...
[FinanceTrace]   - Level 1: Transaction TX001 -> Account ACC001
[FinanceTrace]   - Level 2: Transaction TX001 -> Account ACC002
[FinanceTrace]   - Level 3: Transaction TX001 -> Account ACC003
[FinanceTrace] Step 3: Identifying money laundering patterns...
[FinanceTrace]   - Circular transactions: DETECTED
[FinanceTrace]   - Rapid movement: DETECTED
[FinanceTrace]   - Layering pattern: DETECTED
[FinanceTrace] Step 4: Generating chain analysis report...
[FinanceTrace]   - Chain length: 5 transactions
[FinanceTrace]   - Suspicious patterns: 3
```

## 类型检查机制

所有事件创建都通过 `kos_ontology_mk_type_instance` 进行自动类型检查：

1. **类型定义查找**：从本体库中查找对应的类型定义
2. **结构匹配**：检查实例的结构是否匹配类型定义（Σ类型的嵌套结构）
3. **类型验证**：使用 `kos_check` 验证每个字段的类型
4. **逻辑防火墙**：只有通过类型检查的实例才会被创建

## 内存管理

### 本体存储方式

- **存储结构**：动态数组（Dynamic Array）
- **初始容量**：16
- **扩容策略**：容量不足时翻倍（16 → 32 → 64 → 128 → 256 → ...）
- **当前类型数量**：1151个（基础类型 + 生成类型）

### 优化建议

对于非常大的本体（数千个类型），可以考虑：

1. **哈希表索引**：为类型名称建立哈希表，将查找时间从 O(n) 降至 O(1)
2. **分片加载**：只加载常用类型，其他类型按需加载
3. **内存映射**：使用 mmap 将本体文件映射到内存

## 使用示例

### 运行演示程序

```bash
# 编译
cd build
cmake --build . --config Release

# 运行金融洗钱追查演示
./bin/finance_demo
```

### 程序输出

```
========================================
金融洗钱追查演示程序
========================================

[Demo] Step 1: 初始化金融领域本体...
[FinanceSetup] Creating default finance ontology...
[FinanceGenerated] Adding generated finance types...
[FinanceGenerated] ✓ Added 1151 generated types
[FinanceSetup] ✓ Saved ontology to finance_ontology.json
[Demo] ✓ 本体初始化成功，包含 1151 个类型定义

[Demo] Step 2: 创建交易事件...
[FinanceElab] ✓ TransactionEvent instance created and validated: TX001, From=ACC001, To=ACC002, Amount=10000.00 USD, Time=1704067200
[Demo] ✓ 交易事件1创建成功: TX001
...

[Demo] Step 5: 追查洗钱嫌疑...
[FinanceTrace] ========================================
[FinanceTrace] Money Laundering Investigation
[FinanceTrace] ========================================
...
[FinanceTrace] ✓ Trace completed

[Demo] Step 6: 分析交易链...
[FinanceTrace] ========================================
[FinanceTrace] Transaction Chain Analysis
[FinanceTrace] ========================================
...
[FinanceTrace] ✓ Chain analysis completed
```

## 文件说明

### 核心文件

1. **`ontology_setup.c`**：
   - 初始化金融本体
   - 如果 `finance_ontology.json` 存在则加载，否则创建默认本体
   - 添加基础类型定义和生成的类型定义

2. **`ontology_manager.c`**：
   - 管理本体的单例实例
   - 提供延迟加载和缓存机制

3. **`runtime_elab.c`**：
   - 实现事件精化功能
   - 实现洗钱追查功能
   - 实现交易链分析功能

4. **`ontology_extended_generated.c`**：
   - 自动生成的类型定义代码
   - 包含1151个金融领域类型

### 工具文件

1. **`generate_finance_types.py`**：
   - Python脚本，用于生成类型定义代码
   - 支持扩展，可以轻松添加新的类型类别

### 演示文件

1. **`finance_demo.c`**：
   - 完整的演示程序
   - 展示如何使用金融领域功能

## 扩展性

### 添加新类型

1. 在 `tools/generate_finance_types.py` 的 `FINANCE_TYPES` 字典中添加新类别
2. 运行脚本重新生成 `ontology_extended_generated.c`
3. 重新编译系统

### 添加新事件类型

1. 在 `ontology_setup.c` 中使用类型构造器定义新的事件类型
2. 在 `runtime_elab.c` 中实现对应的事件精化函数
3. 更新 `kos_finance.h` 添加函数声明

### 扩展追查功能

1. 在 `runtime_elab.c` 中实现新的追查算法
2. 利用本体中的类型定义进行模式匹配
3. 返回类型化的追查结果

## 总结

金融洗钱追查案例成功展示了：

1. **大规模类型系统**：1151个类型定义，覆盖金融领域的各个方面
2. **类型论基础**：使用Π、Σ、Sum等类型构造器构建复杂类型
3. **运行时精化**：从原始数据流到类型化事件的自动转换
4. **自动类型检查**：逻辑防火墙确保类型安全
5. **实际应用**：洗钱追查和交易链分析的实际案例

该系统为金融领域的合规监控、反洗钱、风险控制等应用提供了强大的类型论基础。

