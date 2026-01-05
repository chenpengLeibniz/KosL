# KOS-TL 双轴世界支持匹配分析

## 概述

本文档分析当前代码实现与Kos.tex文档中定义的双轴世界（Dual-Axis Universe）系统的匹配程度。

## 一、文档定义的双轴世界（Kos.tex 305-433行）

### 1.1 核心结构

#### Dual-Axis Universes（双轴宇宙）

**1. Computational Axis（计算轴）$\mathcal{U}_i$**
- 遵循直谓性（Predicativity）
- $\mathcal{U}_0$ 包含基础排序（Base Sorts）
- $\mathcal{U}_{i+1}$ 包含 $\mathcal{U}_i$ 作为元素
- 用于建模有物理效应的数据

**2. Logical Axis（逻辑轴）$\textsf{Type}_i$**
- 遵循直谓性，但基础 $\textsf{Prop} : \textsf{Type}_1$ 具有非直谓性（Impredicativity）
- $\textsf{Type}_i$ 用于建模逻辑谓词空间和元逻辑规则
- $\textsf{Prop} : \textsf{Type}_1$（逻辑轴的起点）

**3. 层级关系（Hierarchical Relations）**
- $\textsf{Prop} : \textsf{Type}_1, \quad \textsf{Type}_i : \textsf{Type}_{i+1}$
- $\mathcal{U}_i : \mathcal{U}_{i+1}, \quad \mathcal{U}_i : \textsf{Type}_{i+1}$（计算宇宙可以作为逻辑讨论的对象）
- $\textsf{Prop} \hookrightarrow \mathcal{U}_1$（命题可以嵌入到数据轴）

### 1.2 Base Sorts（基础排序）

- $\textsf{Val}$：原子值（atomic values）
- $\textsf{Time}$：时间点标量（time point scalars）
- $\textsf{ID}$：唯一标识符（unique identifiers）

### 1.3 Universe Lifting Rules（宇宙提升规则）

**定义：Universe Lifting and Inclusion Rules（Kos.tex 422-433行）**

1. **从计算到逻辑的观察（Observation from Computation to Logic）**
   $$\frac{\Gamma \vdash A : \mathcal{U}_i}{\Gamma \vdash A : \textsf{Type}_{i+1}}$$
   任何计算类型都可以被视为逻辑命题讨论的对象

2. **命题的计算嵌入（Computational Embedding of Propositions）**
   $$\frac{\Gamma \vdash P : \textsf{Prop}}{\Gamma \vdash P : \mathcal{U}_1}$$
   逻辑证明项可以被打包到Σ记录中，作为实时计算系统的输入

### 1.4 类型构造规则

根据文档，类型构造需要考虑Universe层级：

**Π类型构造：**
- Logical/Computational Hybrid Rule:
  $$\frac{\Gamma \vdash A : \textsf{Type}_i/\mathcal{U}_i \quad \Gamma, x:A \vdash B : \textsf{Prop}}{\Gamma \vdash \Pi(x:A).B : \textsf{Prop}} (\text{Impredicative})$$
- Pure Universe Rule:
  $$\frac{\Gamma \vdash A : \textsf{Type}_i \quad \Gamma, x:A \vdash B : \textsf{Type}_j}{\Gamma \vdash \Pi(x:A).B : \textsf{Type}_{\max(i, j)}} (\text{Predicative})$$

**Σ类型构造：**
$$\frac{\Gamma \vdash A : \mathcal{U}_i \quad \Gamma, x:A \vdash B : \mathcal{U}_j}{\Gamma \vdash \Sigma(x:A).B : \mathcal{U}_{\max(i, j)}}$$

## 二、当前代码实现分析

### 2.1 类型定义（include/kos_core.h）

```c
typedef enum term_kind {
    KOS_VAL,      // 值类型
    KOS_SIGMA,    // Σ-Types: 依赖和类型
    KOS_PAIR,     // 普通对类型 <d, p>
    KOS_PROP,     // 命题类型
    KOS_PI,       // Π-Types: 依赖积类型
    KOS_SUM       // Sum Types: A + B
} term_kind;
```

### 2.2 匹配情况对比

| 文档要求 | 当前实现 | 匹配状态 | 说明 |
|---------|---------|---------|------|
| **Computational Axis** | | | |
| $\mathcal{U}_0$ | ❌ 未实现 | ❌ 不匹配 | 没有Universe层级系统 |
| $\mathcal{U}_i : \mathcal{U}_{i+1}$ | ❌ 未实现 | ❌ 不匹配 | 没有Universe层级追踪 |
| **Logical Axis** | | | |
| $\textsf{Prop}$ | ✅ `KOS_PROP` | ✅ 部分匹配 | 有Prop，但没有类型层级 |
| $\textsf{Type}_1$ | ❌ 未实现 | ❌ 不匹配 | 没有Type_i层级系统 |
| $\textsf{Type}_i : \textsf{Type}_{i+1}$ | ❌ 未实现 | ❌ 不匹配 | 没有类型层级追踪 |
| **Base Sorts** | | | |
| $\textsf{Val}$ | ✅ `KOS_VAL` | ✅ 匹配 | 有Val类型 |
| $\textsf{Time}$ | ❌ 未实现 | ❌ 不匹配 | 没有Time基础类型 |
| $\textsf{ID}$ | ❌ 未实现 | ❌ 不匹配 | 没有ID基础类型 |
| **Universe Lifting** | | | |
| $\mathcal{U}_i : \textsf{Type}_{i+1}$ | ❌ 未实现 | ❌ 不匹配 | 没有Universe提升规则 |
| $\textsf{Prop} \hookrightarrow \mathcal{U}_1$ | ❌ 未实现 | ❌ 不匹配 | 没有嵌入规则 |
| **类型构造器** | | | |
| $\Pi(x:A).B$ | ✅ `KOS_PI` | ✅ 部分匹配 | 有Π类型，但无Universe层级检查 |
| $\Sigma(x:A).B$ | ✅ `KOS_SIGMA` | ✅ 部分匹配 | 有Σ类型，但无Universe层级检查 |
| $A + B$ | ✅ `KOS_SUM` | ✅ 部分匹配 | 有Sum类型 |
| $\textsf{Id}_A(a, b)$ | ❌ 未实现 | ❌ 不匹配 | 没有Id类型 |

### 2.3 类型检查器分析（src/core/type_checker.c）

**当前实现的关键问题：**

1. **没有Universe层级检查**
   ```c
   bool kos_check(kos_term* ctx, kos_term* term, kos_term* type) {
       (void)ctx; // 上下文暂时未使用（简化实现）
       // ... 没有Universe层级检查
   }
   ```

2. **没有Universe Lifting规则**
   - 代码中没有实现 $\mathcal{U}_i : \textsf{Type}_{i+1}$ 的提升规则
   - 代码中没有实现 $\textsf{Prop} \hookrightarrow \mathcal{U}_1$ 的嵌入规则

3. **类型构造没有Universe层级约束**
   - Π类型构造没有检查 $\textsf{Type}_{\max(i, j)}$
   - Σ类型构造没有检查 $\mathcal{U}_{\max(i, j)}$

### 2.4 缺失的关键功能

#### 1. Universe层级系统

**需要添加：**
- Universe层级枚举或数值
- Universe层级追踪机制
- Universe层级比较和检查函数

**建议实现：**
```c
typedef enum {
    UNIVERSE_COMPUTATIONAL,  // U_i
    UNIVERSE_LOGICAL         // Type_i
} universe_axis;

typedef struct {
    universe_axis axis;
    int level;  // i for U_i or Type_i
} universe_info;
```

#### 2. Base Sorts扩展

**需要添加：**
- `KOS_TIME`：时间类型
- `KOS_ID`：标识符类型

#### 3. Universe Lifting规则

**需要实现：**
- 从 $\mathcal{U}_i$ 到 $\textsf{Type}_{i+1}$ 的自动提升
- 从 $\textsf{Prop}$ 到 $\mathcal{U}_1$ 的嵌入
- 类型检查时应用这些规则

#### 4. 类型构造的Universe检查

**需要实现：**
- Π类型构造时的Universe层级计算（$\max(i, j)$）
- Σ类型构造时的Universe层级计算（$\mathcal{U}_{\max(i, j)}$）
- 类型检查时验证Universe层级约束

## 三、匹配程度总结

### 3.1 总体评估

**匹配程度：❌ 不匹配（约30%）**

当前实现仅支持双轴世界的**基础结构**，但**完全缺失Universe层级系统**和**Universe Lifting规则**。

### 3.2 已实现的部分（✅）

1. **基础类型构造器**
   - ✅ Prop类型（但无Type_i层级）
   - ✅ Val类型
   - ✅ Π类型
   - ✅ Σ类型
   - ✅ Sum类型

2. **类型系统核心功能**
   - ✅ 类型检查框架
   - ✅ 类型构建器
   - ✅ 归约规则（部分）

### 3.3 未实现的关键部分（❌）

1. **Universe层级系统（核心缺失）**
   - ❌ 没有 $\mathcal{U}_i$ 层级
   - ❌ 没有 $\textsf{Type}_i$ 层级
   - ❌ 没有Universe层级追踪

2. **Universe Lifting规则（核心缺失）**
   - ❌ 没有 $\mathcal{U}_i : \textsf{Type}_{i+1}$ 规则
   - ❌ 没有 $\textsf{Prop} \hookrightarrow \mathcal{U}_1$ 规则

3. **Base Sorts扩展**
   - ❌ 没有Time类型
   - ❌ 没有ID类型

4. **类型构造的Universe约束**
   - ❌ Π类型构造没有Universe层级检查
   - ❌ Σ类型构造没有Universe层级检查

5. **其他类型**
   - ❌ 没有Id类型

### 3.4 影响分析

**缺失Universe层级系统的影响：**

1. **无法正确实现类型层级约束**
   - 无法检查类型是否在正确的Universe层级
   - 无法防止Universe层级越界

2. **无法实现Universe Lifting**
   - 计算类型无法自动提升为逻辑讨论对象
   - 命题无法嵌入到数据轴

3. **类型构造规则不完整**
   - Π类型和Σ类型的构造规则缺少Universe层级计算
   - 无法确保类型构造符合层级约束

4. **无法实现非直谓性（Impredicativity）**
   - Prop的非直谓性特性无法正确实现
   - 无法实现"逻辑闭合点"（Logical Closure Point）

## 四、改进建议

### 4.1 高优先级（核心功能）

1. **实现Universe层级系统**
   - 添加Universe层级数据结构
   - 实现Universe层级检查函数
   - 在类型检查中集成Universe层级验证

2. **实现Universe Lifting规则**
   - 实现 $\mathcal{U}_i : \textsf{Type}_{i+1}$ 提升
   - 实现 $\textsf{Prop} \hookrightarrow \mathcal{U}_1$ 嵌入

3. **扩展类型构造规则**
   - 在Π类型构造中添加Universe层级计算
   - 在Σ类型构造中添加Universe层级计算

### 4.2 中优先级（功能扩展）

1. **添加Base Sorts**
   - 实现Time类型
   - 实现ID类型

2. **实现Id类型**
   - 添加Id类型到term_kind
   - 实现Id类型的构造和检查

### 4.3 低优先级（优化）

1. **实现非直谓性特性**
   - 确保Prop的Impredicative特性正确实现

2. **完善类型检查**
   - 实现完整的上下文（Context）支持
   - 实现完整的依赖类型检查

## 五、结论

**当前实现与双轴世界支持不匹配。**

主要问题：
- **核心缺失**：没有Universe层级系统
- **核心缺失**：没有Universe Lifting规则
- **部分缺失**：缺少Time、ID基础类型
- **部分缺失**：类型构造规则不完整

要完全匹配文档中的双轴世界定义，需要实现Universe层级系统，这是最关键的缺失部分。当前的实现可以视为一个"简化版本"，提供了基础的 Prop 和类型构造器，但缺少完整的双轴世界层级结构。




