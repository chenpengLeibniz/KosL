# KOS-Core Haskell 形式化内核

## 概述

`kos-core/` 是使用 Haskell 实现的独立 Core 形式化内核，对应 `CORE_INDEPENDENCE_RECOMMENDATION.md` 中的方案 A（独立实现、Parser 即门卫）。

## 目录结构

```
kos-core/
├── kos-core.cabal      # Cabal 包配置
├── stack.yaml          # Stack 配置（可选）
├── README.md
├── src/
│   ├── KosCore.hs           # 主模块，对外 API
│   └── KosCore/
│       ├── AST.hs           # Term 代数数据类型
│       ├── Universe.hs      # 双轴 Universe (U_i, Type_i)
│       ├── Context.hs       # 类型上下文
│       ├── Substitution.hs  # 变量替换
│       ├── Reduction.hs     # β 归约、正规化
│       ├── TypeCheck.hs     # 双向类型检查、良构判定
│       └── Parser.hs        # Core DSL 解析器（唯一 Term 来源）
├── app/
│   └── Main.hs              # CLI 入口
├── examples/
│   ├── simple.kos
│   └── fail_evt.kos
└── test/
    └── Spec.hs               # HSpec 测试
```

## 构建与运行

```bash
cd kos-core
cabal build
cabal run kos-core -- check examples/simple.kos
cabal run kos-core -- parse examples/simple.kos
cabal run kos-core -- term "Prop P"
cabal test
```

## 形式化保障

1. **Parser 即门卫**：所有 Term 仅能通过 `parseTermFromSource` / `parseModuleFromSource` 产生，解析失败则无输出。
2. **归纳构造**：`Term` 为代数数据类型，穷尽所有合法构造，无未定义状态。
3. **类型检查即合法性**：`parseAndCheckTerm` / `parseAndCheckModule` 通过后才产出合法 Term。
4. **类型良构**：`typeWellFormed` 仅接受 PROP, SIGMA, PI, SUM, U, TYPE 作为类型构造子。

## Core DSL 语法

```
-- 原子
Prop P              -- 命题
U0, U1, U           -- 计算轴 Universe
Type0, Type1, Type  -- 逻辑轴 Universe
val "x", time "t", id "i"
Var x               -- 变量

-- Π 类型
Pi(x:A). B          -- 依赖函数类型
lam(x:A). t         -- λ 抽象
A -> B              -- 非依赖简写

-- Σ 类型
Sigma(x:A). B       -- 依赖对类型
< d , p >           -- 对构造

-- Sum 类型
inl(A, B, v), inr(A, B, v)
case s of inl x -> t1; inr y -> t2

-- 模块
module M where
  type Name : A
  def name : A := t
```

## 与 C Runtime 集成

1. **命令行调用**：C 侧通过 `popen` 或 `CreateProcess` 调用 `kos-core check file.kos`，根据退出码判断是否通过。
2. **序列化**：可扩展 `kos-core` 输出 JSON 格式的已校验 Term，供 C 侧反序列化。
3. **FFI**：未来可考虑 Haskell FFI 导出 C 可调用接口。
