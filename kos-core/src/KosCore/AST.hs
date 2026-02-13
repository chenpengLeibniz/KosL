{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
-- | KosCore.AST - 抽象语法树
-- 对应 Kos.tex 的直觉主义依赖类型论
-- 项由归纳定义，非法构造在类型层面不可表示

module KosCore.AST where

import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)

-- | 双轴 Universe 轴类型
data UniverseAxis
  = Computational  -- U_i 计算轴
  | Logical       -- Type_i 逻辑轴
  deriving (Eq, Show, Generic)

-- | Universe 层级信息
data UniverseInfo = UniverseInfo
  { uiAxis :: UniverseAxis
  , uiLevel :: Int
  }
  deriving (Eq, Show, Generic)

-- | 核心 Term 类型 - 归纳定义，穷尽所有合法构造
data Term
  -- 原子项 (Base Sorts)
  = Val Text (Maybe Term)      -- 值，可选类型标注
  | Time Text                  -- 时间
  | Ident Text                 -- 标识符
  | Prop Text                  -- 命题 (Prop : Type₁)
  -- Universe
  | Universe UniverseAxis Int  -- U_i 或 Type_i
  -- Π 类型 (依赖积)
  | Pi Text Term Term          -- Π(x:A). B
  | Lam Text Term Term         -- λ(x:A). t
  | App Term Term              -- f a
  -- Σ 类型 (依赖和)
  | Sigma Text Term Term       -- Σ(x:A). B
  | Pair Term Term             -- <d, p> 带证数据对
  | Split Term Text Text Term  -- split p as <x,y> in b
  -- Sum 类型
  | Sum Term Term              -- A + B
  | InL Term Term Term         -- inl_{A,B} a
  | InR Term Term Term         -- inr_{A,B} b
  | Case Term Text Term Text Term  -- case s of inl x -> t1; inr y -> t2
  -- Id 类型 (Kos.tex Id_A(a,b), refl)
  | Id Term Term Term              -- Id_A(a, b)
  | Refl Term                      -- refl_a : Id_A(a, a)
  | Sym Term                       -- sym e : Id A b a 当 e : Id A a b（等式对称，用于 rewrite 反向）
  | Transport Term Term Term       -- transport P e p : P b 当 e : Id A a b、p : P a；P : Π(x:A).Type
  -- Let 绑定 (ζ 归约)
  | Let Text Term Term Term        -- let x : A := v in t
  -- 单参数 inl/inr (类型从上下文推断)
  | InLS Term                      -- inl t (单参数)
  | InRS Term                      -- inr t (单参数)
  -- 值依赖谓词 (Value-Dependent Predicates)：类型构造依赖项的值
  | Gt Term Term                   -- a > b : Prop，当 a、b 可比较时
  | Ge Term Term                   -- a >= b
  | Lt Term Term                   -- a < b
  | Le Term Term                   -- a <= b
  | Eq Term Term                   -- a == b (数值或字符串)
  -- 规范证明项：值依赖谓词成立时由证明搜索产生的项（对应 TimeOK/SpaceOK/BatchOK 等小步验证）
  | Triv
  -- 变量 (用于上下文查找)
  | Var Text
  deriving (Eq, Show, Generic)

-- | 顶层声明
data Declaration
  = TypeDef Text Term          -- type Name : A
  | TermDef Text Term Term     -- def name : A := t
  deriving (Eq, Show, Generic)

-- | 模块
data Module = Module
  { modName :: Text
  , modDecls :: [Declaration]
  }
  deriving (Eq, Show, Generic)

-- | 辅助：获取 Term 的 kind（用于类型检查）
termKind :: Term -> Text
termKind = \case
  Val{} -> T.pack "Val"
  Time{} -> T.pack "Time"
  Ident{} -> T.pack "Ident"
  Prop{} -> T.pack "Prop"
  Universe{} -> T.pack "Universe"
  Pi{} -> T.pack "Pi"
  Lam{} -> T.pack "Lam"
  App{} -> T.pack "App"
  Sigma{} -> T.pack "Sigma"
  Pair{} -> T.pack "Pair"
  Split{} -> T.pack "Split"
  Sum{} -> T.pack "Sum"
  InL{} -> T.pack "InL"
  InR{} -> T.pack "InR"
  Case{} -> T.pack "Case"
  Id{} -> T.pack "Id"
  Refl{} -> T.pack "Refl"
  Sym{} -> T.pack "Sym"
  Transport{} -> T.pack "Transport"
  Let{} -> T.pack "Let"
  InLS{} -> T.pack "InLS"
  InRS{} -> T.pack "InRS"
  Gt{} -> T.pack "Gt"
  Ge{} -> T.pack "Ge"
  Lt{} -> T.pack "Lt"
  Le{} -> T.pack "Le"
  Eq{} -> T.pack "Eq"
  Triv -> T.pack "Triv"
  Var{} -> T.pack "Var"
