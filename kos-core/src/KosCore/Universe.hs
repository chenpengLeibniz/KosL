{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
-- | KosCore.Universe - 双轴 Universe 系统
-- 对应 Kos.tex 的 U_i (计算轴) 与 Type_i (逻辑轴)

module KosCore.Universe where

import KosCore.AST
import Data.Text (Text)

-- | 从 Term 提取 Universe 信息
getUniverseInfo :: Term -> UniverseInfo
getUniverseInfo = \case
  Prop{} -> UniverseInfo Logical 1
  Val{} -> UniverseInfo Computational 0
  Time{} -> UniverseInfo Computational 0
  Ident{} -> UniverseInfo Computational 0
  Universe ax lev -> UniverseInfo ax lev
  Pi{} -> UniverseInfo Logical 1
  Sigma{} -> UniverseInfo Logical 1
  Sum{} -> UniverseInfo Logical 1
  Id{} -> UniverseInfo Logical 1
  Let{} -> UniverseInfo Computational 0  -- 占位
  InLS{} -> UniverseInfo Computational 0
  InRS{} -> UniverseInfo Computational 0
  Refl{} -> UniverseInfo Logical 1
  _ -> UniverseInfo Computational 0

-- | Universe 层级比较
-- level1 <= level2 或 level1 可提升到 level2
universeLeq :: UniverseInfo -> UniverseInfo -> Bool
universeLeq (UniverseInfo ax1 l1) (UniverseInfo ax2 l2) =
  case (ax1, ax2) of
    (Computational, Computational) -> l1 <= l2
    (Logical, Logical) -> l1 <= l2
    -- U_i : Type_{i+1} (计算轴可提升到逻辑轴)
    (Computational, Logical) -> l1 + 1 <= l2
    (Logical, Computational) -> False

-- | Prop 嵌入到 U₁
propEmbedToData :: Term -> Term
propEmbedToData (Prop n) = Universe Computational 1  -- Prop ↪ U₁
propEmbedToData t = t

-- | U_i 提升到 Type_{i+1}
universeLiftToLogic :: Term -> Term
universeLiftToLogic (Universe Computational lev) =
  Universe Logical (lev + 1)
universeLiftToLogic t = t
