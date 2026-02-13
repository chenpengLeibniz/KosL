{-# LANGUAGE LambdaCase #-}
-- | KosCore.Alpha - α 等价
-- 用于定义等价判定（忽略绑定变量名差异）

module KosCore.Alpha where

import KosCore.AST
import Data.Text (Text)
import qualified Data.Map.Strict as Map

-- | α 等价：两 term 在重命名绑定变量后是否相同
alphaEquivalent :: Term -> Term -> Bool
alphaEquivalent t1 t2 = go Map.empty Map.empty t1 t2
  where
    go r1 r2 (Var x) (Var y) =
      case (Map.lookup x r1, Map.lookup y r2) of
        (Nothing, Nothing) -> x == y
        (Just a, Just b) -> a == b
        _ -> False
    go r1 r2 (Lam x ty1 b1) (Lam y ty2 b2) = do
      let r1' = Map.insert x (Var y) r1
      let r2' = Map.insert y (Var x) r2
      go r1 r2 ty1 ty2 && go r1' r2' b1 b2
    go r1 r2 (Pi x d1 b1) (Pi y d2 b2) = do
      let r1' = Map.insert x (Var y) r1
      let r2' = Map.insert y (Var x) r2
      go r1 r2 d1 d2 && go r1' r2' b1 b2
    go r1 r2 (Sigma x d1 b1) (Sigma y d2 b2) = do
      let r1' = Map.insert x (Var y) r1
      let r2' = Map.insert y (Var x) r2
      go r1 r2 d1 d2 && go r1' r2' b1 b2
    go r1 r2 (App f1 a1) (App f2 a2) = go r1 r2 f1 f2 && go r1 r2 a1 a2
    go r1 r2 (Pair d1 p1) (Pair d2 p2) = go r1 r2 d1 d2 && go r1 r2 p1 p2
    go r1 r2 (Sum a1 b1) (Sum a2 b2) = go r1 r2 a1 a2 && go r1 r2 b1 b2
    go r1 r2 (InL a1 b1 v1) (InL a2 b2 v2) =
      go r1 r2 a1 a2 && go r1 r2 b1 b2 && go r1 r2 v1 v2
    go r1 r2 (InR a1 b1 v1) (InR a2 b2 v2) =
      go r1 r2 a1 a2 && go r1 r2 b1 b2 && go r1 r2 v1 v2
    go r1 r2 (InLS v1) (InLS v2) = go r1 r2 v1 v2
    go r1 r2 (InRS v1) (InRS v2) = go r1 r2 v1 v2
    go r1 r2 (Case s1 x1 t1 y1 u1) (Case s2 x2 t2 y2 u2) = do
      let r1' = Map.insert x1 (Var x2) $ Map.insert y1 (Var y2) r1
      let r2' = Map.insert x2 (Var x1) $ Map.insert y2 (Var y1) r2
      go r1 r2 s1 s2 && go r1' r2' t1 t2 && go r1' r2' u1 u2
    go r1 r2 (Split p1 x1 y1 b1) (Split p2 x2 y2 b2) = do
      let r1' = Map.insert x1 (Var x2) $ Map.insert y1 (Var y2) r1
      let r2' = Map.insert x2 (Var x1) $ Map.insert y2 (Var y1) r2
      go r1 r2 p1 p2 && go r1' r2' b1 b2
    go r1 r2 (Id a1 b1 c1) (Id a2 b2 c2) =
      go r1 r2 a1 a2 && go r1 r2 b1 b2 && go r1 r2 c1 c2
    go r1 r2 (Refl w1) (Refl w2) = go r1 r2 w1 w2
    go r1 r2 (Sym e1) (Sym e2) = go r1 r2 e1 e2
    go r1 r2 (Transport p1 eq1 t1) (Transport p2 eq2 t2) =
      go r1 r2 p1 p2 && go r1 r2 eq1 eq2 && go r1 r2 t1 t2
    go r1 r2 (Gt a1 b1) (Gt a2 b2) = go r1 r2 a1 a2 && go r1 r2 b1 b2
    go r1 r2 (Ge a1 b1) (Ge a2 b2) = go r1 r2 a1 a2 && go r1 r2 b1 b2
    go r1 r2 (Lt a1 b1) (Lt a2 b2) = go r1 r2 a1 a2 && go r1 r2 b1 b2
    go r1 r2 (Le a1 b1) (Le a2 b2) = go r1 r2 a1 a2 && go r1 r2 b1 b2
    go r1 r2 (Eq a1 b1) (Eq a2 b2) = go r1 r2 a1 a2 && go r1 r2 b1 b2
    go r1 r2 (Let x ty1 v1 b1) (Let y ty2 v2 b2) = do
      let r1' = Map.insert x (Var y) r1
      let r2' = Map.insert y (Var x) r2
      go r1 r2 ty1 ty2 && go r1 r2 v1 v2 && go r1' r2' b1 b2
    go _ _ (Val v1 mt1) (Val v2 mt2) =
      v1 == v2 && case (mt1, mt2) of
        (Nothing, Nothing) -> True
        (Just t1, Just t2) -> go Map.empty Map.empty t1 t2
        _ -> False
    go _ _ (Time t1) (Time t2) = t1 == t2
    go _ _ (Ident i1) (Ident i2) = i1 == i2
    go _ _ (Prop p1) (Prop p2) = p1 == p2
    go _ _ (Universe ax1 l1) (Universe ax2 l2) = ax1 == ax2 && l1 == l2
    go _ _ Triv Triv = True
    go _ _ _ _ = False
