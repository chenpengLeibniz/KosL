{-# LANGUAGE LambdaCase #-}
-- | KosCore.Reduction - β 归约与正规化
-- 用于定义等价判定

module KosCore.Reduction where

import KosCore.AST
import KosCore.Context
import KosCore.Substitution
import Data.Text (Text)

-- | 单步归约（β, ι, ζ, δ）
reduceStep :: Context -> Term -> Maybe Term
reduceStep ctx = \case
  Var x ->
    ctxLookupDef x ctx  -- δ 归约：定义展开
  App (Lam x _ body) arg -> Just (substitute x arg body)
  App f a ->
    case reduceStep ctx f of
      Just f' -> Just (App f' a)
      Nothing ->
        case reduceStep ctx a of
          Just a' -> Just (App f a')
          Nothing -> Nothing
  Lam x ty body ->
    -- η 归约: λx. f x -> f 当 x ∉ FV(f)
    case body of
      App f (Var y) | y == x && not (occursFree x f) -> Just f
      _ -> case reduceStep ctx body of
        Just body' -> Just (Lam x ty body')
        Nothing -> Nothing
  Pi x dom body ->
    case reduceStep ctx dom of
      Just dom' -> Just (Pi x dom' body)
      Nothing ->
        case reduceStep ctx body of
          Just body' -> Just (Pi x dom body')
          Nothing -> Nothing
  Sigma x dom body ->
    case reduceStep ctx dom of
      Just dom' -> Just (Sigma x dom' body)
      Nothing ->
        case reduceStep ctx body of
          Just body' -> Just (Sigma x dom body')
          Nothing -> Nothing
  Pair d p ->
    case reduceStep ctx d of
      Just d' -> Just (Pair d' p)
      Nothing ->
        case reduceStep ctx p of
          Just p' -> Just (Pair d p')
          Nothing -> Nothing
  Split (Pair d p) x1 x2 body ->
    Just (substitute x2 p (substitute x1 d body))
  Case (InL a b v) x1 t1 _x2 _t2 ->
    Just (substitute x1 v t1)
  Case (InR a b v) _x1 _t1 x2 t2 ->
    Just (substitute x2 v t2)
  Case (InLS v) x1 t1 _x2 _t2 ->
    Just (substitute x1 v t1)
  Case (InRS v) _x1 _t1 x2 t2 ->
    Just (substitute x2 v t2)
  Let x _ty val body ->
    Just (substitute x val body)  -- ζ 归约
  Case s x1 t1 x2 t2 ->
    case reduceStep ctx s of
      Just s' -> Just (Case s' x1 t1 x2 t2)
      Nothing -> Nothing
  Split p x1 x2 body ->
    case reduceStep ctx p of
      Just p' -> Just (Split p' x1 x2 body)
      Nothing -> Nothing
  -- Id 消除：transport P (refl a) p ≡ p；sym (refl a) ≡ refl a
  Transport _p (Refl _) t -> Just t
  Sym (Refl a) -> Just (Refl a)
  Transport p eq t ->
    case reduceStep ctx p of
      Just p' -> Just (Transport p' eq t)
      Nothing -> case reduceStep ctx eq of
        Just eq' -> Just (Transport p eq' t)
        Nothing -> case reduceStep ctx t of
          Just t' -> Just (Transport p eq t')
          Nothing -> Nothing
  Sym e ->
    case reduceStep ctx e of
      Just e' -> Just (Sym e')
      Nothing -> Nothing
  Triv -> Nothing
  _ -> Nothing

-- | 正规化（重复归约直到无 redex）
normalize :: Context -> Term -> Term
normalize ctx t =
  case reduceStep ctx t of
    Just t' -> normalize ctx t'
    Nothing -> t

-- | 单步归约并返回证明：若 t → t′，返回 (t′, pf)，其中 pf 在定义等价下可作 Id A t t′ 的证明
-- （如 Refl (normalize ctx t)；类型检查器通过 conversion 接受为 Id A t t′）
reduceStepWithProof :: Context -> Term -> Maybe (Term, Term)
reduceStepWithProof ctx t = do
  t' <- reduceStep ctx t
  let n = normalize ctx t  -- t ≡ t′ 时 n = normalize t = normalize t′
  pure (t', Refl n)
