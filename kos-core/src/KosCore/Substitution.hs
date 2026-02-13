{-# LANGUAGE LambdaCase #-}
-- | KosCore.Substitution - 变量替换
-- 用于 β 归约和依赖类型检查

module KosCore.Substitution where

import KosCore.AST
import Data.Text (Text)

-- | 替换 term 中自由变量 x 为 replacement
-- 注意捕获避免：若 replacement 中有 x 的绑定变量，需 α 重命名
substitute :: Text -> Term -> Term -> Term
substitute x replacement target = go [] target
  where
    go bound = \case
      Var y -> if y == x && y `notElem` bound then replacement else Var y
      Val v mt -> Val v (fmap (go bound) mt)
      Time t -> Time t
      Ident i -> Ident i
      Prop p -> Prop p
      Universe ax lev -> Universe ax lev
      Pi y dom body ->
        Pi y (go bound dom) $
          if y == x then body else go (y : bound) body
      Lam y ty body ->
        Lam y (go bound ty) $
          if y == x then body else go (y : bound) body
      App f a -> App (go bound f) (go bound a)
      Sigma y dom body ->
        Sigma y (go bound dom) $
          if y == x then body else go (y : bound) body
      Pair d p -> Pair (go bound d) (go bound p)
      Split p x1 x2 b -> Split (go bound p) x1 x2 (go (x1:x2:bound) b)
      Sum a b -> Sum (go bound a) (go bound b)
      InL a b v -> InL (go bound a) (go bound b) (go bound v)
      InR a b v -> InR (go bound a) (go bound b) (go bound v)
      Case s x1 t1 x2 t2 ->
        Case (go bound s) x1 (go (x1:bound) t1) x2 (go (x2:bound) t2)
      Id a b c -> Id (go bound a) (go bound b) (go bound c)
      Refl w -> Refl (go bound w)
      Sym e -> Sym (go bound e)
      Transport p eq t -> Transport (go bound p) (go bound eq) (go bound t)
      Let y ty val body ->
        Let y (go bound ty) (go bound val) $
          if y == x then body else go (y : bound) body
      InLS v -> InLS (go bound v)
      InRS v -> InRS (go bound v)
      Gt a b -> Gt (go bound a) (go bound b)
      Ge a b -> Ge (go bound a) (go bound b)
      Lt a b -> Lt (go bound a) (go bound b)
      Le a b -> Le (go bound a) (go bound b)
      Eq a b -> Eq (go bound a) (go bound b)
      Triv -> Triv

-- | 变量 x 在 term 中是否自由出现
occursFree :: Text -> Term -> Bool
occursFree x = go []
  where
    go bound = \case
      Var y -> y == x && y `notElem` bound
      Lam y _ body -> go (y : bound) body
      Pi y _ body -> go (y : bound) body
      Sigma y _ body -> go (y : bound) body
      Split _ y1 y2 body -> go (y1 : y2 : bound) body
      Case _ y1 t1 y2 t2 -> go (y1 : bound) t1 || go (y2 : bound) t2
      Let y _ _ body -> go (y : bound) body
      App f a -> go bound f || go bound a
      Pair d p -> go bound d || go bound p
      Sum a b -> go bound a || go bound b
      InL a b v -> go bound a || go bound b || go bound v
      InR a b v -> go bound a || go bound b || go bound v
      InLS v -> go bound v
      InRS v -> go bound v
      Gt a b -> go bound a || go bound b
      Ge a b -> go bound a || go bound b
      Lt a b -> go bound a || go bound b
      Le a b -> go bound a || go bound b
      Eq a b -> go bound a || go bound b
      Triv -> False
      Id a b c -> go bound a || go bound b || go bound c
      Refl w -> go bound w
      Sym e -> go bound e
      Transport p eq t -> go bound p || go bound eq || go bound t
      Val _ mt -> maybe False (go bound) mt
      _ -> False
