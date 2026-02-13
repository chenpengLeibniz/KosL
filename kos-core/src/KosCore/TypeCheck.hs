{-# LANGUAGE LambdaCase #-}
-- | KosCore.TypeCheck - 双向类型检查
-- 类型良构判定、kos_check 等价

module KosCore.TypeCheck where

import KosCore.AST
import KosCore.Context
import Control.Monad (unless)
import KosCore.Universe
import KosCore.Substitution
import KosCore.Reduction
import KosCore.Alpha
import Data.Text (Text)
import qualified Data.Text as T
import Text.Read (readMaybe)

-- | 类型检查错误
data TypeError
  = NotWellFormed Term
  | TypeMismatch Term Term
  | UnboundVar Text
  | NotAFunction Term Term
  | InvalidPair Term Term
  | InvalidSum Term
  | InLSInRSRequireCheckMode Text  -- inl/inr 单参数需 check 模式，期望 Sum A B
  | PredicateFailed Text           -- 值依赖谓词不成立，如 "Gt: 180 >= 180"
  | NotComparable Term Term        -- 项不可比较（非数值）
  deriving (Eq, Show)

-- | 类型检查结果
type TC = Either TypeError

-- | 类型良构判定
-- 仅 PROP, SIGMA, PI, SUM, U, TYPE 可作为类型
typeWellFormed :: Term -> Bool
typeWellFormed = \case
  Prop{} -> True
  Universe{} -> True
  Sigma _ dom body -> typeWellFormed dom && typeWellFormed body
  Pi _ dom body -> typeWellFormed dom && typeWellFormed body
  Sum a b -> typeWellFormed a && typeWellFormed b
  Id aTy _ _ -> typeWellFormed aTy
  Gt{} -> True   -- 值依赖谓词：结构良构，值检查在 check 时进行
  Ge{} -> True
  Lt{} -> True
  Le{} -> True
  Eq{} -> True
  Val{} -> False
  Time{} -> False
  Ident{} -> False
  Pair{} -> False
  Lam{} -> False
  App{} -> False
  Split{} -> False
  InL{} -> False
  InR{} -> False
  InLS{} -> False
  InRS{} -> False
  Case{} -> False
  Refl{} -> False
  Let{} -> False
  Triv -> False
  Var{} -> False

-- | 从项提取数值（Val/Time 的字符串可解析为 Double 时）
readNumeric :: Context -> Term -> Maybe Double
readNumeric ctx t =
  case normalize ctx t of
    Val v _ -> readMaybe (T.unpack v)
    Time v -> readMaybe (T.unpack v)
    Var x -> ctxLookup x ctx >>= readNumeric ctx
    _ -> Nothing

-- | 值依赖谓词求值：比较 a op b，成立则接受 term（证明可为 Prop 或 trivial）
evalValuePred :: Context -> Term -> Term -> (Double -> Double -> Bool) -> Term -> Term -> Text -> TC ()
evalValuePred ctx term _ty op a b name = do
  let na = normalize ctx a
      nb = normalize ctx b
  case (readNumeric ctx na, readNumeric ctx nb) of
    (Just va, Just vb) ->
      if va `op` vb then Right () else Left (PredicateFailed (name <> T.pack ": " <> T.pack (show va) <> T.pack " not " <> name <> T.pack " " <> T.pack (show vb)))
    _ -> Left (NotComparable na nb)

-- | Eq 谓词：支持数值或字符串相等
evalValuePredEq :: Context -> Term -> Term -> Term -> Term -> TC ()
evalValuePredEq ctx term _ty a b = do
  let na = normalize ctx a
      nb = normalize ctx b
  case (readNumeric ctx na, readNumeric ctx nb) of
    (Just va, Just vb) -> if va == vb then Right () else Left (PredicateFailed (T.pack "Eq: " <> T.pack (show va) <> T.pack " /= " <> T.pack (show vb)))
    _ ->
      case (na, nb) of
        (Val va _, Val vb _) -> if va == vb then Right () else Left (PredicateFailed (T.pack "Eq: " <> va <> T.pack " /= " <> vb))
        (Time ta, Time tb) -> if ta == tb then Right () else Left (PredicateFailed (T.pack "Eq: " <> ta <> T.pack " /= " <> tb))
        (Ident ia, Ident ib) -> if ia == ib then Right () else Left (PredicateFailed (T.pack "Eq: " <> ia <> T.pack " /= " <> ib))
        _ -> Left (NotComparable na nb)

-- | 定义等价：normalize + α 等价
-- 比较前展开类型别名（Var 在上下文中解析为类型时）
definitionallyEqual :: Context -> Term -> Term -> TC Bool
definitionallyEqual ctx t1 t2 = do
  let n1 = normalize ctx (expandTypeSynonym ctx t1)
  let n2 = normalize ctx (expandTypeSynonym ctx t2)
  pure $ alphaEquivalent n1 n2

-- | 展开类型别名/定义（Var 在上下文中解析时），递归展开子项
-- def 优先展开为体（δ），否则 type 展开为类型
expandTypeSynonym :: Context -> Term -> Term
expandTypeSynonym ctx (Var x) =
  case ctxLookupDef x ctx of
    Just body -> expandTypeSynonym ctx body  -- def: 展开为体
    Nothing -> case ctxLookup x ctx of
      Just ty -> expandTypeSynonym ctx ty   -- type: 展开为类型
      Nothing -> Var x
expandTypeSynonym ctx t = case t of
  Pi x a b -> Pi x (expandTypeSynonym ctx a) (expandTypeSynonym ctx b)
  Sigma x a b -> Sigma x (expandTypeSynonym ctx a) (expandTypeSynonym ctx b)
  Sum a b -> Sum (expandTypeSynonym ctx a) (expandTypeSynonym ctx b)
  Id aTy a b -> Id (expandTypeSynonym ctx aTy) (expandTypeSynonym ctx a) (expandTypeSynonym ctx b)
  App f a -> App (expandTypeSynonym ctx f) (expandTypeSynonym ctx a)
  Lam x ty b -> Lam x (expandTypeSynonym ctx ty) (expandTypeSynonym ctx b)
  Pair d p -> Pair (expandTypeSynonym ctx d) (expandTypeSynonym ctx p)
  Split p x y b -> Split (expandTypeSynonym ctx p) x y (expandTypeSynonym ctx b)
  InL a b v -> InL (expandTypeSynonym ctx a) (expandTypeSynonym ctx b) (expandTypeSynonym ctx v)
  InR a b v -> InR (expandTypeSynonym ctx a) (expandTypeSynonym ctx b) (expandTypeSynonym ctx v)
  Case s x t1 y t2 -> Case (expandTypeSynonym ctx s) x (expandTypeSynonym ctx t1) y (expandTypeSynonym ctx t2)
  Refl w -> Refl (expandTypeSynonym ctx w)
  Sym e -> Sym (expandTypeSynonym ctx e)
  Transport p eq t -> Transport (expandTypeSynonym ctx p) (expandTypeSynonym ctx eq) (expandTypeSynonym ctx t)
  Let x ty v b -> Let x (expandTypeSynonym ctx ty) (expandTypeSynonym ctx v) (expandTypeSynonym ctx b)
  InLS v -> InLS (expandTypeSynonym ctx v)
  InRS v -> InRS (expandTypeSynonym ctx v)
  Gt a b -> Gt (expandTypeSynonym ctx a) (expandTypeSynonym ctx b)
  Ge a b -> Ge (expandTypeSynonym ctx a) (expandTypeSynonym ctx b)
  Lt a b -> Lt (expandTypeSynonym ctx a) (expandTypeSynonym ctx b)
  Le a b -> Le (expandTypeSynonym ctx a) (expandTypeSynonym ctx b)
  Eq a b -> Eq (expandTypeSynonym ctx a) (expandTypeSynonym ctx b)
  _ -> t

-- | 双向类型检查：term 是否具有类型 ty
check :: Context -> Term -> Term -> TC ()
check ctx term ty =
  let ty' = expandTypeSynonym ctx ty
  in if not (typeWellFormed ty')
     then Left (NotWellFormed ty)
     else checkInternal ctx term ty'

checkInternal :: Context -> Term -> Term -> TC ()
checkInternal ctx term ty = case ty of
  Prop _ ->
    case term of
      Prop{} -> Right ()
      Let x valTy val body -> do
        check ctx val valTy
        check (extend x valTy ctx) body ty
      Var x ->
        case ctxLookup x ctx of
          Just tty -> if typeWellFormed tty then Right () else Left (NotWellFormed tty)
          Nothing -> Left (UnboundVar x)
      _ -> Left (TypeMismatch term ty)

  Pi x dom body ->
    case term of
      Lam y tyBody bodyTerm ->
        if not (typeWellFormed dom)
          then Left (NotWellFormed dom)
          else do
            -- 类型标注须与域定义等价，非 check tyBody : dom
            eq <- definitionallyEqual ctx tyBody dom
            unless eq $ Left (TypeMismatch tyBody dom)
            let ctx' = extend y tyBody ctx
            let bodyTy = substitute x (Var y) body
            check ctx' bodyTerm bodyTy
      _ -> Left (TypeMismatch term ty)

  Sigma x dom body ->
    case term of
      Pair d p ->
        if not (typeWellFormed dom) || not (typeWellFormed body)
          then Left (NotWellFormed ty)
          else do
            check ctx d dom
            let bodyInst = substitute x d body
            check ctx p bodyInst
      Var y -> case ctxLookupDef y ctx of
        Just defBody ->
          case normalize ctx defBody of
            Pair{} -> check ctx (normalize ctx defBody) ty
            _ -> Left (InvalidPair term ty)
        Nothing -> case ctxLookup y ctx of
          Just varTy -> do
            eq <- definitionallyEqual ctx (expandTypeSynonym ctx varTy) ty
            unless eq $ Left (InvalidPair term ty)
          Nothing -> Left (UnboundVar y)
      _ -> case normalize ctx term of
        Pair d p -> do
          check ctx d dom
          let bodyInst = substitute x d body
          check ctx p bodyInst
        _ -> Left (InvalidPair term ty)

  Sum leftTy rightTy ->
    case term of
      -- 类型表达式 A+B 与期望 Sum leftTy rightTy 的定义等价
      Sum a b -> do
        eq1 <- definitionallyEqual ctx a leftTy
        eq2 <- definitionallyEqual ctx b rightTy
        if eq1 && eq2 then pure () else Left (TypeMismatch term ty)
      InL a b v -> do
        check ctx a leftTy
        check ctx b rightTy
        check ctx v leftTy
      InR a b v -> do
        check ctx a leftTy
        check ctx b rightTy
        check ctx v rightTy
      InLS v -> check ctx v leftTy
      InRS v -> check ctx v rightTy
      _ -> Left (InvalidSum term)

  Id aTy a b ->
    case term of
      Refl w -> do
        unless (typeWellFormed aTy) $ Left (NotWellFormed ty)
        check ctx w aTy
        eqAB <- definitionallyEqual ctx a b
        eqAW <- definitionallyEqual ctx a w
        if eqAB && eqAW then Right () else Left (TypeMismatch term ty)
      Sym e -> do
        -- Sym e : Id A b a 当 e : Id A a b；当前目标 ty = Id aTy a b，故需 ty ≡ Id aTy b a
        eqTy <- definitionallyEqual ctx ty (Id aTy b a)
        unless eqTy $ Left (TypeMismatch term ty)
        check ctx e (Id aTy a b)
      _ -> Left (TypeMismatch term ty)

  Universe ax lev ->
    case term of
      Universe ax' lev' -> if ax == ax' && lev == lev' then Right () else Left (TypeMismatch term ty)
      _ ->
        let termInfo = getUniverseInfo term
            tyInfo = getUniverseInfo ty
        in if universeLeq termInfo tyInfo then Right () else Left (TypeMismatch term ty)

  -- 值依赖谓词：类型构造依赖项的值，需在检查时求值；Triv 为证明搜索产生的规范证明项
  Gt a b -> case term of
    Triv -> evalValuePred ctx term ty (>) a b (T.pack "Gt")
    _ -> evalValuePred ctx term ty (>) a b (T.pack "Gt")
  Ge a b -> case term of
    Triv -> evalValuePred ctx term ty (>=) a b (T.pack "Ge")
    _ -> evalValuePred ctx term ty (>=) a b (T.pack "Ge")
  Lt a b -> case term of
    Triv -> evalValuePred ctx term ty (<) a b (T.pack "Lt")
    _ -> evalValuePred ctx term ty (<) a b (T.pack "Lt")
  Le a b -> case term of
    Triv -> evalValuePred ctx term ty (<=) a b (T.pack "Le")
    _ -> evalValuePred ctx term ty (<=) a b (T.pack "Le")
  Eq a b -> case term of
    Triv -> evalValuePredEq ctx term ty a b
    _ -> evalValuePredEq ctx term ty a b

  _ ->
    -- Conversion 规则：Γ⊢t:A, A≡B, Γ⊢B:S ⇒ Γ⊢t:B
    -- 结构匹配失败时 fallback：infer + definitionallyEqual
    case infer ctx term of
      Right inferredTy -> do
        eq <- definitionallyEqual ctx inferredTy ty
        if eq then Right () else Left (TypeMismatch term ty)
      Left _ -> Left (TypeMismatch term ty)

-- | 推断类型（简化版，仅支持部分构造）
infer :: Context -> Term -> TC Term
infer ctx = \case
  Var x ->
    case ctxLookup x ctx of
      Just ty -> Right ty
      Nothing -> Left (UnboundVar x)

  Prop n -> Right (Universe Logical 1)

  Universe ax lev -> Right (Universe ax (lev + 1))

  Pi x dom body -> do
    unless (typeWellFormed dom) $ Left (NotWellFormed dom)
    domSort <- infer ctx dom
    let ctx' = extend x dom ctx
    bodySort <- infer ctx' body
    unless (typeWellFormed bodySort) $ Left (NotWellFormed bodySort)
    -- Kos.tex predicative: Π(x:A).B : Type_{max(i,j)} when A:Type_i, B:Type_j
    let levelFrom s = case s of
          Universe Logical k -> Just k
          Prop _ -> Just 1
          _ -> Nothing
    case (levelFrom domSort, levelFrom bodySort) of
      (Just i, Just j) -> Right (Universe Logical (max i j))
      _ -> Right (Universe Logical 1)  -- fallback (Impredicativity / non-logical)

  Lam x ty body -> do
    unless (typeWellFormed ty) $ Left (NotWellFormed ty)
    let ctx' = extend x ty ctx
    bodyTy <- infer ctx' body
    Right (Pi x ty bodyTy)

  App f a -> do
    fTy <- infer ctx f
    case fTy of
      Pi x dom body -> do
        check ctx a dom
        Right (substitute x a body)
      _ -> Left (NotAFunction f fTy)

  Sigma x dom body -> do
    unless (typeWellFormed dom) $ Left (NotWellFormed dom)
    let ctx' = extend x dom ctx
    unless (typeWellFormed body) $ Left (NotWellFormed body)
    Right (Universe Logical 1)

  Pair d p -> do
    dTy <- infer ctx d
    let x = T.pack "x"
        ctx' = extend x dTy ctx
    pTy <- infer ctx' p   -- p 在 x:dTy 下推断，使 pTy 可依赖 x（Σ 依赖对）
    Right (Sigma x dTy pTy)

  Sum a b -> do
    unless (typeWellFormed a) $ Left (NotWellFormed a)
    unless (typeWellFormed b) $ Left (NotWellFormed b)
    Right (Universe Logical 1)

  InL a b v -> do
    aTy <- infer ctx a
    bTy <- infer ctx b
    check ctx v aTy
    Right (Sum aTy bTy)

  InR a b v -> do
    aTy <- infer ctx a
    bTy <- infer ctx b
    check ctx v bTy
    Right (Sum aTy bTy)

  Val{} -> Right (Universe Computational 0)
  Time{} -> Right (Universe Computational 0)
  Ident{} -> Right (Universe Computational 0)

  Id aTy a b -> do
    check ctx a aTy
    check ctx b aTy
    Right (Prop (T.pack "Id"))  -- Id_A(a,b) : Prop

  Refl w -> do
    wTy <- infer ctx w
    Right (Id wTy w w)

  Sym e -> do
    eTy <- infer ctx e
    case eTy of
      Id aTy a b -> Right (Id aTy b a)
      _ -> Left (TypeMismatch e eTy)

  Transport p eq t -> do
    eqTy <- infer ctx eq
    (aTy, a, b) <- case eqTy of
      Id ty a' b' -> Right (ty, a', b')
      _ -> Left (TypeMismatch eq eqTy)
    pTy <- infer ctx p
    case pTy of
      Pi x dom body -> do
        _ <- unless (typeWellFormed dom) $ Left (NotWellFormed dom)
        eqDom <- definitionallyEqual ctx dom aTy
        unless eqDom $ Left (TypeMismatch p pTy)
        check ctx t (substitute x a body)  -- t : P a
        Right (substitute x b body)         -- Transport P eq t : P b
      _ -> Left (NotAFunction p pTy)

  Let x ty val body -> do
    check ctx val ty
    let ctx' = extend x ty ctx
    infer ctx' body

  Split p x1 x2 body -> do
    pTy <- infer ctx p
    case pTy of
      Sigma x' dom bod -> do
        let ctx' = extend x2 (substitute x' (Var x1) bod) (extend x1 dom ctx)
        infer ctx' body
      _ -> Left (InvalidPair p pTy)

  InLS _ -> Left (InLSInRSRequireCheckMode (T.pack "inl"))
  InRS _ -> Left (InLSInRSRequireCheckMode (T.pack "inr"))

  Gt{} -> Right (Universe Logical 1)  -- Gt(a,b) : Prop
  Ge{} -> Right (Universe Logical 1)
  Lt{} -> Right (Universe Logical 1)
  Le{} -> Right (Universe Logical 1)
  Eq{} -> Right (Universe Logical 1)

  _ -> Left (NotWellFormed (Var (T.pack "?")))
