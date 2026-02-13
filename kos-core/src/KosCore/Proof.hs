{-# LANGUAGE LambdaCase #-}
-- | KosCore.Proof - 自动化证明（推导）规则与策略
--
-- 对应 monograph 第 8 章「核心层：证明构造与小步演化」：
--   - 逻辑谓词作为类型（TimeOK/SpaceOK/BatchOK）的验证
--   - tryWithProc：checkTime → checkSpace → checkBatch → mkCausalProof 的链式小步
--   - deriveCausal / analyze 的目标类型驱动证明搜索
--
-- 对标 Coq/Lean/Agda 的常见能力（见 docs/PROOF_TOOLS_COMPARISON.md）：
--   - intro/apply/split：Pi/Sigma/Sum 引入、构造函数 apply、exact
--   - assumption：上下文假设匹配（tryAssumption）、exact 指定假设（tryExactVar）
--   - cases/destruct：对 Sum 假设分情况（tryCaseHyp / tryCaseHypAny）
--   - 深度限制：proveDepth、deepen（从深度 1 递增直到成功）
--   - rewrite：tryRewriteL2R / tryRewriteR2L / tryRewriteWith（用 Id 假设重写目标，Transport + Sym）
--   - first | t1 | t2：firstSuccess / orElse；try t：tryTactic
module KosCore.Proof where

import KosCore.AST
import KosCore.Context
import KosCore.Substitution
import KosCore.Reduction
import KosCore.TypeCheck (definitionallyEqual, readNumeric, expandTypeSynonym, infer, check)
import Data.Text (Text)
import qualified Data.Text as T

-- | 默认证明搜索深度上限，防止无限递归（对标 Coq/Lean 的 auto 深度）
defaultProveDepth :: Int
defaultProveDepth = 100

-- | 策略：给定上下文与目标类型，尝试产生一个证明项（Nothing 表示失败）
type Tactic = Context -> Term -> Maybe Term

-- | 带深度限制的策略（depth <= 0 时不再递归，用于 proveDepth）
type TacticD = Int -> Context -> Term -> Maybe Term

-- | 小步配置：与 monograph 中 C = ⟨Γ, σ, f⟩ 对应，此处简化为 (ctx, goal)
type StepConfig = (Context, Term)

-- | 对目标进行正规化并展开类型同义词（与 TypeCheck 一致）
goalNorm :: Context -> Term -> Term
goalNorm ctx = normalize ctx . expandTypeSynonym ctx

orElse :: Maybe a -> Maybe a -> Maybe a
orElse Nothing y = y
orElse x _ = x

firstJust :: [Maybe a] -> Maybe a
firstJust [] = Nothing
firstJust (Just x : _) = Just x
firstJust (Nothing : xs) = firstJust xs

-- | 值依赖谓词策略：当目标为 Gt/Ge/Lt/Le/Eq(a,b) 且谓词成立时返回 Triv
-- 对应 checkTime / checkSpace / checkBatch 的“可计算验证”部分
tryValuePred :: Tactic
tryValuePred ctx goal = case goalNorm ctx goal of
  Gt a b -> cmp (>) a b
  Ge a b -> cmp (>=) a b
  Lt a b -> cmp (<) a b
  Le a b -> cmp (<=) a b
  Eq a b -> eq a b
  _ -> Nothing
  where
    cmp op a b = do
      va <- readNumeric ctx (normalize ctx a)
      vb <- readNumeric ctx (normalize ctx b)
      if va `op` vb then Just Triv else Nothing
    eq a b =
      case (readNumeric ctx na, readNumeric ctx nb) of
        (Just va, Just vb) -> if va == vb then Just Triv else Nothing
        _ -> case (na, nb) of
          (Val va _, Val vb _) -> if va == vb then Just Triv else Nothing
          (Time ta, Time tb) -> if ta == tb then Just Triv else Nothing
          (Ident ia, Ident ib) -> if ia == ib then Just Triv else Nothing
          _ -> Nothing
      where na = normalize ctx a
            nb = normalize ctx b

-- | Id 类型策略：当 a ≡ b 时返回 Refl a（定义等价）
tryRefl :: Tactic
tryRefl ctx goal = case goalNorm ctx goal of
  Id aTy a b ->
    case definitionallyEqual ctx a b of
      Right True -> Just (Refl a)
      _ -> Nothing
  _ -> Nothing

-- | 假设策略：若目标类型与上下文中某变量的类型定义等价，则返回该变量（对应 PSM 中的见证使用）
tryAssumption :: Tactic
tryAssumption ctx goal =
  let g = goalNorm ctx goal
  in foldr orElse Nothing
       [ case ctxLookup name ctx of
           Just ty -> case definitionallyEqual ctx (normalize ctx ty) g of
             Right True -> Just (Var name)
             _ -> Nothing
           Nothing -> Nothing
       | name <- ctxNames ctx ]

-- | Sigma 引入：目标为 Σ(x:A).B 时，先证 A 得 d，再证 B[d/x] 得 p，返回 Pair d p
tryIntroSigma :: Tactic
tryIntroSigma ctx goal = tryIntroSigmaD defaultProveDepth ctx goal

tryIntroSigmaD :: TacticD
tryIntroSigmaD d _ _ | d <= 0 = Nothing
tryIntroSigmaD d ctx goal = case goalNorm ctx goal of
  Sigma x dom body -> do
    d' <- proveDepth (d - 1) ctx dom
    let bodyInst = substitute x d' body
    p <- proveDepth (d - 1) ctx bodyInst
    Just (Pair d' p)
  _ -> Nothing

-- | Pi 引入：目标为 Π(x:A).B 时，在扩展上下文下证 B 得 body，返回 Lam x A body
tryIntroPi :: Tactic
tryIntroPi ctx goal = tryIntroPiD defaultProveDepth ctx goal

tryIntroPiD :: TacticD
tryIntroPiD d _ _ | d <= 0 = Nothing
tryIntroPiD d ctx goal = case goalNorm ctx goal of
  Pi x dom body -> do
    let ctx' = extend x dom ctx
    bodyTerm <- proveDepth (d - 1) ctx' body
    Just (Lam x dom bodyTerm)
  _ -> Nothing

-- | Sum 左/右引入（对标 Lean cases / apply Or.inl | Or.inr）：目标为 A+B 时证 A 得 InL，或证 B 得 InR
tryIntroSumLeft, tryIntroSumRight :: Tactic
tryIntroSumLeft ctx goal = tryIntroSumLeftD defaultProveDepth ctx goal
tryIntroSumRight ctx goal = tryIntroSumRightD defaultProveDepth ctx goal

tryIntroSumLeftD :: TacticD
tryIntroSumLeftD d _ _ | d <= 0 = Nothing
tryIntroSumLeftD d ctx goal = case goalNorm ctx goal of
  Sum a b -> do
    v <- proveDepth (d - 1) ctx a
    Just (InL a b v)
  _ -> Nothing

tryIntroSumRightD :: TacticD
tryIntroSumRightD d _ _ | d <= 0 = Nothing
tryIntroSumRightD d ctx goal = case goalNorm ctx goal of
  Sum a b -> do
    v <- proveDepth (d - 1) ctx b
    Just (InR a b v)
  _ -> Nothing

-- | exact：若给定项 t 的类型与目标定义等价，则直接返回 t（对标 Lean exact / Coq exact）
exactTerm :: Context -> Term -> Term -> Maybe Term
exactTerm ctx goal t = case check ctx t goal of
  Right () -> Just t
  Left _ -> Nothing

-- | 用指定名称的假设填满目标（对标 Lean exact h / Coq exact (H)）
tryExactVar :: Text -> Tactic
tryExactVar name ctx goal = do
  ty <- ctxLookup name ctx
  case definitionallyEqual ctx (normalize ctx ty) (goalNorm ctx goal) of
    Right True -> Just (Var name)
    _ -> Nothing

-- | 对假设做 case 分析（对标 Lean cases H / Coq destruct H）：若 hypName 的类型为 Sum A B，
-- 则在左分支用 s = inl x、右分支用 s = inr y 扩展上下文后分别证 goal，返回 Case s x pL y pR
tryCaseHyp :: Text -> Tactic
tryCaseHyp hypName ctx goal =
  tryCaseHypD hypName defaultProveDepth ctx goal

tryCaseHypD :: Text -> TacticD
tryCaseHypD hypName d _ _ | d <= 0 = Nothing
tryCaseHypD hypName d ctx goal = do
  ty <- ctxLookup hypName ctx
  case goalNorm ctx ty of
    Sum a b -> do
      let ctxL = extend (T.pack "__caseL") a ctx
          ctxR = extend (T.pack "__caseR") b ctx
          goalL = substitute hypName (InL a b (Var (T.pack "__caseL"))) goal
          goalR = substitute hypName (InR a b (Var (T.pack "__caseR"))) goal
      pL <- proveDepth (d - 1) ctxL goalL
      pR <- proveDepth (d - 1) ctxR goalR
      Just (Case (Var hypName) (T.pack "__caseL") pL (T.pack "__caseR") pR)
    _ -> Nothing

-- | 对上下文中任一 Sum 类型假设做 case 分析（并入主策略顺序时使用）
tryCaseHypAny :: TacticD
tryCaseHypAny d _ _ | d <= 0 = Nothing
tryCaseHypAny d ctx goal =
  firstJust [ tryCaseHypD name d ctx goal | name <- ctxNames ctx ]

-- | 只运行一个策略 t；失败则返回 Nothing（对标 try t）
tryTactic :: Tactic -> Tactic
tryTactic t = t

-- | 从深度 1 递增到 maxDepth 尝试证明，直到某一深度成功（对标 repeat / auto 的加深）
deepen :: Int -> Context -> Term -> Maybe Term
deepen maxDepth ctx goal =
  firstJust [ proveDepth d ctx goal | d <- [1 .. maxDepth] ]

-- | 构造函数应用策略：在上下文中查找定义 d，若其类型以目标为余域，
-- 则依次对每个参数类型做证明搜索，成功则构造 App(..App(d, p1).., pn)
-- 对应 monograph 中 mkCausalProof(a,f,e,p_t,p_s,p_b) 的“小步”构造
tryConstructor :: Tactic
tryConstructor ctx goal = tryConstructorD defaultProveDepth ctx goal

tryConstructorD :: TacticD
tryConstructorD d _ _ | d <= 0 = Nothing
tryConstructorD d ctx goal =
  let g = goalNorm ctx goal
  in firstJust $ map (tryOne g) (ctxNames ctx)
  where
    tryOne g name = do
      fty <- ctxLookup name ctx
      let ftyN = normalize ctx fty
      (bindersTys, _) <- piChainToGoal ctx ftyN g
      args <- proveArgsD (d - 1) ctx bindersTys
      Just (foldl App (Var name) args)

-- | 将 Π(x1:A1)..Π(xn:An). T 分解为 [(x1,A1),..,(xn,An)] 且 T ≡ goal（保留绑定名以便依赖代入）
piChainToGoal :: Context -> Term -> Term -> Maybe ([(Text, Term)], Term)
piChainToGoal ctx fty goal = go [] fty
  where
    go acc = \case
      Pi x dom body ->
        go ((x, dom) : acc) body
      ty ->
        case definitionallyEqual ctx ty goal of
          Right True -> Just (reverse acc, ty)
          _ -> Nothing

-- | 依赖参数顺序证明：证完 A_i 得到 d_i 后，将 d_i 代入后续类型再证（支持 mkCausalProof(…,p_t,p_s,p_b) 等依赖型构造子）
proveArgs :: Context -> [(Text, Term)] -> Maybe [Term]
proveArgs ctx rest = proveArgsD defaultProveDepth ctx rest

proveArgsD :: Int -> Context -> [(Text, Term)] -> Maybe [Term]
proveArgsD depth _ [] = Just []
proveArgsD depth _ _ | depth <= 0 = Nothing
proveArgsD depth ctx ((x, dom) : rest) = do
  d <- proveDepth (depth - 1) ctx dom
  let restSubst = [(y, substitute x d ty) | (y, ty) <- rest]
  ds <- proveArgsD (depth - 1) ctx restSubst
  Just (d : ds)

-- | 带深度限制的证明搜索（对标 Coq auto 深度、Lean 的有限回溯）
-- depth <= 0 时立即返回 Nothing，避免无限递归
proveDepth :: Int -> Context -> Term -> Maybe Term
proveDepth depth _ _ | depth <= 0 = Nothing
proveDepth depth ctx goal =
  tryValuePred ctx goal
  `orElse` tryRefl ctx goal
  `orElse` tryAssumption ctx goal
  `orElse` tryRewriteAny ctx goal
  `orElse` tryCaseHypAny depth ctx goal
  `orElse` tryIntroSigmaD depth ctx goal
  `orElse` tryIntroPiD depth ctx goal
  `orElse` tryIntroSumLeftD depth ctx goal
  `orElse` tryIntroSumRightD depth ctx goal
  `orElse` tryConstructorD depth ctx goal

-- | 主证明搜索：按策略顺序尝试，直至某一策略成功；内部使用 defaultProveDepth 限制深度
-- 策略顺序：值谓词/Refl（小步验证）→ 假设 → Sigma/Pi/Sum 引入 → 构造函数应用（小步演化）
prove :: Context -> Term -> Maybe Term
prove = proveDepth defaultProveDepth

-- | 小步演化：给定目标与候选列表（如 getProcSteps(f,σ) 的返回值），
-- 对每个候选 c 将 c 加入上下文为 __e，再对 goal 做证明搜索；返回第一个成功的证明。
-- 对应 monograph：deriveCausal(a,f) = firstSome (map (λe. tryWithProc(a,f,e), getProcSteps(f,σ)))
tryWithCandidates :: Context -> Term -> [Term] -> Maybe Term
tryWithCandidates ctx goal candidates =
  firstJust
    [ prove (addDef (T.pack "__e") ty c ctx) goal
    | c <- candidates
    , Right ty <- [infer ctx c]
    ]

-- | 策略列表的“第一个成功”
firstSuccess :: [Tactic] -> Tactic
firstSuccess tactics ctx goal =
  firstJust (map (\t -> t ctx goal) tactics)
  where
    firstJust [] = Nothing
    firstJust (Just x : _) = Just x
    firstJust (Nothing : xs) = firstJust xs

-- ---------------------------------------------------------------------------
-- Rewrite：用等式替换目标中的子项，再通过 Transport 构造证明
-- ---------------------------------------------------------------------------

-- | 将 goal 中所有与 a 定义等价的子项替换为 b（用于构造 P 与 newGoal）
replaceSubterm :: Context -> Term -> Term -> Term -> Term
replaceSubterm ctx a b t =
  case definitionallyEqual ctx t a of
    Right True -> b
    _ -> case t of
      Var x -> Var x
      Val v mt -> Val v (fmap (replaceSubterm ctx a b) mt)
      Time tm -> Time tm
      Ident i -> Ident i
      Prop p -> Prop p
      Universe ax lev -> Universe ax lev
      Pi x dom body -> Pi x (replaceSubterm ctx a b dom) (replaceSubterm ctx a b body)
      Lam x ty body -> Lam x (replaceSubterm ctx a b ty) (replaceSubterm ctx a b body)
      App f arg -> App (replaceSubterm ctx a b f) (replaceSubterm ctx a b arg)
      Sigma x dom body -> Sigma x (replaceSubterm ctx a b dom) (replaceSubterm ctx a b body)
      Pair d p -> Pair (replaceSubterm ctx a b d) (replaceSubterm ctx a b p)
      Split p x1 x2 body -> Split (replaceSubterm ctx a b p) x1 x2 (replaceSubterm ctx a b body)
      Sum u v -> Sum (replaceSubterm ctx a b u) (replaceSubterm ctx a b v)
      InL u v w -> InL (replaceSubterm ctx a b u) (replaceSubterm ctx a b v) (replaceSubterm ctx a b w)
      InR u v w -> InR (replaceSubterm ctx a b u) (replaceSubterm ctx a b v) (replaceSubterm ctx a b w)
      Case s x1 t1 x2 t2 -> Case (replaceSubterm ctx a b s) x1 (replaceSubterm ctx a b t1) x2 (replaceSubterm ctx a b t2)
      Id ty x y -> Id (replaceSubterm ctx a b ty) (replaceSubterm ctx a b x) (replaceSubterm ctx a b y)
      Refl w -> Refl (replaceSubterm ctx a b w)
      Sym e -> Sym (replaceSubterm ctx a b e)
      Transport p eq w -> Transport (replaceSubterm ctx a b p) (replaceSubterm ctx a b eq) (replaceSubterm ctx a b w)
      Let x ty val body -> Let x (replaceSubterm ctx a b ty) (replaceSubterm ctx a b val) (replaceSubterm ctx a b body)
      InLS v -> InLS (replaceSubterm ctx a b v)
      InRS v -> InRS (replaceSubterm ctx a b v)
      Gt u v -> Gt (replaceSubterm ctx a b u) (replaceSubterm ctx a b v)
      Ge u v -> Ge (replaceSubterm ctx a b u) (replaceSubterm ctx a b v)
      Lt u v -> Lt (replaceSubterm ctx a b u) (replaceSubterm ctx a b v)
      Le u v -> Le (replaceSubterm ctx a b u) (replaceSubterm ctx a b v)
      Eq u v -> Eq (replaceSubterm ctx a b u) (replaceSubterm ctx a b v)
      Triv -> Triv

rewriteX :: Text
rewriteX = T.pack "__rewrite_x"

-- | 用等式 eqName : Id A a b 将目标中 a 重写为 b（左到右）；先证 newGoal = goal[b/a]，再 Transport 得 goal
tryRewriteL2R :: Text -> Tactic
tryRewriteL2R eqName ctx goal = do
  eqProof <- ctxLookup eqName ctx
  let eqTyN = goalNorm ctx eqProof
  (aTy, a, b) <- case eqTyN of Id ty a' b' -> Just (ty, a', b'); _ -> Nothing
  let newGoal = replaceSubterm ctx a b goal
  proofNew <- prove ctx newGoal
  let pFam = Lam rewriteX aTy (replaceSubterm ctx a (Var rewriteX) goal)
  -- goal = P a，newGoal = P b；有 proofNew : P b、eq : Id A a b，需得 P a：用 Sym eq : Id A b a，Transport P (Sym eq) proofNew : P a
  Just (Transport pFam (Sym (Var eqName)) proofNew)

-- | 用等式 eqName : Id A a b 将目标中 b 重写为 a（右到左）；先证 newGoal = goal[a/b]，再 Transport 得 goal
tryRewriteR2L :: Text -> Tactic
tryRewriteR2L eqName ctx goal = do
  eqProof <- ctxLookup eqName ctx
  let eqTyN = goalNorm ctx eqProof
  (aTy, a, b) <- case eqTyN of Id ty a' b' -> Just (ty, a', b'); _ -> Nothing
  let newGoal = replaceSubterm ctx b a goal
  proofNew <- prove ctx newGoal
  let pFam = Lam rewriteX aTy (replaceSubterm ctx b (Var rewriteX) goal)
  -- goal = P b，newGoal = P a；有 proofNew : P a、eq : Id A a b，Transport P eq proofNew : P b
  Just (Transport pFam (Var eqName) proofNew)

-- | 先尝试 L2R，再尝试 R2L（对标 rewrite H / rw [H]）
tryRewriteWith :: Text -> Tactic
tryRewriteWith eqName ctx goal =
  tryRewriteL2R eqName ctx goal `orElse` tryRewriteR2L eqName ctx goal

-- | 对上下文中任一 Id 类型假设尝试 rewrite（并入主策略时使用）
tryRewriteAny :: Tactic
tryRewriteAny ctx goal =
  firstJust
    [ tryRewriteWith name ctx goal
    | name <- ctxNames ctx
    , Just ty <- [ctxLookup name ctx]
    , Id _ _ _ <- [goalNorm ctx ty]
    ]
