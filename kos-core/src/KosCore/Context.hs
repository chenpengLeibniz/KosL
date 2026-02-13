{-# LANGUAGE DeriveGeneric #-}
-- | KosCore.Context - 类型检查上下文
-- 变量绑定、不可变扩展

module KosCore.Context where

import KosCore.AST
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)

-- | 定义：(类型, 体)
type Definition = (Term, Term)

-- | 类型上下文：变量名 -> 类型，定义名 -> (类型, 体)
data Context = Context
  { ctxBindings :: Map Text Term      -- 变量 -> 类型
  , ctxDefs :: Map Text Definition   -- 定义名 -> (类型, 体)
  }
  deriving (Eq, Show)

empty :: Context
empty = Context Map.empty Map.empty

-- | 扩展上下文（不可变）
extend :: Text -> Term -> Context -> Context
extend x ty (Context m d) = Context (Map.insert x ty m) d

-- | 添加定义（δ 归约用）
addDef :: Text -> Term -> Term -> Context -> Context
addDef name ty body (Context m d) =
  Context (Map.insert name ty m) (Map.insert name (ty, body) d)

-- | 查找变量类型
ctxLookup :: Text -> Context -> Maybe Term
ctxLookup x (Context m _) = Map.lookup x m

-- | 查找定义体（δ 归约）
ctxLookupDef :: Text -> Context -> Maybe Term
ctxLookupDef x (Context _ d) = snd <$> Map.lookup x d

-- | 检查变量是否绑定
hasBinding :: Text -> Context -> Bool
hasBinding x (Context m _) = Map.member x m

-- | 上下文中所有绑定名（含定义），供证明搜索枚举构造函数
ctxNames :: Context -> [Text]
ctxNames (Context m _) = Map.keys m
