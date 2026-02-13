-- | KosCore - KOS 形式化类型论内核
-- Parser 即门卫：唯一合法 Term 来源
--
-- 对外 API：
--   parseAndCheckTerm  - 解析并类型检查单个项
--   parseAndCheckModule - 解析并类型检查模块
--   typeWellFormed     - 类型良构判定

module KosCore
  ( module KosCore.AST
  , module KosCore.Universe
  , module KosCore.Context
  , module KosCore.TypeCheck
  , module KosCore.Parser
  , module KosCore.Reduction
  , module KosCore.Proof
  , parseAndCheckTerm
  , parseAndCheckModule
  , contextFromModule
  ) where

import KosCore.AST
import KosCore.Universe
import Control.Monad (unless)
import KosCore.Context
import KosCore.TypeCheck
import KosCore.Parser
import KosCore.Reduction
import KosCore.Proof
import KosCore.Substitution
import Data.Text (Text)
import Text.Parsec (ParseError)

-- | 解析并类型检查单个 Term
-- 失败则 Left，成功则 Right (term, type)
parseAndCheckTerm :: Text -> Either String (Term, Term)
parseAndCheckTerm src =
  case parseTermFromSource src of
    Left err -> Left ("Parse error: " ++ show err)
    Right term ->
      case infer empty term of
        Left tyErr -> Left ("Type error: " ++ show tyErr)
        Right ty -> Right (term, ty)

-- | 解析并类型检查模块
-- 失败则 Left，成功则 Right Module
parseAndCheckModule :: Text -> Either String Module
parseAndCheckModule src =
  case parseModuleFromSource src of
    Left err -> Left ("Parse error: " ++ show err)
    Right modu -> do
      -- 检查所有声明
      let ctx = foldl extendCtx empty (modDecls modu)
      _ <- checkModuleDecls ctx (modDecls modu)
      Right modu
  where
    extendCtx ctx (TypeDef name ty) = extend name ty ctx
    extendCtx ctx (TermDef name ty body) = addDef name ty body (extend name ty ctx)
    checkModuleDecls _ [] = Right ()
    checkModuleDecls ctx (TypeDef name ty : rest) = do
      unless (typeWellFormed ty) $ Left ("Type not well-formed: " ++ show ty)
      checkModuleDecls (extend name ty ctx) rest
    checkModuleDecls ctx (TermDef name ty body : rest) = do
      let ty' = expandTypeSynonym ctx ty
      unless (typeWellFormed ty') $ Left ("Type not well-formed: " ++ show ty)
      case check ctx body ty of
        Left e -> Left ("Type error: " ++ show e)
        Right () -> checkModuleDecls (addDef name ty body (extend name ty ctx)) rest

-- | 从已类型检查的模块构建证明用上下文（供自动证明在“带公理/定义”下搜索）
contextFromModule :: Module -> Context
contextFromModule modu = foldl extendCtx empty (modDecls modu)
  where
    extendCtx ctx (TypeDef name ty) = extend name ty ctx
    extendCtx ctx (TermDef name ty body) = addDef name ty body (extend name ty ctx)
