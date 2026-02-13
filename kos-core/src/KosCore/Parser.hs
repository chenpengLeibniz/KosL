{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
-- | KosCore.Parser - 语法分析器 (parsec)
-- 唯一合法 Term 来源，解析失败则无输出

module KosCore.Parser where

import KosCore.AST
import Data.Text (Text)
import qualified Data.Text as T
import Data.Functor (void)
import Data.Functor.Identity (Identity)
import Text.Parsec (try)
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Token as Tok

type Parser = Parsec String ()

lexer :: Tok.GenTokenParser String () Identity
lexer = Tok.makeTokenParser $ emptyDef
  { Tok.commentStart = "/*"
  , Tok.commentEnd = "*/"
  , Tok.commentLine = "--"
  , Tok.identStart = letter
  , Tok.identLetter = alphaNum <|> oneOf "_'"
  ,   Tok.reservedNames = ["let", "in", "module", "where", "type", "def"
                        , "lam", "Pi", "Sigma", "case", "of", "Prop"
                        , "U0", "U1", "U", "Type0", "Type1", "Type"
                        , "val", "time", "id", "inl", "inr", "Id", "IdA", "refl", "split", "as"
                        , "gt", "ge", "lt", "le", "eq"]
  }

lexeme :: Parser a -> Parser a
lexeme = Tok.lexeme lexer

symbol :: String -> Parser ()
symbol = void . Tok.symbol lexer

keyword :: String -> Parser ()
keyword kw = void (Tok.reserved lexer kw)

identifier :: Parser Text
identifier = T.pack <$> Tok.identifier lexer

stringLit :: Parser Text
stringLit = T.pack <$> Tok.stringLiteral lexer

-- | 解析 Term
parseTerm :: Parser Term
parseTerm = parseArrow

parseArrow :: Parser Term
parseArrow = do
  t <- parseLam
  (do symbol "->"
      body <- parseArrow
      pure $ case t of
        Pi x dom _ -> Pi x dom body
        _ -> Pi (T.pack "_") t body)
    <|> pure t

parseLam :: Parser Term
parseLam =
  (do try (keyword "lam")
      symbol "("
      x <- identifier
      symbol ":"
      ty <- parseTerm
      symbol ")"
      symbol "."
      body <- parseLam
      pure $ Lam x ty body)
  <|> parseSum

parseSum :: Parser Term
parseSum = do
  t <- parsePi
  (do symbol "+"
      u <- parseSum
      pure $ Sum t u)
    <|> pure t

parsePi :: Parser Term
parsePi =
  (do try ((keyword "Pi" <|> keyword "Π"))
      symbol "("
      x <- identifier
      symbol ":"
      dom <- parseTerm
      symbol ")"
      symbol "."
      body <- parsePi
      pure $ Pi x dom body)
  <|> parseSigma

parseSigma :: Parser Term
parseSigma =
  (do try ((keyword "Sigma" <|> keyword "Σ"))
      symbol "("
      x <- identifier
      symbol ":"
      dom <- parseTerm
      symbol ")"
      symbol "."
      body <- parseSigma
      pure $ Sigma x dom body)
  <|> parseApp

parseApp :: Parser Term
parseApp = do
  f <- parseAtomic
  args <- many parseAtomic
  pure $ foldl App f args

parseAtomic :: Parser Term
parseAtomic =
  (Prop <$> (keyword "Prop" *> identifier)) <|>
  (Universe Computational 0 <$ keyword "U0") <|>
  (Universe Computational 1 <$ keyword "U1") <|>
  (Universe Computational 0 <$ keyword "U") <|>
  (Universe Logical 0 <$ keyword "Type0") <|>
  (Universe Logical 1 <$ keyword "Type1") <|>
  (Universe Logical 1 <$ keyword "Type") <|>
  (Val <$> (keyword "val" *> stringLit) <*> pure Nothing) <|>
  (Time <$> (keyword "time" *> stringLit)) <|>
  (Ident <$> (keyword "id" *> stringLit)) <|>
  try parseLet <|>  -- let 必须在 Var 之前，否则 "let" 被解析为变量
  (Var <$> identifier) <|>
  (symbol "(" *> parseTerm <* symbol ")") <|>
  parsePair <|>
  parseInL <|>
  parseInR <|>
  parseInLS <|>
  parseInRS <|>
  parseId <|>
  parseRefl <|>
  parseSplit <|>
  parseCase <|>
  parseGt <|>
  parseGe <|>
  parseLt <|>
  parseLe <|>
  parseEq

parsePair :: Parser Term
parsePair = do
  symbol "<"
  d <- parseTerm
  symbol ","
  p <- parseTerm
  symbol ">"
  pure $ Pair d p

parseInL :: Parser Term
parseInL = try $ do
  keyword "inl"
  symbol "("
  a <- parseTerm
  symbol ","
  b <- parseTerm
  symbol ","
  v <- parseTerm
  symbol ")"
  pure $ InL a b v

parseInR :: Parser Term
parseInR = try $ do
  keyword "inr"
  symbol "("
  a <- parseTerm
  symbol ","
  b <- parseTerm
  symbol ","
  v <- parseTerm
  symbol ")"
  pure $ InR a b v

parseInLS :: Parser Term
parseInLS = try $ do
  keyword "inl"
  v <- parseAtomic
  pure $ InLS v

parseInRS :: Parser Term
parseInRS = try $ do
  keyword "inr"
  v <- parseAtomic
  pure $ InRS v

parseId :: Parser Term
parseId = do
  try (keyword "Id" <|> keyword "IdA")
  symbol "("
  aTy <- parseTerm
  symbol ","
  a <- parseTerm
  symbol ","
  b <- parseTerm
  symbol ")"
  pure $ Id aTy a b

parseRefl :: Parser Term
parseRefl = do
  keyword "refl"
  optional (symbol "(")
  w <- parseTerm
  optional (symbol ")")
  pure $ Refl w

parseSplit :: Parser Term
parseSplit = do
  keyword "split"
  symbol "("
  p <- parseTerm
  symbol ")"
  keyword "as"
  x <- identifier
  y <- identifier
  keyword "in"
  body <- parseTerm
  pure $ Split p x y body

parseGt :: Parser Term
parseGt = try $ do
  keyword "gt"
  symbol "("
  a <- parseTerm
  symbol ","
  b <- parseTerm
  symbol ")"
  pure $ Gt a b

parseGe :: Parser Term
parseGe = try $ do
  keyword "ge"
  symbol "("
  a <- parseTerm
  symbol ","
  b <- parseTerm
  symbol ")"
  pure $ Ge a b

parseLt :: Parser Term
parseLt = try $ do
  keyword "lt"
  symbol "("
  a <- parseTerm
  symbol ","
  b <- parseTerm
  symbol ")"
  pure $ Lt a b

parseLe :: Parser Term
parseLe = try $ do
  keyword "le"
  symbol "("
  a <- parseTerm
  symbol ","
  b <- parseTerm
  symbol ")"
  pure $ Le a b

parseEq :: Parser Term
parseEq = try $ do
  keyword "eq"
  symbol "("
  a <- parseTerm
  symbol ","
  b <- parseTerm
  symbol ")"
  pure $ Eq a b

parseLet :: Parser Term
parseLet = do
  keyword "let"
  x <- identifier
  symbol ":"
  ty <- parseTerm
  symbol ":="
  val <- parseTerm
  keyword "in"
  body <- parseTerm
  pure $ Let x ty val body

parseCase :: Parser Term
parseCase = do
  keyword "case"
  s <- parseTerm
  keyword "of"
  keyword "inl"
  x1 <- identifier
  symbol "->"
  t1 <- parseTerm
  symbol ";"
  keyword "inr"
  x2 <- identifier
  symbol "->"
  t2 <- parseTerm
  pure $ Case s x1 t1 x2 t2

-- | 解析完整模块
parseModule :: Parser Module
parseModule = do
  Tok.whiteSpace lexer
  keyword "module"
  name <- identifier
  keyword "where"
  decls <- many parseDeclaration
  pure $ Module name decls

parseDeclaration :: Parser Declaration
parseDeclaration =
  (do keyword "type"
      name <- identifier
      symbol ":"
      ty <- parseTerm
      pure $ TypeDef name ty) <|>
  (do keyword "def"
      name <- identifier
      symbol ":"
      ty <- parseTerm
      symbol ":="
      body <- parseTerm
      pure $ TermDef name ty body)

-- | 解析入口：从源码解析 Term
parseTermFromSource :: Text -> Either ParseError Term
parseTermFromSource src = parse (Tok.whiteSpace lexer *> parseTerm <* eof) "" (T.unpack src)

-- | 解析入口：从源码解析 Module
parseModuleFromSource :: Text -> Either ParseError Module
parseModuleFromSource src = parse (parseModule <* eof) "" (T.unpack src)
