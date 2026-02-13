{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
-- | KosCore.JSON - 序列化 Term 为 C kos_term 兼容的 JSON 格式
-- 用于 Bridge：kos-core 校验后输出 JSON，C 层反序列化为 kos_term

module KosCore.JSON where

import KosCore.AST
import KosCore.Reduction
import KosCore.Context
import Data.List (intersperse)
import Data.Text (Text)
import qualified Data.Text as T

-- | 序列化 Term 为 C kos_term JSON 格式（先正规化）
termToJSON :: Term -> Text
termToJSON t = termToJSON' (normalize empty t)

termToJSON' :: Term -> Text
termToJSON' = \case
  Val v mt ->
    T.pack $ jsonObj ( [ ("kind", T.unpack $ jsonStr "VAL")
                       , ("val", T.unpack $ jsonStr v)
                       ] ++ maybe [] (\x -> [("type", T.unpack $ termToJSON' x)]) mt
                     )
  Time t -> T.pack $ jsonObj [("kind", T.unpack $ jsonStr "TIME"), ("val", T.unpack $ jsonStr t)]
  Ident i -> T.pack $ jsonObj [("kind", T.unpack $ jsonStr "ID"), ("val", T.unpack $ jsonStr i)]
  Prop n -> T.pack $ jsonObj [("kind", T.unpack $ jsonStr "PROP"), ("val", T.unpack $ jsonStr n)]
  Universe ax lev ->
    T.pack $ jsonObj [ ("kind", T.unpack $ jsonStr $ case ax of Computational -> "U"; Logical -> "TYPE")
                     , ("axis", show $ case ax of Computational -> 0; Logical -> 1)
                     , ("level", show lev)
                     ]
  Pi _ dom body ->
    T.pack $ jsonObj [ ("kind", T.unpack $ jsonStr "PI")
                     , ("domain", T.unpack $ termToJSON' dom)
                     , ("body", T.unpack $ termToJSON' body)
                     ]
  Sigma _ dom body ->
    T.pack $ jsonObj [ ("kind", T.unpack $ jsonStr "SIGMA")
                     , ("domain", T.unpack $ termToJSON' dom)
                     , ("body", T.unpack $ termToJSON' body)
                     ]
  Pair d p ->
    T.pack $ jsonObj [ ("kind", T.unpack $ jsonStr "PAIR")
                     , ("data", T.unpack $ termToJSON' d)
                     , ("proof", T.unpack $ termToJSON' p)
                     ]
  Sum a b ->
    T.pack $ jsonObj [ ("kind", T.unpack $ jsonStr "SUM")
                     , ("left_type", T.unpack $ termToJSON' a)
                     , ("right_type", T.unpack $ termToJSON' b)
                     ]
  InL a b v ->
    T.pack $ jsonObj [ ("kind", T.unpack $ jsonStr "SUM")
                     , ("left_type", T.unpack $ termToJSON' a)
                     , ("right_type", T.unpack $ termToJSON' b)
                     , ("is_left", "true")
                     , ("value", T.unpack $ termToJSON' v)
                     ]
  InR a b v ->
    T.pack $ jsonObj [ ("kind", T.unpack $ jsonStr "SUM")
                     , ("left_type", T.unpack $ termToJSON' a)
                     , ("right_type", T.unpack $ termToJSON' b)
                     , ("is_left", "false")
                     , ("value", T.unpack $ termToJSON' v)
                     ]
  Var x -> T.pack $ jsonObj [("kind", T.unpack $ jsonStr "ID"), ("val", T.unpack $ jsonStr x)]
  Gt a b -> T.pack $ jsonObj [ ("kind", T.unpack $ jsonStr "GT")
                             , ("left", T.unpack $ termToJSON' a)
                             , ("right", T.unpack $ termToJSON' b)
                             ]
  Ge a b -> T.pack $ jsonObj [ ("kind", T.unpack $ jsonStr "GE")
                             , ("left", T.unpack $ termToJSON' a)
                             , ("right", T.unpack $ termToJSON' b)
                             ]
  Lt a b -> T.pack $ jsonObj [ ("kind", T.unpack $ jsonStr "LT")
                             , ("left", T.unpack $ termToJSON' a)
                             , ("right", T.unpack $ termToJSON' b)
                             ]
  Le a b -> T.pack $ jsonObj [ ("kind", T.unpack $ jsonStr "LE")
                             , ("left", T.unpack $ termToJSON' a)
                             , ("right", T.unpack $ termToJSON' b)
                             ]
  Eq a b -> T.pack $ jsonObj [ ("kind", T.unpack $ jsonStr "EQ")
                             , ("left", T.unpack $ termToJSON' a)
                             , ("right", T.unpack $ termToJSON' b)
                             ]
  Id ty a b -> T.pack $ jsonObj [ ("kind", T.unpack $ jsonStr "EQID")
                                , ("type", T.unpack $ termToJSON' ty)
                                , ("left", T.unpack $ termToJSON' a)
                                , ("right", T.unpack $ termToJSON' b)
                                ]
  Refl w -> T.pack $ jsonObj [("kind", T.unpack $ jsonStr "REFL"), ("term", T.unpack $ termToJSON' w)]
  Sym e -> T.pack $ jsonObj [("kind", T.unpack $ jsonStr "SYM"), ("eq", T.unpack $ termToJSON' e)]
  Transport p eq t -> T.pack $ jsonObj [ ("kind", T.unpack $ jsonStr "TRANSPORT")
                                       , ("family", T.unpack $ termToJSON' p)
                                       , ("eq", T.unpack $ termToJSON' eq)
                                       , ("proof", T.unpack $ termToJSON' t)
                                       ]
  App f a -> T.pack $ jsonObj [ ("kind", T.unpack $ jsonStr "APP")
                              , ("func", T.unpack $ termToJSON' f)
                              , ("arg", T.unpack $ termToJSON' a)
                              ]
  Lam x ty body -> T.pack $ jsonObj [ ("kind", T.unpack $ jsonStr "LAM")
                                    , ("var", T.unpack $ jsonStr x)
                                    , ("type", T.unpack $ termToJSON' ty)
                                    , ("body", T.unpack $ termToJSON' body)
                                    ]
  Triv -> T.pack $ jsonObj [("kind", T.unpack $ jsonStr "PROP"), ("val", T.unpack $ jsonStr "Triv")]
  _ -> T.pack $ jsonObj [("kind", T.unpack $ jsonStr "PROP"), ("val", T.unpack $ jsonStr "?")]

jsonStr :: Text -> Text
jsonStr s = T.pack $ "\"" ++ escape (T.unpack s) ++ "\""
  where escape [] = []
        escape ('"' : xs) = '\\' : '"' : escape xs
        escape ('\\' : xs) = '\\' : '\\' : escape xs
        escape (x : xs) = x : escape xs

jsonObj :: [(String, String)] -> String
jsonObj kvs = "{" ++ (concat $ intersperse "," [ "\"" ++ k ++ "\":" ++ v | (k,v) <- kvs ]) ++ "}"
