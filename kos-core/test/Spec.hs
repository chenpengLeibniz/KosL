{-# LANGUAGE OverloadedStrings #-}
-- | KosCore 测试（无 hspec 依赖）

module Main where

import KosCore
import KosCore.Parser
import KosCore.TypeCheck
import KosCore.Context
import KosCore.Reduction
import KosCore.AST
import qualified Data.Text as T

main :: IO ()
main = do
  testParser
  testTypeWellFormed
  testParseAndCheck
  testIdRefl
  testConversion
  testReductions
  testModuleWithDef
  putStrLn "All tests passed."

testParser :: IO ()
testParser = do
  assert "parse Prop" $ isRight (parseTermFromSource "Prop P")
  assert "parse U0" $ isRight (parseTermFromSource "U0")
  assert "parse Pi" $ isRight (parseTermFromSource "Pi(x:U0). x -> x")
  assert "parse Sigma" $ isRight (parseTermFromSource "Sigma(x:U0). x")
  assert "parse pair" $ isRight (parseTermFromSource "< Prop P , Prop Q >")
  assert "parse Let" $ case parseTermFromSource "let x : U0 := Prop P in x" of
    Right (Let _ _ _ _) -> True
    _ -> False
  -- Let with Type1 (Prop P : Type1)
  assert "parse Let Type1" $ isRight (parseTermFromSource "let x : Type1 := Prop P in x")
  assert "parse inl single" $ isRight (parseTermFromSource "inl (Prop P)")
  assert "parse Id" $ isRight (parseTermFromSource "Id(U0, Prop P, Prop P)")
  assert "parse refl" $ isRight (parseTermFromSource "refl (Prop P)")

testTypeWellFormed :: IO ()
testTypeWellFormed = do
  assert "Prop well-formed" $ typeWellFormed (Prop "P")
  assert "Universe well-formed" $ typeWellFormed (Universe Computational 0)
  assert "Pi well-formed" $ typeWellFormed (Pi (T.pack "x") (Universe Computational 0) (Universe Computational 0))
  assert "Id well-formed" $ typeWellFormed (Id (Universe Computational 0) (Prop "P") (Prop "P"))
  assert "Pair not type" $ not (typeWellFormed (Pair (Prop "P") (Prop "Q")))

testParseAndCheck :: IO ()
testParseAndCheck = do
  assert "check Prop" $ isRight (parseAndCheckTerm "Prop P")
  assert "check U0" $ isRight (parseAndCheckTerm "U0")
  assert "check Let" $ isRight (parseAndCheckTerm "let x : Type1 := Prop P in x")
  assert "check refl" $ isRight (parseAndCheckTerm "refl (Prop P)")

testIdRefl :: IO ()
testIdRefl = do
  case parseAndCheckTerm "refl (Prop P)" of
    Right (_, ty) -> assert "refl type is Id" $ case ty of Id{} -> True; _ -> False
    Left _ -> error "FAIL: refl should type check"

testConversion :: IO ()
testConversion = do
  -- lam 恒等函数可类型检查
  assert "conversion: lam identity" $ isRight (parseAndCheckTerm "lam (x : Type1) . x")
  case parseAndCheckTerm "lam (x : Type1) . x" of
    Right (_, ty) -> assert "lam identity type" $ case ty of Pi{} -> True; _ -> False
    Left _ -> error "FAIL: lam identity"

testReductions :: IO ()
testReductions = do
  -- ζ: Let 展开
  case parseTermFromSource "let x : Type1 := Prop P in x" of
    Right t -> do
      let n = normalize empty t
      assert "zeta: Let normalizes" $ case n of Prop _ -> True; _ -> False
    Left _ -> error "FAIL: parse Let"
  -- β: 应用归约 (lam (x : Type1) . x) (Prop P) -> Prop P
  case parseTermFromSource "lam (x : Type1) . x" of
    Right lam ->
      case parseTermFromSource "Prop P" of
        Right arg ->
          let app = App lam arg
              n = normalize empty app
          in assert "beta: App normalizes" $ case n of Prop _ -> True; _ -> False
        Left _ -> error "FAIL: parse Prop"
    Left _ -> error "FAIL: parse lam"

testModuleWithDef :: IO ()
testModuleWithDef = do
  let modSrc = T.unlines
        [ "module WithDef where"
        , "type T : Type1"
        , "def p : T := Prop P"
        , "def q : T := p"
        ]
  assert "module with def" $ isRight (parseAndCheckModule modSrc)

assert :: String -> Bool -> IO ()
assert name True = putStrLn $ "  " ++ name ++ " OK"
assert name False = error $ "FAIL: " ++ name

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _ = False
