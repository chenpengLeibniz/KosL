{-# LANGUAGE OverloadedStrings #-}
-- | KosCore CLI - 形式化内核命令行入口

module Main where

import KosCore
import KosCore.Parser (parseModuleFromSource, parseTermFromSource)
import KosCore.JSON (termToJSON)
import KosCore.TypeCheck (check)
import KosCore.Context (empty)
import KosCore.Proof (prove)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.IO (stderr, withFile, IOMode(ReadMode), hSetEncoding)
import GHC.IO.Encoding (utf8)

main :: IO ()
main = getArgs >>= \args ->
  case args of
    ["check", file] -> checkFile file
    ["parse", file] -> parseFile file
    ["term", expr] -> checkTerm (T.pack expr)
    ["json-term", expr] -> jsonTerm (T.pack expr)
    ["infer-term", expr] -> inferTerm (T.pack expr)
    ["check-term", termExpr, typeExpr] -> checkTermExpr (T.pack termExpr) (T.pack typeExpr)
    ["prove", goalExpr] -> autoProve Nothing (T.pack goalExpr)
    ["auto", goalExpr] -> autoProve Nothing (T.pack goalExpr)
    ["prove", "--ctx", ctxFile, goalExpr] -> autoProve (Just ctxFile) (T.pack goalExpr)
    ["auto", "--ctx", ctxFile, goalExpr] -> autoProve (Just ctxFile) (T.pack goalExpr)
    ["prove-json", "--ctx", ctxFile, goalExpr] -> autoProveJson (Just ctxFile) (T.pack goalExpr)
    _ -> do
      putStrLn "Usage: kos-core check <file.kos>"
      putStrLn "       kos-core parse <file.kos>"
      putStrLn "       kos-core term <expr>"
      putStrLn "       kos-core json-term <expr>"
      putStrLn "       kos-core infer-term <expr>     -- 类型推理，输出 {term,type} JSON"
      putStrLn "       kos-core check-term <term> <type>  -- 检查 term : type"
      putStrLn "       kos-core prove <goal>          -- 自动证明：在空上下文中对目标做证明搜索"
      putStrLn "       kos-core auto <goal>           -- 同上（prove 的别名）"
      putStrLn "       kos-core prove --ctx <file.kos> <goal>  -- 在模块提供的上下文中自动证明"
      putStrLn "       kos-core prove-json --ctx <file.kos> <goal>  -- 同上，输出证明项 JSON 供 C Bridge 使用"
      exitFailure

-- | 以 UTF-8 编码读取文件（解决 Windows 下中文注释的解码问题）
readFileUtf8 :: FilePath -> IO T.Text
readFileUtf8 path = withFile path ReadMode $ \h -> do
  hSetEncoding h utf8
  TIO.hGetContents h

checkFile :: FilePath -> IO ()
checkFile path = do
  content <- readFileUtf8 path
  let content' = T.replace (T.pack "\xFEFF") (T.pack "") content
  case parseAndCheckModule content' of
    Left err -> do
      putStrLn $ "ERROR: " ++ err
      exitFailure
    Right modu -> do
      putStrLn $ "OK Module '" ++ T.unpack (modName modu) ++ "' type checks successfully!"
      exitSuccess

parseFile :: FilePath -> IO ()
parseFile path = do
  content <- readFileUtf8 path
  let content' = T.replace (T.pack "\xFEFF") (T.pack "") content
  case parseModuleFromSource content' of
    Left err -> do
      putStrLn $ "Parse error: " ++ show err
      exitFailure
    Right modu -> do
      print modu
      exitSuccess

checkTerm :: T.Text -> IO ()
checkTerm expr = do
  case parseAndCheckTerm expr of
    Left err -> do
      putStrLn $ "ERROR: " ++ err
      exitFailure
    Right (term, ty) -> do
      putStrLn $ T.unpack expr ++ " : " ++ show ty
      exitSuccess

-- | 校验项并输出 C kos_term 兼容的 JSON（仅 stdout，供 Bridge 捕获）
jsonTerm :: T.Text -> IO ()
jsonTerm expr = do
  case parseAndCheckTerm expr of
    Left err -> do
      TIO.hPutStrLn stderr $ T.pack ("ERROR: " ++ err)
      exitFailure
    Right (term, _) -> do
      TIO.putStrLn $ termToJSON term
      exitSuccess

-- | 类型推理：输出 { "term": <term_json>, "type": <type_json> }，供 C Bridge 使用
inferTerm :: T.Text -> IO ()
inferTerm expr = do
  case parseAndCheckTerm expr of
    Left err -> do
      TIO.hPutStrLn stderr $ T.pack ("ERROR: " ++ err)
      exitFailure
    Right (term, ty) -> do
      let termJson = termToJSON term
      let typeJson = termToJSON ty
      TIO.putStrLn $ T.pack $ "{\"term\":" ++ T.unpack termJson ++ ",\"type\":" ++ T.unpack typeJson ++ "}"
      exitSuccess

-- | 检查 term : type（空上下文）
checkTermExpr :: T.Text -> T.Text -> IO ()
checkTermExpr termExpr typeExpr =
  case (parseTermFromSource termExpr, parseTermFromSource typeExpr) of
    (Left e1, _) -> do
      TIO.hPutStrLn stderr $ T.pack ("Parse term error: " ++ show e1)
      exitFailure
    (_, Left e2) -> do
      TIO.hPutStrLn stderr $ T.pack ("Parse type error: " ++ show e2)
      exitFailure
    (Right term, Right ty) ->
      case check empty term ty of
        Left err -> do
          TIO.hPutStrLn stderr $ T.pack ("Type error: " ++ show err)
          exitFailure
        Right () -> exitSuccess

-- | 自动证明：对目标类型 goal 做证明搜索；可选从 .kos 文件加载上下文
autoProve :: Maybe FilePath -> T.Text -> IO ()
autoProve mctxFile goalExpr = do
  ctx <- case mctxFile of
    Nothing -> return empty
    Just path -> do
      content <- readFileUtf8 path
      let content' = T.replace (T.pack "\xFEFF") (T.pack "") content
      case parseAndCheckModule content' of
        Left err -> do
          TIO.hPutStrLn stderr $ T.pack ("Context file error: " ++ err)
          exitFailure
        Right modu -> return (contextFromModule modu)
  case parseTermFromSource goalExpr of
    Left err -> do
      TIO.hPutStrLn stderr $ T.pack ("Parse goal error: " ++ show err)
      exitFailure
    Right goal ->
      case prove ctx goal of
        Nothing -> do
          TIO.hPutStrLn stderr $ T.pack "No proof found."
          exitFailure
        Just p ->
          case check ctx p goal of
            Left err -> do
              TIO.hPutStrLn stderr $ T.pack ("Internal: generated proof does not type-check: " ++ show err)
              exitFailure
            Right () -> do
              TIO.putStrLn $ T.pack ("Proof: " ++ show p)
              exitSuccess

-- | 自动证明并输出证明项 JSON（供 C Bridge 反序列化为 kos_term）
autoProveJson :: Maybe FilePath -> T.Text -> IO ()
autoProveJson mctxFile goalExpr = do
  ctx <- case mctxFile of
    Nothing -> do
      TIO.hPutStrLn stderr $ T.pack "prove-json requires --ctx"
      exitFailure
    Just path -> do
      content <- readFileUtf8 path
      let content' = T.replace (T.pack "\xFEFF") (T.pack "") content
      case parseAndCheckModule content' of
        Left err -> do
          TIO.hPutStrLn stderr $ T.pack ("Context file error: " ++ err)
          exitFailure
        Right modu -> return (contextFromModule modu)
  case parseTermFromSource goalExpr of
    Left err -> do
      TIO.hPutStrLn stderr $ T.pack ("Parse goal error: " ++ show err)
      exitFailure
    Right goal ->
      case prove ctx goal of
        Nothing -> do
          TIO.hPutStrLn stderr $ T.pack "No proof found."
          exitFailure
        Just p ->
          case check ctx p goal of
            Left err -> do
              TIO.hPutStrLn stderr $ T.pack ("Internal: generated proof does not type-check: " ++ show err)
              exitFailure
            Right () -> do
              TIO.putStrLn $ termToJSON p
              exitSuccess
