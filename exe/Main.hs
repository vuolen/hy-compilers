module Main where

import Assembly (genAssembly)
import IR (generateIRASTStream)
import Parser (parse)
import System.Environment (getArgs)
import Tokenizer (tokenize)
import TypeChecker (typeCheckASTStream)

main :: IO ()
main = do
  source <- getContents
  args <- getArgs
  let tokens = tokenize source
  case args of
    ["tokens"] -> do
      putStrLn $ unlines $ map show tokens
      return ()
    _ -> do
      let ast = parse tokens
      case ast of
        Left err -> error $ show err
        Right astStream -> do
          let astTypes = typeCheckASTStream astStream
          case astTypes of
            Left err -> error $ show err
            Right ((finalType), _) -> do
              let ir = generateIRASTStream astStream finalType
              let asm = genAssembly ir
              putStrLn asm