module Main where

import Control.Monad (foldM)
import IR (generateIR, generateIRASTStream)
import Parser (parse)
import System.Environment (getArgs)
import Tokenizer (tokenize)
import TypeChecker (typeCheckASTStream, typeCheckBase)

main :: IO ()
main = do
  putStrLn "~ COMPILER ~"
  args <- getArgs
  case args of
    [file] -> do
      putStrLn $ "Compiling " ++ file
      source <- readFile file
      let tokens = tokenize source
      let ast = parse tokens
      case ast of
        Left err -> error $ show err
        Right astStream -> do
          let astTypes = typeCheckASTStream astStream
          case astTypes of
            Left err -> error $ show err
            Right (_, symTab) -> do
              let ir = generateIRASTStream symTab astStream
              putStrLn $ unlines $ map (show . fst) ir
    _ -> putStrLn "Usage: compiler <file>"
