module Main where

import Parser (parse)
import System.Environment (getArgs)
import Tokenizer (tokenize)
import TypeChecker (typeCheckBase)

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
          let astTypes = mapM typeCheckBase astStream
          case astTypes of
            Left err -> error $ show err
            Right _ -> putStrLn "Typechecking successful"
    _ -> putStrLn "Usage: compiler <file>"
