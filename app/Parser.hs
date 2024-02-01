module Parser where

import Prelude

import Tokenizer
import Data.Int (Int64)

data AST = IntegerLiteralAST Int64 | BooleanLiteralAST Bool deriving (Eq, Show)

parse :: [(Token, Location)] -> [(AST, Location)]
parse [] = []
parse ((IntegerLiteral n, loc) : rest) = [(IntegerLiteralAST n, loc)] ++ parse rest
parse ((Operator "-", loc) : (IntegerLiteral n, _) : rest) = [(IntegerLiteralAST (-n), loc)] ++ parse rest
parse ((Identifier "true", loc) : rest) = [(BooleanLiteralAST True, loc)] ++ parse rest
parse ((token, loc) : rest) = error $ "Parse error at " ++ (show loc)