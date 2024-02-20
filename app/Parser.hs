module Parser where

import Prelude

import Tokenizer
import Data.Int (Int64)
import Data.List (uncons)
import Debug.Trace (trace)

data AST = IntegerLiteralAST Int64 | BooleanLiteralAST Bool | UnitAST | IdentifierAST String | Apply AST AST deriving (Eq, Show)

mkBinaryApply :: String -> AST -> AST -> AST
mkBinaryApply op left right = Apply (Apply (IdentifierAST op) left) right


-- return an AST and the number of tokens consumed

parseExpr :: [(Token, Location)] -> ((AST, Location), Int)

parseExpr tokens = let 
        ((firstAST, firstLoc), consumed) = parseValue tokens
        associative left consumed rest = 
            case uncons rest of
                Just ((Operator op, loc), rest2) -> 
                    let ((secondAST, _), consumed2) = parseValue rest2
                    in associative (mkBinaryApply op left secondAST) (consumed + 1 + consumed2) (drop consumed2 rest2)
                _ -> ((left, firstLoc), consumed)
    in 
        associative firstAST consumed (drop consumed tokens) 

parseValue :: [(Token, Location)] -> ((AST, Location), Int)
-- integers
parseValue ((IntegerLiteral n, loc) : _) = ((IntegerLiteralAST n, loc), 1)
parseValue ((Operator "-", loc) : (IntegerLiteral n, _) : _ ) = ((IntegerLiteralAST (-n), loc), 2) 

-- identifiers
parseValue ((Identifier "true", loc) : _ ) = ((BooleanLiteralAST True, loc), 1)
parseValue ((Identifier "false", loc) : _ ) = ((BooleanLiteralAST False, loc), 1)
parseValue ((Identifier "unit", loc) : _ ) = ((UnitAST, loc), 1)
parseValue ((Identifier id, loc) : _) = ((IdentifierAST id, loc), 1)

parseValue ((token, loc) : _ ) = error $ "Parse error at " ++ (show loc) ++ " with token " ++ (show token)

parse :: [(Token, Location)] -> [(AST, Location)]
parse [] = []
parse tokens = [ast] ++ parse rest
    where (ast, consumed) = parseExpr tokens
          rest = drop consumed tokens