module Parser where

import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.Identity
import Control.Monad.State.Lazy (StateT, evalStateT, get, put, runStateT)
import Data.Int (Int64)
import Data.List (uncons)
import Debug.Trace (trace, traceM)
import Tokenizer
  ( Location,
    Token (Identifier, IntegerLiteral, Operator, Punctuation),
  )
import Prelude

data AST = IntegerLiteralAST Int64 | BooleanLiteralAST Bool | UnitAST | IdentifierAST String | Apply AST AST | IfAST AST AST AST deriving (Eq, Show)

mkBinaryApply :: String -> AST -> AST -> AST
mkBinaryApply op left right = Apply (Apply (IdentifierAST op) left) right

-- precedence for binary operations
precedence :: String -> Int
precedence "=" = 1
precedence "or" = 2
precedence "and" = 3
precedence op
  | elem op ["==", "!="] = 4
  | elem op ["<", ">=", ">", ">="] = 5
  | elem op ["+", "-"] = 6
  | elem op ["*", "/", "%"] = 7
  | otherwise = 8

type TokenStream = ([(Token, Location)], Int)

type ASTStream = [(AST, Location)]

type Parser a = StateT TokenStream (ExceptT String Identity) a

peek :: Parser (Maybe (Token, Location))
peek = do
  (tokens, _) <- get
  case tokens of
    [] -> return Nothing
    (token : _) -> return $ Just token

consume :: Parser (Token, Location)
consume = do
  (tokens, consumed) <- get
  put (tail tokens, consumed + 1)
  return (head tokens)

isEmpty :: Parser Bool
isEmpty = do
  (tokens, _) <- get
  return $ null tokens

parseExpr :: Parser (AST, Location)
parseExpr = do
  nextToken <- peek
  case nextToken of
    Nothing -> throwError "Parse error: unexpected end of input"
    Just (_, loc) -> do
      ast <- parseExpr' 0
      return (ast, loc)
  where
    parseExpr' :: Int -> Parser AST
    parseExpr' prec = do
      (left, _) <- parseValue
      let loop left = do
            nextToken <- peek
            case nextToken of
              Just (Operator op, _) ->
                if precedence op >= prec
                  then do
                    consume
                    right <- parseExpr' $ precedence op + 1
                    loop $ mkBinaryApply op left right
                  else return left
              _ -> return left
      loop left

parseValue :: Parser (AST, Location)
parseValue = do
  (token, loc) <- consume
  case token of
    IntegerLiteral n -> return (IntegerLiteralAST n, loc)
    Operator "-" -> do
      (token, loc) <- consume
      case token of
        IntegerLiteral n -> return (IntegerLiteralAST (-n), loc)
        _ -> throwError $ "Parse error at " ++ show loc ++ " with token " ++ show token
    Identifier "true" -> return (BooleanLiteralAST True, loc)
    Identifier "false" -> return (BooleanLiteralAST False, loc)
    Identifier "unit" -> return (UnitAST, loc)
    Identifier "not" -> do
      (expr, _) <- parseExpr
      return (Apply (IdentifierAST "not") expr, loc)
    Identifier "if" -> do
      (cond, _) <- parseExpr
      consume -- then
      (thenBranch, _) <- parseExpr
      elseKeyword <- peek
      elseBranch <- case elseKeyword of
        Just (Identifier "else", _) -> do
          consume
          (expr, _) <- parseExpr
          return expr
        _ -> return UnitAST
      return (IfAST cond thenBranch elseBranch, loc)
    Identifier id -> do
      maybeArgumentList <- peek
      case maybeArgumentList of
        Just (Punctuation "(", _) -> do
          consume
          let loop args = do
                (arg, argloc) <- parseExpr
                nextToken <- consume
                case nextToken of
                  (Punctuation ")", _) -> return $ args ++ [arg]
                  (Punctuation ",", _) -> loop (arg : args)
                  _ -> throwError $ "Unexpected token in argument list " ++ show nextToken ++ " " ++ show argloc
          arglist <- loop []
          return (foldl Apply (IdentifierAST id) arglist, loc)
        _ -> return (IdentifierAST id, loc)
    Punctuation "(" -> do
      expr <- parseExpr
      consume
      return expr
    _ -> throwError $ "Parse error at " ++ show loc ++ " with token " ++ show token

parse :: [(Token, Location)] -> Either String [(AST, Location)]
parse tokens = runIdentity $ runExceptT $ evalStateT parse' (tokens, 0)
  where
    parse' :: Parser ASTStream
    parse' = do
      empty <- isEmpty
      if empty
        then return []
        else do
          (ast, loc) <- parseExpr
          rest <- parse'
          return $ (ast, loc) : rest