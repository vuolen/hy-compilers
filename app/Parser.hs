module Parser where

import Control.Applicative (Alternative)
import Control.Monad.Error.Class (withError)
import Control.Monad.Except (ExceptT, MonadError, catchError, mapError, runExceptT, throwError, withExceptT)
import Control.Monad.Identity
import Control.Monad.Loops (untilM)
import Control.Monad.State.Lazy (MonadState, StateT, evalStateT, get, gets, modify, put, runStateT)
import Data.Int (Int64)
import Data.List (uncons)
import Data.Maybe (isJust)
import Data.Tree (drawTree, unfoldTree)
import Debug.Trace (trace, traceM)
import GHC.Base (Alternative (..))
import Tokenizer
  ( Location,
    Token (Identifier, IntegerLiteral, Operator, Punctuation),
  )
import Prelude

data AST = IntegerLiteralAST Int64 | BooleanLiteralAST Bool | UnitAST | IdentifierAST String | Apply AST AST | IfAST AST AST AST | BlockAST [AST] AST deriving (Eq, Show)

prettyPrint :: AST -> String
prettyPrint ast = drawTree $ unfoldTree unfold' ast
  where
    unfold' :: AST -> (String, [AST])
    unfold' (Apply fn arg) = ("Apply", [fn, arg])
    unfold' (IfAST cond thenBranch elseBranch) = ("If", [cond, thenBranch, elseBranch])
    unfold' (BlockAST exprs value) = ("Block", exprs ++ [value])
    unfold' ast = (show ast, [])

applyArgs :: String -> [AST] -> AST
applyArgs op = foldl Apply (IdentifierAST op)

applyTwo :: String -> AST -> AST -> AST
applyTwo op ast1 ast2 = applyArgs op [ast1, ast2]

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

data ParserState = ParserState
  { tokens :: [(Token, Location)],
    consumed :: Int
  }

type ASTStream = [(AST, Location)]

data ParserError = ParserError {message :: String}

instance Show ParserError where
  show (ParserError {message}) =
    unlines
      [ "Parser Error",
        "Message: " ++ message
      ]

throwMessage :: forall a. String -> Parser a
throwMessage message = throwError $ ParserError message

newtype Parser a = Parser (ExceptT ParserError (StateT ParserState Identity) a) deriving (Functor, Applicative, Monad, MonadError ParserError, MonadState ParserState)

-- yes im rolling out my own parser combinators, what are you going to do about it

instance Alternative Parser where
  empty = throwMessage "Empty parser"
  p1 <|> p2 =
    p1
      `catchError` ( \_ -> do
                       modify (\state -> state {consumed = 0})
                       p2
                   )

satisfy :: (Token -> Bool) -> Parser (Token, Location)
satisfy predicate = do
  (token, loc) <- consume
  if predicate token
    then return (token, loc)
    else throwMessage $ "predicate did not satisfy token " ++ show token ++ " at " ++ show loc

peek :: Parser (Maybe (Token, Location))
peek = do
  empty <- isEmpty
  if empty
    then return Nothing
    else do
      state <- get
      return $ Just $ tokens state !! consumed state

consume :: Parser (Token, Location)
consume = do
  state <- get
  empty <- isEmpty
  if empty
    then throwMessage "unexpected eof"
    else do
      let t = tokens state
      let c = consumed state
      put state {consumed = c + 1}
      return (t !! c)

isEmpty :: Parser Bool
isEmpty = do
  (tokens, consumed) <- gets (\state -> (tokens state, consumed state))
  return $ consumed == length tokens

consumeIf :: Token -> Parser Bool
consumeIf token = do
  nextToken <- peek
  case nextToken of
    Just (t, _) -> do
      if t == token
        then do
          consume
          return True
        else
          return False
    Nothing -> return False

parseExpr :: Parser (AST, Location)
parseExpr = do
  nextToken <- peek
  case nextToken of
    Nothing -> throwMessage "unexpected end of input"
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
                    loop $ applyArgs op [left, right]
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
        _ -> throwMessage $ "Parse error at " ++ show loc ++ " with token " ++ show token
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
    Identifier "while" -> do
      (cond, _) <- parseExpr
      consume -- do
      (body, _) <- parseExpr
      return (applyArgs "while" [cond, body], loc)
    Identifier id -> do
      isArgumentList <- consumeIf $ Punctuation "("
      if isArgumentList
        then do
          isEmptyArgumentList <- consumeIf $ Punctuation ")"
          if isEmptyArgumentList
            then return $ (applyArgs id [UnitAST], loc)
            else do
              arglist <- loop []
              return (foldl Apply (IdentifierAST id) arglist, loc)
        else
          return (IdentifierAST id, loc)
      where
        loop args = do
          (arg, argloc) <- parseExpr
          nextToken <- consume
          case nextToken of
            (Punctuation ")", _) -> return $ args ++ [arg]
            (Punctuation ",", _) -> loop (arg : args)
            _ -> throwMessage $ "Unexpected token in argument list " ++ show nextToken ++ " " ++ show argloc
    Punctuation "(" -> do
      expr <- parseExpr
      consume
      return expr
    Punctuation "{" -> do
      isEmptyBlock <- consumeIf $ Punctuation "}"
      if isEmptyBlock
        then return (BlockAST [] UnitAST, loc)
        else do
          let blockEnd = do
                state <- get
                let t = tokens state
                let c = consumed state
                case fmap fst (drop c t) of
                  (Punctuation "}" : _) -> return True
                  (Punctuation ";" : Punctuation "}" : _) -> return True
                  _ -> return False
          exprList <- parseExpressionList blockEnd
          finalSemicolon <- consumeIf $ Punctuation ";"
          consume -- }
          let exprs = fmap fst exprList
          if finalSemicolon
            then return (BlockAST exprs UnitAST, loc)
            else return (BlockAST (init exprs) (last exprs), loc)
    _ -> throwMessage $ "No parser matching token " ++ show token ++ " at loc " ++ show loc

-- parse expressions until condition
parseExpressionList :: Parser Bool -> Parser ASTStream
parseExpressionList stopCondition = parseExpressionList' []
  where
    parseExpressionList' :: ASTStream -> Parser ASTStream
    parseExpressionList' exprs = do
      expr <- parseExpr
      stop <- stopCondition
      if stop
        then return $ exprs ++ [expr]
        else do
          isSemicolon <- consumeIf $ Punctuation ";"
          let inferSemicolon = case fst expr of
                (BlockAST _ _) -> True
                (IfAST _ (BlockAST _ _) UnitAST) -> True
                (IfAST _ _ (BlockAST _ _)) -> True
                _ -> False
          if isSemicolon || inferSemicolon
            then
              parseExpressionList' (exprs ++ [expr])
            else
              throwMessage "Missing semicolon"

runParser :: forall a. Parser a -> [(Token, Location)] -> Either ParserError a
runParser (Parser parser) tokens = runIdentity $ evalStateT (runExceptT parser) (ParserState {tokens = tokens, consumed = 0})

parse :: [(Token, Location)] -> Either ParserError [(AST, Location)]
parse tokens = runParser (parseExpressionList isEmpty) tokens