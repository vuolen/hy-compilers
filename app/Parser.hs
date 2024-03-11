{-# LANGUAGE LambdaCase #-}

module Parser where

import Control.Applicative (Alternative (empty, (<|>)), asum)
import Control.Exception (throw)
import Control.Monad.Error.Class (withError)
import Control.Monad.Except (ExceptT, MonadError, catchError, mapError, runExceptT, throwError, withExceptT)
import Control.Monad.Identity
import Control.Monad.Loops (untilM)
import Control.Monad.State.Lazy (MonadState, StateT, evalStateT, get, gets, modify, put, runStateT)
import Data.Int (Int64)
import Data.List (uncons)
import Data.Maybe (isJust)
import Data.Tree (drawTree, unfoldTree)
import Debug.Trace (trace, traceM, traceWith)
import Tokenizer
  ( Location,
    Token (Identifier, IntegerLiteral, Operator, Punctuation),
  )
import Prelude

data Expr = IntegerLiteralAST Int64 | BooleanLiteralAST Bool | UnitAST | IdentifierAST String | Apply AST AST | IfAST AST AST AST | BlockAST [AST] AST | VarDeclAST AST AST deriving (Eq, Show)

data AST = AST {expr :: Expr, location :: Location} deriving (Eq, Show)

prettyPrint :: AST -> String
prettyPrint ast = drawTree $ unfoldTree unfold' ast
  where
    unfold' :: AST -> (String, [AST])
    unfold' (Apply fn arg) = ("Apply", [fn, arg])
    unfold' (IfAST cond thenBranch elseBranch) = ("If", [cond, thenBranch, elseBranch])
    unfold' (BlockAST exprs value) = ("Block", exprs ++ [value])
    unfold' ast = (show ast, [])

applyArgs :: String -> [AST] -> AST
applyArgs op [] = applyArgs op [UnitAST]
applyArgs op args = foldl Apply (IdentifierAST op) args

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

data ParserState
  = ParserState
  { tokens :: [(Token, Location)],
    consumed :: Int
  }
  deriving
    (Eq, Show)

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

instance MonadFail Parser where
  fail _ = throwMessage "Failed to match pattern"

-- yes im rolling out my own parser combinators, what are you going to do about it

instance Alternative Parser where
  empty = throwMessage "Empty parser"
  p1 <|> p2 = do
    oldState <- get
    p1
      `catchError` ( \err ->
                       do
                         put oldState
                         p2
                   )

satisfy :: (Token -> Bool) -> Parser (Token, Location)
satisfy predicate = do
  token <- peek
  case token of
    Just (token, loc) ->
      do
        if predicate token
          then do
            consume
            return (token, loc)
          else throwMessage $ "predicate did not satisfy token " ++ show token ++ " at " ++ show loc
    _ -> throwMessage "unexpected eof"

satisfyToken :: Token -> Parser (Token, Location)
satisfyToken token = satisfy (== token)

-- return only location, since the token is obvious
satisfyIdentifier id = snd <$> satisfy (== Identifier id)

many :: Parser a -> Parser [a]
many p = many1 p <|> return []

many1 :: Parser a -> Parser [a]
many1 p = do
  x <- p
  xs <- many p
  return $ x : xs

optional :: Parser a -> Parser Bool
optional p = (p >> return True) <|> return False

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

semicolon :: Parser ()
semicolon = semicolon' <|> inferSemicolon
  where
    semicolon' = do
      satisfyToken (Punctuation ";")
      return ()
    inferSemicolon = do
      state <- get
      let previous = tokens state !! (consumed state - 1)
      next <- peek
      case (previous, next) of
        ((Punctuation "}", _), Just (Punctuation "}", loc)) -> throwMessage $ "Expected semicolon before " ++ show loc
        ((Punctuation "}", _), _) -> return ()
        (_, Just (_, loc)) -> throwMessage $ "Expected semicolon before " ++ show loc

integerLiteral = do
  (IntegerLiteral value, loc) <- satisfy isIntegerLiteral
  return (IntegerLiteralAST value, loc)
  where
    isIntegerLiteral token = case token of
      IntegerLiteral _ -> True
      _ -> False

identifier = do
  ((Identifier id), loc) <- satisfy isIdentifier
  return (IdentifierAST id, loc)
  where
    isIdentifier token = case token of
      Identifier _ -> True
      _ -> False

boolean = do
  true <|> false
  where
    true = (BooleanLiteralAST True,) <$> satisfyIdentifier "true"
    false = (BooleanLiteralAST False,) <$> satisfyIdentifier "false"

unit = (UnitAST,) <$> satisfyIdentifier "unit"

unaryOperator = not <|> negate
  where
    not = do
      loc <- satisfyIdentifier "not"
      (expr, _) <- parseExpr
      return (applyArgs "not" [expr], loc)
    negate = do
      (_, loc) <- satisfyToken (Operator "-")
      (expr, _) <- parseExpr
      return (applyArgs "-" [expr], loc)

ifExpr = do
  loc <- satisfyIdentifier "if"
  (condExpr, _) <- parseExpr
  satisfyIdentifier "then"
  (thenExpr, _) <- parseExpr
  elseBranch <- elseBranch <|> return UnitAST
  return (IfAST condExpr thenExpr elseBranch, loc)
  where
    elseBranch = do
      satisfyIdentifier "else"
      (elseExpr, _) <- parseExpr
      return elseExpr

while = do
  loc <- satisfyIdentifier "while"
  (cond, _) <- parseExpr
  satisfyIdentifier "do"
  (body, _) <- parseExpr
  return (applyArgs "while" [cond, body], loc)

block = do
  (_, loc) <- satisfyToken (Punctuation "{")
  exprs <- many $ do
    expr <- variableDeclaration <|> parseExpr
    semicolon
    return expr
  value <- valueExpr <|> noValueExpr
  return (BlockAST (fmap fst exprs) value, loc)
  where
    valueExpr = do
      (expr, _) <- variableDeclaration <|> parseExpr
      satisfyToken (Punctuation "}")
      return expr
    noValueExpr = do
      satisfyToken (Punctuation "}")
      return UnitAST

funCall = do
  (IdentifierAST id, loc) <- identifier
  args <- argumentList
  return (applyArgs id args, loc)
  where
    argumentList = do
      satisfyToken $ Punctuation "("
      args <- many $ do
        (expr, _) <- parseExpr
        return expr
      satisfyToken $ Punctuation ")"
      return args

variableDeclaration = do
  loc <- satisfyIdentifier "var"
  (id, _) <- identifier
  satisfyToken (Operator "=")
  (expr, _) <- parseExpr
  return $ (VarDeclAST id expr, loc)

parseValue :: Parser (AST, Location)
parseValue = do
  foldr (<|>) noMatch [funCall, block, while, ifExpr, unaryOperator, integerLiteral, boolean, unit, old]
  where
    noMatch = do
      token <- peek
      throwMessage $ "no parser matched token " ++ show token
    -- old parser to be removed
    old = do
      (token, loc) <- consume
      case token of
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
        _ -> throwMessage $ "No parser matching token " ++ show token ++ " at loc " ++ show loc

-- parse expressions until condition
parseExpressionList :: Parser Bool -> Parser ASTStream
parseExpressionList stopCondition = parseExpressionList' []
  where
    parseExpressionList' :: ASTStream -> Parser ASTStream
    parseExpressionList' exprs = do
      expr <- variableDeclaration <|> parseExpr
      stop <- stopCondition
      if stop
        then return $ exprs ++ [expr]
        else do
          semicolon
          parseExpressionList' (exprs ++ [expr])

runParser :: forall a. Parser a -> [(Token, Location)] -> Either ParserError a
runParser (Parser parser) tokens = runIdentity $ evalStateT (runExceptT parser) (ParserState {tokens = tokens, consumed = 0})

parse :: [(Token, Location)] -> Either ParserError [(AST, Location)]
parse tokens = runParser (parseExpressionList isEmpty) tokens