module Parser
  ( AST (..),
    ASTNode (..),
    ParserError (..),
    parse,
    prettyPrint,
  )
where

import Control.Applicative (Alternative (empty, (<|>)), asum)
import Control.Exception (throw)
import Control.Monad.Error.Class (withError)
import Control.Monad.Except (ExceptT, MonadError, catchError, mapError, runExceptT, throwError, withExceptT)
import Control.Monad.Identity
import Control.Monad.Loops (untilM)
import Control.Monad.State.Lazy (MonadState, StateT, evalStateT, get, gets, modify, put, runStateT)
import Data.Either (isLeft)
import Data.Int (Int64)
import Data.List (uncons)
import Data.Maybe (isJust)
import Data.Tree (drawTree, unfoldTree)
import Debug.Trace (trace, traceM, traceWith)
import EitherState (EitherState, runEitherState)
import Tokenizer qualified as T
  ( Location (..),
    Token (Identifier, IntegerLiteral, Operator, Punctuation),
  )
import Prelude

data ASTNode = ASTNode {ast :: AST, loc :: T.Location} deriving (Eq, Show)

data AST = IntegerLiteral Int64 | BooleanLiteral Bool | Unit | IdentifierAST String | Apply ASTNode [ASTNode] | If ASTNode ASTNode ASTNode | Block [ASTNode] ASTNode | VarDecl ASTNode ASTNode deriving (Eq, Show)

prettyPrint :: ASTNode -> String
prettyPrint astnode = drawTree $ unfoldTree unfold' (ast astnode)
  where
    unfold' :: AST -> (String, [AST])
    unfold' (Apply fn args) = ("Apply", ast fn : map ast args)
    unfold' (If cond thenBranch elseBranch) = ("If", [ast cond, ast thenBranch, ast elseBranch])
    unfold' (Block exprs value) = ("Block", map ast exprs ++ [ast value])
    unfold' ast = (show ast, [])

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

isLeftAssociative :: String -> Bool
isLeftAssociative "=" = False
isLeftAssociative _ = True

data ParserState
  = ParserState
  { tokens :: [(T.Token, T.Location)],
    consumed :: Int
  }
  deriving
    (Eq, Show)

data ParserError = ParserError {message :: String}

type ASTStream = [ASTNode]

instance Show ParserError where
  show (ParserError {message}) =
    unlines
      [ "Parser Error",
        "Message: " ++ message
      ]

throwMessage :: forall a. String -> Parser a
throwMessage message = throwError $ ParserError message

type Parser a = EitherState ParserError ParserState a

instance MonadFail (EitherState ParserError ParserState) where
  fail _ = throwMessage "Failed to match pattern"

-- yes im rolling out my own parser combinators, what are you going to do about it

instance Alternative (EitherState ParserError ParserState) where
  empty = throwMessage "Empty parser"
  p1 <|> p2 = do
    oldState <- get
    p1
      `catchError` ( \err ->
                       do
                         put oldState
                         p2
                   )

satisfy :: (T.Token -> Bool) -> Parser (T.Token, T.Location)
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

satisfyToken :: T.Token -> Parser (T.Token, T.Location)
satisfyToken token = satisfy (== token)

-- return only location, since the token is obvious
satisfyIdentifier id = snd <$> satisfy (== T.Identifier id)

many :: Parser a -> Parser [a]
many p = many1 p <|> return []

many1 :: Parser a -> Parser [a]
many1 p = do
  x <- p
  xs <- many p
  return $ x : xs

delimited :: Parser a1 -> Parser a2 -> Parser [a1]
delimited p d = do
  xs <- many $ do
    x <- p
    d
    return x
  last <- p
  return $ xs ++ [last]

optional :: Parser a -> Parser Bool
optional p = (p >> return True) <|> return False

peek :: Parser (Maybe (T.Token, T.Location))
peek = do
  empty <- isEmpty
  if empty
    then return Nothing
    else do
      state <- get
      return $ Just $ tokens state !! consumed state

consume :: Parser (T.Token, T.Location)
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

consumeIf :: T.Token -> Parser Bool
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

parseExpr :: Parser ASTNode
parseExpr = do
  parseExpr' 0
  where
    parseExpr' :: Int -> Parser ASTNode
    parseExpr' prec = do
      left <- parseValue
      let loop left = do
            nextToken <- peek
            case nextToken of
              Just (T.Operator op, opLoc) ->
                if precedence op >= prec
                  then do
                    consume
                    let newPrecedence = if isLeftAssociative op then precedence op + 1 else precedence op
                    right <- parseExpr' newPrecedence
                    let newLeft = ASTNode (Apply (ASTNode (IdentifierAST op) opLoc) [left, right]) opLoc
                    loop newLeft
                  else do
                    return left
              _ -> return left
      loop left
    isOperator token = case token of
      T.Operator _ -> True
      _ -> False

semicolon :: Parser ()
semicolon = semicolon' <|> inferSemicolon
  where
    semicolon' = do
      satisfyToken (T.Punctuation ";")
      return ()
    inferSemicolon = do
      state <- get
      let previous = tokens state !! (consumed state - 1)
      next <- peek
      case (previous, next) of
        ((T.Punctuation "}", _), Just (T.Punctuation "}", loc)) -> throwMessage $ "Expected semicolon before " ++ show loc
        ((T.Punctuation "}", _), _) -> return ()
        (_, Just (_, loc)) -> throwMessage $ "Expected semicolon before " ++ show loc

integerLiteral :: Parser ASTNode
integerLiteral = do
  (T.IntegerLiteral value, loc) <- satisfy isIntegerLiteral
  return $ ASTNode (IntegerLiteral value) loc
  where
    isIntegerLiteral token = case token of
      T.IntegerLiteral _ -> True
      _ -> False

matchIdentifier :: String -> Parser ASTNode
matchIdentifier match = do
  id <- identifier
  if ast id == IdentifierAST match
    then return id
    else throwMessage $ "Expected identifier " ++ match ++ " but got " ++ show (ast id)

identifier :: Parser ASTNode
identifier = do
  (T.Identifier id, loc) <- satisfy isIdentifier
  return $ ASTNode (IdentifierAST id) loc
  where
    isIdentifier token = case token of
      T.Identifier _ -> True
      _ -> False

boolean :: Parser ASTNode
boolean = do
  true <|> false
  where
    true = ASTNode (BooleanLiteral True) <$> satisfyIdentifier "true"
    false = ASTNode (BooleanLiteral False) <$> satisfyIdentifier "false"

unit :: Parser ASTNode
unit = ASTNode Unit <$> satisfyIdentifier "unit"

unaryOperator = not <|> negate
  where
    not = do
      keyword <- matchIdentifier "not"
      expr <- parseExpr
      return $ ASTNode (Apply keyword [expr]) (loc keyword)
    negate = do
      (_, loc) <- satisfyToken (T.Operator "-")
      expr <- parseExpr
      let keyword = ASTNode (IdentifierAST "-") loc
      return $ ASTNode (Apply keyword [expr]) loc

ifExpr :: Parser ASTNode
ifExpr = do
  keyword <- matchIdentifier "if"
  condExpr <- parseExpr
  satisfyIdentifier "then"
  thenExpr <- parseExpr
  elseBranch <- elseBranch <|> return (ASTNode Unit T.NoLocation)
  return $ ASTNode (If condExpr thenExpr elseBranch) (loc keyword)
  where
    elseBranch = do
      satisfyIdentifier "else"
      parseExpr

while :: Parser ASTNode
while = do
  keyword <- matchIdentifier "while"
  cond <- parseExpr
  satisfyIdentifier "do"
  body <- parseExpr
  return $ ASTNode (Apply keyword [cond, body]) (loc keyword)

block :: Parser ASTNode
block = do
  (_, loc) <- satisfyToken (T.Punctuation "{")
  exprs <- many $ do
    expr <- variableDeclaration <|> parseExpr
    semicolon
    return expr
  value <- valueExpr <|> noValueExpr
  return $ ASTNode (Block exprs value) loc
  where
    valueExpr = do
      expr <- variableDeclaration <|> parseExpr
      satisfyToken (T.Punctuation "}")
      return expr
    noValueExpr = do
      satisfyToken (T.Punctuation "}")
      return $ ASTNode Unit T.NoLocation

funCall = do
  id <- identifier
  args <- argumentList
  return $ ASTNode (Apply id args) (loc id)
  where
    argumentList = do
      (_, loc) <- satisfyToken $ T.Punctuation "("
      let comma = satisfyToken $ T.Punctuation ","
      -- if the arg list fails, return Unit
      args <-
        delimited parseExpr comma <|> return [ASTNode Unit loc]
      satisfyToken $ T.Punctuation ")"
      return args

variableDeclaration :: Parser ASTNode
variableDeclaration = do
  loc <- satisfyIdentifier "var"
  id <- identifier
  satisfyToken (T.Operator "=")
  expr <- parseExpr
  return $ ASTNode (VarDecl id expr) loc

parseValue :: Parser ASTNode
parseValue = do
  let astNodeToTuple astnode = (ast astnode, loc astnode)
  foldr (<|>) noMatch [funCall, block, while, ifExpr, unaryOperator, integerLiteral, boolean, unit, identifier, old]
  where
    noMatch = do
      token <- peek
      throwMessage $ "no parser matched token " ++ show token
    -- old parser to be removed
    old = do
      (token, loc) <- consume
      case token of
        T.Punctuation "(" -> do
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

runParser :: Parser a -> [(T.Token, T.Location)] -> Either ParserError a
runParser parser tokens = result
  where
    (result, _) = runEitherState parser (ParserState {tokens = tokens, consumed = 0})

parse :: [(T.Token, T.Location)] -> Either ParserError ASTStream
parse tokens = runParser (parseExpressionList isEmpty) tokens