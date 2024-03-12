module Parser
  ( AST (..),
    ASTNode (..),
    ParserError (..),
    parse,
  )
where

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
import Tokenizer qualified as T
  ( Location (..),
    Token (Identifier, IntegerLiteral, Operator, Punctuation),
  )
import Prelude

data ASTNode = ASTNode {ast :: AST, loc :: T.Location} deriving (Eq, Show)

mkASTNode :: (AST, T.Location) -> ASTNode
mkASTNode = uncurry ASTNode

data AST = IntegerLiteral Int64 | BooleanLiteral Bool | Unit | IdentifierAST String | Apply ASTNode [ASTNode] | If AST AST AST | Block [AST] AST | VarDecl ASTNode ASTNode deriving (Eq, Show)

prettyPrint :: AST -> String
prettyPrint ast' = drawTree $ unfoldTree unfold' ast'
  where
    unfold' :: AST -> (String, [AST])
    unfold' (Apply fn args) = ("Apply", ast fn : map ast args)
    unfold' (If cond thenBranch elseBranch) = ("If", [cond, thenBranch, elseBranch])
    unfold' (Block exprs value) = ("Block", exprs ++ [value])
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

data ParserState
  = ParserState
  { tokens :: [(T.Token, T.Location)],
    consumed :: Int
  }
  deriving
    (Eq, Show)

type ASTStream = [(AST, T.Location)]

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

parseExpr :: Parser (AST, T.Location)
parseExpr = do
  nextToken <- peek
  case nextToken of
    Nothing -> throwMessage "unexpected end of input"
    _ -> do
      astnode <- parseExpr' 0
      return (ast astnode, loc astnode)
  where
    parseExpr' :: Int -> Parser ASTNode
    parseExpr' prec = do
      left <- mkASTNode <$> parseValue
      let loop left = do
            nextToken <- peek
            case nextToken of
              Just (T.Operator op, opLoc) ->
                if precedence op >= prec
                  then do
                    consume
                    right <- parseExpr' $ precedence op + 1
                    loop $ mkASTNode (Apply (mkASTNode (IdentifierAST op, opLoc)) [left, right], opLoc)
                  else return left
              _ -> return left
      loop left

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

integerLiteral = do
  (T.IntegerLiteral value, loc) <- satisfy isIntegerLiteral
  return (IntegerLiteral value, loc)
  where
    isIntegerLiteral token = case token of
      T.IntegerLiteral _ -> True
      _ -> False

identifier = do
  (T.Identifier id, loc) <- satisfy isIdentifier
  return (IdentifierAST id, loc)
  where
    isIdentifier token = case token of
      T.Identifier _ -> True
      _ -> False

boolean = do
  true <|> false
  where
    true = (BooleanLiteral True,) <$> satisfyIdentifier "true"
    false = (BooleanLiteral False,) <$> satisfyIdentifier "false"

unit = (Unit,) <$> satisfyIdentifier "unit"

unaryOperator = not <|> negate
  where
    not = do
      loc <- satisfyIdentifier "not"
      expr <- mkASTNode <$> parseExpr
      return (Apply (mkASTNode (IdentifierAST "not", loc)) [expr], loc)
    negate = do
      (_, loc) <- satisfyToken (T.Operator "-")
      expr <- mkASTNode <$> parseExpr
      return (Apply (mkASTNode (IdentifierAST "-", loc)) [expr], loc)

ifExpr = do
  loc <- satisfyIdentifier "if"
  (condExpr, _) <- parseExpr
  satisfyIdentifier "then"
  (thenExpr, _) <- parseExpr
  elseBranch <- elseBranch <|> return Unit
  return (If condExpr thenExpr elseBranch, loc)
  where
    elseBranch = do
      satisfyIdentifier "else"
      (elseExpr, _) <- parseExpr
      return elseExpr

while = do
  loc <- satisfyIdentifier "while"
  cond <- mkASTNode <$> parseExpr
  satisfyIdentifier "do"
  body <- mkASTNode <$> parseExpr
  return (Apply (mkASTNode (IdentifierAST "while", loc)) [cond, body], loc)

block = do
  (_, loc) <- satisfyToken (T.Punctuation "{")
  exprs <- many $ do
    expr <- variableDeclaration <|> parseExpr
    semicolon
    return expr
  value <- valueExpr <|> noValueExpr
  return (Block (fmap fst exprs) value, loc)
  where
    valueExpr = do
      (expr, _) <- variableDeclaration <|> parseExpr
      satisfyToken (T.Punctuation "}")
      return expr
    noValueExpr = do
      satisfyToken (T.Punctuation "}")
      return Unit

funCall = do
  id <- mkASTNode <$> identifier
  args <- argumentList
  return $ mkASTNode (Apply id args, loc id)
  where
    argumentList = do
      (_, loc) <- satisfyToken $ T.Punctuation "("
      let comma = satisfyToken $ T.Punctuation ","
      -- if the arg list fails, return Unit
      args <-
        map mkASTNode <$> delimited parseExpr comma <|> return [mkASTNode (Unit, loc)]
      satisfyToken $ T.Punctuation ")"
      return args

variableDeclaration = do
  loc <- satisfyIdentifier "var"
  id <- identifier
  satisfyToken (T.Operator "=")
  expr <- parseExpr
  return (VarDecl (mkASTNode id) (mkASTNode expr), loc)

parseValue :: Parser (AST, T.Location)
parseValue = do
  let astNodeToTuple astnode = (ast astnode, loc astnode)
  foldr (<|>) noMatch [astNodeToTuple <$> funCall, block, while, ifExpr, unaryOperator, integerLiteral, boolean, unit, identifier, old]
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

runParser :: forall a. Parser a -> [(T.Token, T.Location)] -> Either ParserError a
runParser (Parser parser) tokens = runIdentity $ evalStateT (runExceptT parser) (ParserState {tokens = tokens, consumed = 0})

parse :: [(T.Token, T.Location)] -> Either ParserError [(AST, T.Location)]
parse tokens = runParser (parseExpressionList isEmpty) tokens