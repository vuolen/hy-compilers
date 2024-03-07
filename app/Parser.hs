module Parser where

import Control.Monad.Error.Class (withError)
import Control.Monad.Except (ExceptT, catchError, mapError, runExceptT, throwError, withExceptT)
import Control.Monad.Identity
import Control.Monad.Loops (untilM)
import Control.Monad.State.Lazy (StateT, evalStateT, get, put, runStateT)
import Data.Int (Int64)
import Data.List (uncons)
import Data.Maybe (isJust)
import Data.Tree (drawTree, unfoldTree)
import Debug.Trace (trace, traceM)
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

type TokenStream = ([(Token, Location)], Int)

type ASTStream = [(AST, Location)]

data ParserError = ParserError {message :: String, ast :: Maybe AST, topLevelASTs :: Maybe ASTStream}

addASTContext astContext =
  withError
    ( \err -> case ast err of
        Nothing -> err {ast = Just astContext}
        _ -> err
    )

instance Show ParserError where
  show (ParserError {message, ast, topLevelASTs}) =
    unlines
      [ "Parser Error",
        "Message: " ++ message,
        "AST: " ++ maybe "Nothing" prettyPrint ast
      ]

throwMessage :: forall a. String -> Parser a
throwMessage message = throwError $ ParserError message Nothing Nothing

type ParserT e a = StateT TokenStream (ExceptT e Identity) a

type Parser a = ParserT ParserError a

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
      return $ (applyArgs "while" [cond, body], loc)
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
          ast <- loop $ BlockAST [] UnitAST
          return (ast, loc)
      where
        loop (BlockAST exprs value) = do
          (expr, _) <- addASTContext (BlockAST exprs value) parseExpr
          isSemicolon <- consumeIf $ Punctuation ";"
          isBlockClose <- consumeIf $ Punctuation "}"

          -- terminating
          if isBlockClose && isSemicolon
            then return $ BlockAST (exprs ++ [expr]) UnitAST
            else
              if isBlockClose && not isSemicolon
                then return $ BlockAST exprs expr
                -- looping
                else
                  if isSemicolon
                    then loop $ BlockAST (exprs ++ [expr]) UnitAST
                    else throwMessage $ "Expected semicolon after expr " ++ show expr
    _ -> throwMessage $ "Parse error at " ++ show loc ++ " with token " ++ show token

parseTopLevelExpr :: Parser (AST, Location)
parseTopLevelExpr = do
  (ast, loc) <- parseExpr
  isSemicolon <- consumeIf $ Punctuation ";"
  stillEmpty <- isEmpty
  -- only accept no semicolon if we are at the end of the input
  if not isSemicolon && not stillEmpty
    then throwMessage $ "Missing semicolon after expression at " ++ show loc
    else return (ast, loc)

parseProgram :: Parser ASTStream
parseProgram = parseProgram' []
  where
    parseProgram' :: ASTStream -> Parser ASTStream
    parseProgram' asts = addErrorContext asts $ do
      expr <- parseTopLevelExpr
      empty <- isEmpty
      if empty
        then return $ asts ++ [expr]
        else parseProgram' (asts ++ [expr])
    addErrorContext asts =
      withError
        ( \err -> case topLevelASTs err of
            Nothing -> err {topLevelASTs = Just asts}
            _ -> err
        )

parse :: [(Token, Location)] -> Either ParserError [(AST, Location)]
parse tokens = runIdentity $ runExceptT $ evalStateT parseProgram (tokens, 0)