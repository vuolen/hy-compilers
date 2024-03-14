module TypeChecker where

import Control.Applicative (Alternative (..))
import Control.Exception ()
import Control.Monad.Except (MonadError (catchError), throwError)
import Control.Monad.State (MonadState (put), get, modify)
import Data.Map (argSet)
import Debug.Trace (trace, traceM)
import EitherState (EitherState, runEitherState)
import GHC.Generics (Constructor (conName))
import GHC.Read (paren)
import Parser (AST (Apply, BooleanLiteral, IdentifierAST, IntegerLiteral, TypedVarDecl, VarDecl), ASTNode (ASTNode), prettyPrint)
import Parser qualified as P (AST (Unit))
import Tokenizer (Location (Location))

data Type = Int | Bool | Unit | Fun [Type] Type
  deriving (Show, Eq)

data TypeError
  = TypeError
      { message :: String,
        astNode :: Maybe ASTNode
      }
  | Skip
  deriving (Eq)

instance Show TypeError where
  show (TypeError {message, astNode}) = "TypeError \n message: " ++ message ++ " \n ast: " ++ maybe "Nothing" prettyPrint astNode
  show Skip = "Skip"

data SymTab a = SymTab
  { parent :: Maybe (SymTab a),
    symbols :: [(String, a)]
  }
  deriving (Show, Eq)

baseSymTab :: SymTab Type
baseSymTab =
  SymTab
    { parent = Nothing,
      symbols =
        [ ("+", Fun [Int, Int] Int),
          ("-", Fun [Int, Int] Int),
          ("*", Fun [Int, Int] Int),
          ("/", Fun [Int, Int] Int),
          ("%", Fun [Int, Int] Int),
          ("not", Fun [Bool] Bool),
          ("<", Fun [Bool, Bool] Bool),
          ("<=", Fun [Bool, Bool] Bool),
          (">", Fun [Bool, Bool] Bool),
          (">=", Fun [Bool, Bool] Bool),
          ("and", Fun [Bool, Bool] Bool),
          ("or", Fun [Bool, Bool] Bool),
          ("print_int", Fun [Int] Unit),
          ("print_bool", Fun [Bool] Unit),
          ("read_int", Fun [] Int)
        ]
    }

type TypeChecker a = EitherState TypeError (SymTab Type) a

-- Propagate TypeErrors, try new parser if the error is Skip
instance Alternative (EitherState TypeError (SymTab Type)) where
  empty = throwTypeError "Empty typechecker" Nothing
  p1 <|> p2 = do
    oldState <- get
    p1
      `catchError` ( \err ->
                       case err of
                         Skip -> do
                           put oldState
                           p2
                         _ -> throwError err
                   )

throwTypeError :: String -> Maybe ASTNode -> TypeChecker a
throwTypeError message astNode = throwError $ TypeError message astNode

skipParser :: TypeChecker a
skipParser = throwError Skip

getVar :: String -> TypeChecker Type
getVar name = do
  symTab <- get
  case lookup name (symbols symTab) of
    Just t -> return t
    Nothing -> do
      case parent symTab of
        Just parentSymTab -> do
          put parentSymTab
          t <- getVar name
          put symTab
          return t
        Nothing -> throwTypeError ("Variable " ++ name ++ " not found") Nothing

addVar :: String -> Type -> TypeChecker ()
addVar name value = modify $ \symTab -> symTab {symbols = newSymbols name value (symbols symTab)}
  where
    newSymbols :: String -> Type -> [(String, Type)] -> [(String, Type)]
    newSymbols name value [] = [(name, value)]
    newSymbols name value ((n, v) : xs) =
      if name == n
        then (name, value) : xs
        else (n, v) : newSymbols name value xs

declareVariable :: String -> Type -> TypeChecker ()
declareVariable name value = do
  symTab <- get
  let localVariables = map fst (symbols symTab)
  if name `elem` localVariables
    then throwTypeError ("Variable " ++ name ++ " already declared") Nothing
    else do
      addVar name value

integerLiteral :: ASTNode -> TypeChecker Type
integerLiteral ast = case ast of
  ASTNode (IntegerLiteral _) _ -> return Int
  _ -> skipParser

booleanLiteral :: ASTNode -> TypeChecker Type
booleanLiteral ast = case ast of
  ASTNode (BooleanLiteral _) _ -> return Bool
  _ -> skipParser

unitLiteral :: ASTNode -> TypeChecker Type
unitLiteral ast = case ast of
  ASTNode P.Unit _ -> return Unit
  _ -> skipParser

varDecl :: ASTNode -> TypeChecker Type
varDecl ast = case ast of
  ASTNode (VarDecl (ASTNode (IdentifierAST name) _) value) _ -> do
    valueType <- typeChecker value
    declareVariable name valueType
    return Unit
  _ -> skipParser

typedVarDecl :: ASTNode -> TypeChecker Type
typedVarDecl ast = case ast of
  ASTNode (TypedVarDecl (ASTNode (IdentifierAST name) _) (ASTNode (IdentifierAST typeName) _) value) _ -> do
    valueType <- typeChecker value
    expectedType <- case typeName of
      "Int" -> return Int
      "Bool" -> return Bool
      "Unit" -> return Unit
      _ -> throwTypeError ("Invalid type in variable declaration " ++ typeName) (Just ast)
    if valueType /= expectedType
      then throwTypeError ("Declaring variable " ++ name ++ " expected a value of type " ++ show expectedType ++ " but got " ++ show valueType) (Just ast)
      else do
        declareVariable name valueType
        return Unit
  _ -> skipParser

unaryNegation :: ASTNode -> TypeChecker Type
unaryNegation ast = case ast of
  ASTNode (Apply (ASTNode (IdentifierAST "-") _) [arg]) _ -> do
    argType <- typeChecker arg
    if argType == Int
      then return Int
      else throwTypeError "Expected Int argument in negation" (Just ast)
  _ -> skipParser

apply :: ASTNode -> TypeChecker Type
apply ast = case ast of
  ASTNode (Apply (ASTNode (IdentifierAST name) _) args) _ -> do
    funType <- getVar name
    case funType of
      Fun argList returnType -> do
        argTypes <- mapM typeChecker args
        if argTypes == argList
          then return returnType
          else
            throwTypeError
              ( "Type mismatch in function "
                  ++ name
                  ++ " arguments. Expected "
                  ++ show argList
                  ++ " but got "
                  ++ show argTypes
              )
              (Just ast)
      _ -> throwTypeError ("Expected " ++ name ++ " to be a function but it is " ++ show funType) (Just ast)
  _ -> skipParser

varAssignment :: ASTNode -> TypeChecker Type
varAssignment ast = case ast of
  ASTNode (Apply (ASTNode (IdentifierAST "=") _) [ASTNode (IdentifierAST name) _, value]) _ -> do
    valueType <- typeChecker value
    varType <- getVar name
    if valueType == varType
      then do
        addVar name valueType
        return Unit
      else
        throwTypeError
          ( "Cannot assign a value of type "
              ++ show valueType
              ++ " to variable "
              ++ name
              ++ " of type "
              ++ show varType
          )
          (Just ast)
  _ -> skipParser

equalities :: ASTNode -> TypeChecker Type
equalities ast = case ast of
  ASTNode (Apply (ASTNode (IdentifierAST op) _) [arg1, arg2]) _ -> do
    if op /= "==" && op /= "!="
      then skipParser
      else do
        arg1Type <- typeChecker arg1
        arg2Type <- typeChecker arg2
        if arg1Type == arg2Type
          then return Bool
          else
            throwTypeError
              ( "Cannot compare equality between values of type "
                  ++ show arg1Type
                  ++ " and "
                  ++ show arg2Type
              )
              (Just ast)
  _ -> skipParser

typeChecker :: ASTNode -> TypeChecker Type
typeChecker ast = foldr (<|>) noMatch typeCheckers
  where
    noMatch = throwTypeError "No type could be determined for" (Just ast)
    typeCheckers = map (\checker -> checker ast) [integerLiteral, booleanLiteral, unitLiteral, varDecl, typedVarDecl, varAssignment, unaryNegation, equalities, apply]

typeCheck :: SymTab Type -> ASTNode -> Either TypeError (Type, SymTab Type)
typeCheck symTab astNode = case result of
  Right t -> Right (t, state)
  Left e -> Left e
  where
    (result, state) = runEitherState (typeChecker astNode) symTab