module TypeChecker where

import Control.Applicative (Alternative (..))
import Control.Exception ()
import Control.Monad.Except (MonadError (catchError), throwError)
import Control.Monad.State (MonadState (put), get, modify)
import Data.Map (argSet)
import EitherState (EitherState, runEitherState)
import GHC.Generics (Constructor (conName))
import GHC.Read (paren)
import Parser (AST (Apply, BooleanLiteral, IdentifierAST, IntegerLiteral, VarDecl), ASTNode (ASTNode), prettyPrint)
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
          ("or", Fun [Bool, Bool] Bool)
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
addVar name value = modify $ \symTab -> symTab {symbols = (name, value) : symbols symTab}

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
    addVar name Int
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
      then return Unit
      else throwTypeError ("Type mismatch in variable assignment: " ++ name) (Just ast)
  _ -> skipParser

typeChecker :: ASTNode -> TypeChecker Type
typeChecker ast = foldr (<|>) noMatch typeCheckers
  where
    noMatch = throwTypeError "No type could be determined for" (Just ast)
    typeCheckers = map (\checker -> checker ast) [integerLiteral, booleanLiteral, unitLiteral, varDecl, varAssignment, unaryNegation, apply]

typeCheck :: SymTab Type -> ASTNode -> Either TypeError (Type, SymTab Type)
typeCheck symTab astNode = case result of
  Right t -> Right (t, state)
  Left e -> Left e
  where
    (result, state) = runEitherState (typeChecker astNode) symTab