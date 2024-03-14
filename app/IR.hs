module IR where

import Control.Applicative (Alternative (..))
import Control.Monad.Except (MonadError (..))
import Control.Monad.State (MonadState (get), put)
import Data.Bool (bool)
import Data.Int (Int64)
import EitherState (EitherState, runEitherState)
import Parser (AST (..), ASTNode (..), prettyPrint)
import SymTab (SymTab, insert)
import Tokenizer (Location, Token ())
import TypeChecker (Type (..))

type IRVar = String

type Label = String

data Instruction
  = LoadBoolConst Bool IRVar
  | LoadIntConst Int64 IRVar
  | Copy IRVar IRVar
  | Call IRVar [IRVar] IRVar
  | Jump Label
  | CondJump IRVar Label Label
  | Label Label
  deriving (Eq, Show)

data IRGenState
  = IRGenState
  { symTab :: SymTab Type,
    varCount :: Int
  }

type IRGen a = EitherState IRError IRGenState a

skipIRGen :: IRGen a
skipIRGen = throwError Skip

data IRError
  = IRError
      { message :: String,
        astNode :: Maybe ASTNode
      }
  | Skip
  deriving (Eq)

throwIRError message astNode = throwError $ IRError message astNode

-- Propagate IRErrors, try new generator if the error is Skip
instance Alternative (EitherState IRError IRGenState) where
  empty = throwIRError "Empty IRGen" Nothing
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

instance Show IRError where
  show (IRError {message, astNode}) = "IRError \n message: " ++ message ++ " \n ast: " ++ maybe "Nothing" prettyPrint astNode
  show Skip = "Skip"

newVar :: Type -> IRGen IRVar
newVar t = do
  state <- get
  let name = "var" ++ show (varCount state)
  let newState = insert name t (symTab state)
  put $ state {symTab = newState, varCount = varCount state + 1}
  return name

integerLiteral ast = case ast of
  (ASTNode (IntegerLiteral val) loc) -> do
    var <- newVar Int
    return [(LoadIntConst 1 var, loc)]
  _ -> skipIRGen

booleanLiteral ast = case ast of
  (ASTNode (BooleanLiteral val) loc) -> do
    var <- newVar Bool
    return [(LoadBoolConst val var, loc)]
  _ -> skipIRGen

irGenerator :: ASTNode -> IRGen [(Instruction, Location)]
irGenerator ast = foldr (<|>) noMatch typeCheckers
  where
    noMatch = throwIRError "No IR generator matched ast" (Just ast)
    typeCheckers =
      map
        (\checker -> checker ast)
        [integerLiteral, booleanLiteral]

generateIR :: SymTab Type -> ASTNode -> [(Instruction, Location)]
generateIR symTab astNode = case result of
  Right t -> t
  Left e -> error $ show e
  where
    (result, state) = runEitherState (irGenerator astNode) (IRGenState symTab 0)