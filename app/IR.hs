module IR where

import Control.Applicative (Alternative (..))
import Control.Monad (foldM)
import Control.Monad.Accum (MonadAccum (add))
import Control.Monad.Except (MonadError (..))
import Control.Monad.State (MonadState (get), gets, put)
import Data.Bool (bool)
import Data.Int (Int64)
import EitherState (EitherState, runEitherState)
import Parser (AST (Apply, BooleanLiteral, IdentifierAST, IntegerLiteral), ASTNode (..), prettyPrint)
import Parser qualified as P
import SymTab (SymTab (..), insert, lookup)
import Tokenizer (Location, Token ())
import TypeChecker (Type (..), unitLiteral)

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
  { symTab :: SymTab IRVar,
    instructions :: [(Instruction, Location)],
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

getVar :: String -> IRGen IRVar
getVar name = do
  symTab <- gets symTab
  case SymTab.lookup name symTab of
    Just t -> return t
    Nothing -> throwIRError ("Variable " ++ name ++ " not found") Nothing

newVar :: IRGen IRVar
newVar = do
  state <- get
  let name = "var" ++ show (varCount state)
  let newState = insert name name (symTab state)
  put $ state {symTab = newState, varCount = varCount state + 1}
  return name

newLabel :: IRGen Label
newLabel = do
  state <- get
  let name = "label" ++ show (varCount state)
  put $ state {varCount = varCount state + 1}
  return name

addInstruction :: (Instruction, Location) -> IRGen ()
addInstruction instr = do
  state <- get
  put $ state {instructions = instructions state ++ [instr]}

integerLiteral :: ASTNode -> IRGen IRVar
integerLiteral ast = case ast of
  (ASTNode (IntegerLiteral val) loc) -> do
    var <- newVar
    addInstruction (LoadIntConst val var, loc)
    return var
  _ -> skipIRGen

booleanLiteral :: ASTNode -> IRGen IRVar
booleanLiteral ast = case ast of
  (ASTNode (BooleanLiteral val) loc) -> do
    var <- newVar
    addInstruction (LoadBoolConst val var, loc)
    return var
  _ -> skipIRGen

unitLiteral ast = case ast of
  (ASTNode P.Unit loc) -> do
    return "unit"
  _ -> skipIRGen

identifier ast = case ast of
  (ASTNode (IdentifierAST name) loc) -> do
    getVar name
  _ -> skipIRGen

apply ast = case ast of
  (ASTNode (Apply (ASTNode (IdentifierAST name) _) args) loc) -> do
    st <- gets symTab
    opVar <- getVar name
    argVars <- mapM irGenerator args
    resultVar <- newVar
    addInstruction (Call opVar argVars resultVar, loc)
    return resultVar
  _ -> skipIRGen

ifElseThen ast = case ast of
  (ASTNode (Apply (ASTNode (IdentifierAST "if") _) [cond, thenBranch, elseBranch]) loc) -> do
    case elseBranch of
      (ASTNode P.Unit _) -> do
        thenLabel <- newVar
        endLabel <- newVar
        condVar <- irGenerator cond
        addInstruction (CondJump condVar thenLabel endLabel, loc)
        addInstruction (Label thenLabel, loc)
        thenVar <- irGenerator thenBranch
        addInstruction (Label endLabel, loc)
        return "unit"
      _ -> do
        thenLabel <- newVar
        elseLabel <- newVar
        endLabel <- newVar
        condVar <- irGenerator cond
        addInstruction (CondJump condVar thenLabel elseLabel, loc)
        addInstruction (Label thenLabel, loc)
        thenVar <- irGenerator thenBranch
        addInstruction (Jump endLabel, loc)
        addInstruction (Label elseLabel, loc)
        elseVar <- irGenerator elseBranch
        addInstruction (Label endLabel, loc)
        return "unit"
  _ -> skipIRGen

-- \| If ASTNode ASTNode ASTNode
-- \| Block [ASTNode] ASTNode
-- \| VarDecl ASTNode ASTNode
-- \| TypedVarDecl ASTNode ASTNode ASTNode

irGenerator :: ASTNode -> IRGen IRVar
irGenerator ast = do
  foldr (<|>) noMatch generators
  where
    noMatch = throwIRError "No IR generator matched ast" (Just ast)
    generators =
      map
        (\generator -> generator ast)
        [integerLiteral, booleanLiteral, identifier, apply, ifElseThen]

generateIR :: SymTab a -> ASTNode -> [(Instruction, Location)]
generateIR symTab astNode = case result of
  Right t -> instructions state
  Left e -> error $ show e
  where
    (result, state) = runEitherState (irGenerator astNode) (IRGenState baseSymTab [] 0)
    baseSymTab :: SymTab IRVar
    baseSymTab = SymTab {parent = Nothing, symbols = map (\(name, _) -> (name, name)) (symbols symTab)}

generateIRASTStream :: SymTab a -> [ASTNode] -> [(Instruction, Location)]
generateIRASTStream symTab astStream = do
  let (result, state) =
        runEitherState
          ( foldM
              ( \instructions ast -> do
                  instr <- irGenerator ast
                  return $ instructions ++ instr
              )
              []
              astStream
          )
          (IRGenState baseSymTab [] 0)
  case result of
    Right t -> instructions state
    Left e -> error $ show e
  where
    baseSymTab :: SymTab IRVar
    baseSymTab = SymTab {parent = Nothing, symbols = map (\(name, _) -> (name, name)) (symbols symTab)}