module IR where

import Control.Applicative (Alternative (..))
import Control.Monad (foldM)
import Control.Monad.Accum (MonadAccum (add))
import Control.Monad.Except (MonadError (..))
import Control.Monad.State (MonadState (get), gets, modify, put)
import Data.Bool (bool)
import Data.Int (Int64)
import Debug.Trace (traceM)
import EitherState (EitherState, runEitherState)
import Parser (AST (Apply, Block, BooleanLiteral, IdentifierAST, If, IntegerLiteral, TypedVarDecl, VarDecl), ASTNode (..), prettyPrint)
import Parser qualified as P
import SymTab (SymTab (..), insert, lookup, newScope, undoScope)
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
  deriving (Eq)

instance Show Instruction where
  show (LoadBoolConst val var) = "LoadBoolConst(" ++ show val ++ ", " ++ var ++ ")"
  show (LoadIntConst val var) = "LoadIntConst(" ++ show val ++ ", " ++ var ++ ")"
  show (Copy src dest) = "Copy(" ++ src ++ ", " ++ dest ++ ")"
  show (Call op args result) = "Call(" ++ op ++ ", " ++ show args ++ ", " ++ result ++ ")"
  show (Jump label) = "Jump(" ++ label ++ ")"
  show (CondJump cond thenLabel elseLabel) = "CondJump(" ++ cond ++ ", " ++ thenLabel ++ ", " ++ elseLabel ++ ")"
  show (Label label) = "Label(" ++ label ++ ")"

baseSymTab :: SymTab IRVar
baseSymTab = SymTab {parent = Nothing, symbols = map (\name -> (name, name)) symbols}
  where
    symbols = ["+", "-", "*", "/", "%", "not", "<", "<=", ">", ">=", "and", "or", "print_int", "print_bool", "read_int"]

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
  (ASTNode (If cond thenBranch elseBranch) loc) -> do
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

block ast = case ast of
  (ASTNode (Block exprs value) loc) -> do
    st <- gets symTab
    modify $ \state -> state {symTab = newScope (symTab state)}
    mapM_ irGenerator exprs
    valueVar <- irGenerator value
    modify $ \state -> state {symTab = undoScope (symTab state)}
    return valueVar
  _ -> skipIRGen

varDecl ast = case ast of
  (ASTNode (VarDecl (ASTNode (IdentifierAST name) _) value) loc) -> do
    var <- newVar
    valueVar <- irGenerator value
    addInstruction (Copy valueVar var, loc)
    modify $ \state -> state {symTab = insert name var (symTab state)}
    return var
  _ -> skipIRGen

typedVarDecl ast = case ast of
  (ASTNode (TypedVarDecl name t value) loc) -> do
    var <- newVar
    valueVar <- irGenerator value
    addInstruction (Copy valueVar var, loc)
    return var
  _ -> skipIRGen

while ast = case ast of
  (ASTNode (Apply (ASTNode (IdentifierAST "while") _) [cond, body]) loc) -> do
    startLabel <- newLabel
    endLabel <- newLabel
    addInstruction (Label startLabel, loc)
    condVar <- irGenerator cond
    addInstruction (CondJump condVar startLabel endLabel, loc)
    irGenerator body
    addInstruction (Jump startLabel, loc)
    addInstruction (Label endLabel, loc)
    return "unit"
  _ -> skipIRGen

varAssignment :: ASTNode -> EitherState IRError IRGenState IRVar
varAssignment ast = case ast of
  (ASTNode (Apply (ASTNode (IdentifierAST "=") _) [ASTNode (IdentifierAST varName) _, value]) loc) -> do
    var <- getVar varName
    valueVar <- irGenerator value
    addInstruction (Copy valueVar var, loc)
    return var
  _ -> skipIRGen

irGenerator :: ASTNode -> IRGen IRVar
irGenerator ast = do
  foldr (<|>) noMatch generators
  where
    noMatch = throwIRError "No IR generator matched ast" (Just ast)
    generators =
      map
        (\generator -> generator ast)
        [integerLiteral, booleanLiteral, unitLiteral, identifier, while, varAssignment, apply, ifElseThen, block, varDecl, typedVarDecl]

generateIR :: ASTNode -> [(Instruction, Location)]
generateIR astNode = case result of
  Right t -> instructions state
  Left e -> error $ show e
  where
    (result, state) = runEitherState (irGenerator astNode) (IRGenState baseSymTab [] 0)

generateIRASTStream :: [ASTNode] -> [(Instruction, Location)]
generateIRASTStream astStream = do
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