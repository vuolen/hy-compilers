module TypeChecker where

import Control.Exception ()
import Parser (AST (BooleanLiteral, IdentifierAST, IntegerLiteral, VarDecl), ASTNode (ASTNode))
import Parser qualified as P (AST (Unit))

data Type = Int | Bool | Unit
  deriving (Show, Eq)

data TypeError = TypeError
  { message :: String,
    astNode :: ASTNode
  }
  deriving (Show, Eq)

data SymTab a = SymTab
  { parent :: Maybe (SymTab a),
    symbols :: [(String, a)]
  }
  deriving (Show, Eq)

getVar :: SymTab a -> String -> Maybe a
getVar symTab name = case lookup name (symbols symTab) of
  Just a -> Just a
  Nothing -> case parent symTab of
    Just parentSymTab -> getVar parentSymTab name
    Nothing -> Nothing

addVar :: SymTab a -> String -> a -> SymTab a
addVar symTab name value = symTab {symbols = (name, value) : symbols symTab}

typeCheck :: SymTab Type -> ASTNode -> Either TypeError (Type, SymTab Type)
typeCheck symTab (ASTNode (IntegerLiteral _) _) = Right (Int, symTab)
typeCheck symTab (ASTNode (BooleanLiteral _) _) = Right (Bool, symTab)
typeCheck symTab (ASTNode P.Unit _) = Right (Unit, symTab)
typeCheck symTab (ASTNode (VarDecl (ASTNode (IdentifierAST name) _) value) _) = do
  (valueType, _) <- typeCheck symTab value
  return (Unit, addVar symTab name valueType)
  where
    valueType = typeCheck symTab value
typeCheck _ _ = undefined