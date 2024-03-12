module TestTypeChecker where

import Control.Applicative (Alternative (empty))
import Parser (AST (BooleanLiteral, IdentifierAST, IntegerLiteral, VarDecl), ASTNode (..))
import Parser qualified as P (AST (Unit))
import Test.Tasty (testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)
import Tokenizer qualified as T
import TypeChecker (SymTab (..), Type (..), typeCheck)

typeCheckerTests = testGroup "typeChecker" [equalityUnitTests]

emptySymTab = SymTab {parent = Nothing, symbols = []}

equalityTestCases =
  [ ("Integer literal", emptySymTab, ASTNode (IntegerLiteral 1) (T.Location 0 0), (Int, emptySymTab)),
    ("Boolean literal", emptySymTab, ASTNode (BooleanLiteral True) (T.Location 0 0), (Bool, emptySymTab)),
    ("Unit literal", emptySymTab, ASTNode P.Unit (T.Location 0 0), (Unit, emptySymTab)),
    ( "Variable declaration",
      emptySymTab,
      ASTNode
        ( VarDecl
            (ASTNode (IdentifierAST "x") (T.Location 0 0))
            (ASTNode (IntegerLiteral 1) (T.Location 0 0))
        )
        (T.Location 0 0),
      (Unit, SymTab {parent = Nothing, symbols = [("x", Int)]})
    )
  ]

equalityUnitTests =
  testGroup
    "Equality unit tests"
    $ map
      ( \(name, symtab, input, expected) -> testCase name $ do
          let result = typeCheck symtab input
          assertEqual "" (Right expected) result
      )
      equalityTestCases