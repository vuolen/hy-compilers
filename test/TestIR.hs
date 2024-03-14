module TestIR where

import IR (Instruction (..), generateIR)
import Parser (AST (..), ASTNode (..))
import SymTab (SymTab (..))
import Test.Tasty
import Test.Tasty.HUnit (assertEqual, assertFailure, testCase)
import Tokenizer (Location (..))
import TypeChecker (Type, baseSymTab)

irTests = testGroup "IR" [unitTests, equalityUnitTests]

emptySymTab = SymTab {parent = Just baseSymTab, symbols = []}

equalityTestCases :: [(String, SymTab Type, ASTNode, [Instruction])]
equalityTestCases =
  [ ( "Integer literal",
      emptySymTab,
      ASTNode (IntegerLiteral 1) (Location 0 0),
      [LoadIntConst 1 "var0"]
    ),
    ( "Boolean literal",
      emptySymTab,
      ASTNode (BooleanLiteral True) (Location 0 0),
      [LoadBoolConst True "var0"]
    )
  ]

equalityUnitTests =
  testGroup "equalityUnitTests" $
    map
      (\(name, symTab, ast, expected) -> testCase name $ assertEqual "" expected (map fst $ generateIR symTab ast))
      equalityTestCases

unitTests =
  testGroup
    "Unit tests"
    []
