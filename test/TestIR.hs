module TestIR where

import IR (Instruction (..), generateIR)
import Parser (AST (..), ASTNode (..))
import SymTab (SymTab (..))
import Test.Tasty
import Test.Tasty.HUnit (assertEqual, assertFailure, testCase)
import Tokenizer (Location (..))
import TypeChecker (Type, baseSymTab)

irTests = testGroup "IR" [unitTests, equalityUnitTests]

equalityTestCases :: [(String, ASTNode, [Instruction])]
equalityTestCases =
  [ ( "Integer literal",
      ASTNode (IntegerLiteral 1) (Location 0 0),
      [LoadIntConst 1 "var0"]
    ),
    ( "Boolean literal",
      ASTNode (BooleanLiteral True) (Location 0 0),
      [LoadBoolConst True "var0"]
    )
  ]

equalityUnitTests =
  testGroup "equalityUnitTests" $
    map
      (\(name, ast, expected) -> testCase name $ assertEqual "" expected (map fst $ generateIR ast))
      equalityTestCases

unitTests =
  testGroup
    "Unit tests"
    []
