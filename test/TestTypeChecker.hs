module TestTypeChecker (typeCheckerTests) where

import Control.Applicative (Alternative (empty))
import Data.Either (isLeft)
import Data.List.NonEmpty (fromList)
import Parser (AST (Apply, Block, BooleanLiteral, IdentifierAST, If, IntegerLiteral, TypedVarDecl, VarDecl), ASTNode (..), parse)
import Parser qualified as P (AST (Unit))
import SymTab (SymTab (..))
import Test.Falsify.Generator qualified as Gen
import Test.Falsify.Predicate as P
import Test.Tasty (testGroup)
import Test.Tasty.Falsify
import Test.Tasty.HUnit (assertEqual, assertFailure, testCase)
import Tokenizer qualified as T
import TypeChecker (Type (..), TypeError (..), baseSymTab, message, typeCheck)

typeCheckerTests = testGroup "typeChecker" [propertyTests, equalityUnitTests, failureUnitTests]

propertyTests =
  testGroup
    "Property tests"
    []

emptySymTab = SymTab {parent = Just baseSymTab, symbols = []}

-- x = 1
xIsOneAST =
  ASTNode
    ( Apply
        (ASTNode (IdentifierAST "=") (T.Location 0 0))
        [ ASTNode (IdentifierAST "x") (T.Location 0 0),
          ASTNode (IntegerLiteral 1) (T.Location 0 0)
        ]
    )
    (T.Location 0 0)

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
      (Unit, SymTab {parent = Just baseSymTab, symbols = [("x", Int)]})
    ),
    ( "Variable assignment",
      SymTab {parent = Nothing, symbols = [("x", Int)]},
      xIsOneAST,
      (Unit, SymTab {parent = Nothing, symbols = [("x", Int)]})
    ),
    ( "Unary negation",
      emptySymTab,
      ASTNode
        ( Apply
            (ASTNode (IdentifierAST "-") (T.Location 0 0))
            [ASTNode (IntegerLiteral 1) (T.Location 0 0)]
        )
        (T.Location 0 0),
      (Int, emptySymTab)
    ),
    ( "Unary not",
      emptySymTab,
      ASTNode
        ( Apply
            (ASTNode (IdentifierAST "not") (T.Location 0 0))
            [ASTNode (BooleanLiteral True) (T.Location 0 0)]
        )
        (T.Location 0 0),
      (Bool, emptySymTab)
    ),
    ( "Equality",
      emptySymTab,
      ASTNode
        ( Apply
            (ASTNode (IdentifierAST "==") (T.Location 0 0))
            [ASTNode (IntegerLiteral 1) (T.Location 0 0), ASTNode (IntegerLiteral 1) (T.Location 0 0)]
        )
        (T.Location 0 0),
      (Bool, emptySymTab)
    ),
    ( "Inequality",
      emptySymTab,
      ASTNode
        ( Apply
            (ASTNode (IdentifierAST "!=") (T.Location 0 0))
            [ASTNode (IntegerLiteral 1) (T.Location 0 0), ASTNode (IntegerLiteral 1) (T.Location 0 0)]
        )
        (T.Location 0 0),
      (Bool, emptySymTab)
    ),
    ( "print_int",
      emptySymTab,
      ASTNode
        ( Apply
            (ASTNode (IdentifierAST "print_int") (T.Location 0 0))
            [ASTNode (IntegerLiteral 1) (T.Location 0 0)]
        )
        (T.Location 0 0),
      (Unit, emptySymTab)
    ),
    ( "print_bool",
      emptySymTab,
      ASTNode
        ( Apply
            (ASTNode (IdentifierAST "print_bool") (T.Location 0 0))
            [ASTNode (BooleanLiteral True) (T.Location 0 0)]
        )
        (T.Location 0 0),
      (Unit, emptySymTab)
    ),
    ( "read_int",
      emptySymTab,
      ASTNode
        ( Apply
            (ASTNode (IdentifierAST "read_int") (T.Location 0 0))
            []
        )
        (T.Location 0 0),
      (Int, emptySymTab)
    ),
    ( "Typed variable declaration",
      emptySymTab,
      ASTNode
        ( TypedVarDecl
            (ASTNode (IdentifierAST "x") (T.Location 0 0))
            (ASTNode (IdentifierAST "Int") (T.Location 0 0))
            (ASTNode (IntegerLiteral 1) (T.Location 0 0))
        )
        (T.Location 0 0),
      (Unit, SymTab {parent = Just baseSymTab, symbols = [("x", Int)]})
    ),
    ( "Block without result expression",
      emptySymTab,
      ASTNode
        ( Block
            [ ASTNode (IntegerLiteral 1) (T.Location 0 2),
              ASTNode (IntegerLiteral 2) (T.Location 0 5),
              ASTNode (IntegerLiteral 3) (T.Location 0 8)
            ]
            (ASTNode P.Unit T.NoLocation)
        )
        (T.Location 0 0),
      (Unit, emptySymTab)
    ),
    ( "Block with result expression",
      emptySymTab,
      ASTNode
        ( Block
            [ ASTNode (IntegerLiteral 1) (T.Location 0 2),
              ASTNode (IntegerLiteral 2) (T.Location 0 5)
            ]
            (ASTNode (IntegerLiteral 3) (T.Location 0 8))
        )
        (T.Location 0 0),
      (Int, emptySymTab)
    ),
    ( "if then expression",
      emptySymTab,
      ASTNode
        ( If
            (ASTNode (BooleanLiteral True) (T.Location 0 0))
            (ASTNode (IntegerLiteral 1) (T.Location 0 0))
            (ASTNode P.Unit T.NoLocation)
        )
        (T.Location 0 0),
      (Unit, emptySymTab)
    ),
    ( "if then else expression",
      emptySymTab,
      ASTNode
        ( If
            (ASTNode (BooleanLiteral True) (T.Location 0 0))
            (ASTNode (IntegerLiteral 1) (T.Location 0 0))
            (ASTNode (IntegerLiteral 1) (T.Location 0 0))
        )
        (T.Location 0 0),
      (Int, emptySymTab)
    ),
    ( "while loop",
      emptySymTab,
      ASTNode
        ( Apply
            (ASTNode (IdentifierAST "while") (T.Location 0 0))
            [ ASTNode (BooleanLiteral True) (T.Location 0 0),
              ASTNode (IntegerLiteral 1) (T.Location 0 0)
            ]
        )
        (T.Location 0 0),
      (Unit, emptySymTab)
    ),
    ( "variables",
      SymTab {parent = Nothing, symbols = [("x", Int)]},
      ASTNode
        (IdentifierAST "x")
        (T.Location 0 0),
      (Int, SymTab {parent = Nothing, symbols = [("x", Int)]})
    )
  ]
    ++ map
      ( \op ->
          ( "1 " ++ op ++ " 1" ++ " returns Int",
            emptySymTab,
            ASTNode
              ( Apply
                  (ASTNode (IdentifierAST op) (T.Location 0 0))
                  [ASTNode (IntegerLiteral 1) (T.Location 0 0), ASTNode (IntegerLiteral 1) (T.Location 0 0)]
              )
              (T.Location 0 0),
            (Int, emptySymTab)
          )
      )
      ["+", "-", "*", "/", "%"]
    ++ map
      ( \op ->
          ( "True " ++ op ++ " True" ++ " returns Bool",
            emptySymTab,
            ASTNode
              ( Apply
                  (ASTNode (IdentifierAST op) (T.Location 0 0))
                  [ASTNode (BooleanLiteral True) (T.Location 0 0), ASTNode (BooleanLiteral True) (T.Location 0 0)]
              )
              (T.Location 0 0),
            (Bool, emptySymTab)
          )
      )
      ["<", "<=", ">", ">=", "and", "or"]

failureTestCases =
  [ ( "Variable assignment mismatch",
      SymTab {parent = Nothing, symbols = [("x", Bool)]},
      xIsOneAST
    ),
    ( "Argument number mismatch",
      emptySymTab,
      ASTNode
        ( Apply
            (ASTNode (IdentifierAST "*") (T.Location 0 0))
            [ASTNode (IntegerLiteral 1) (T.Location 0 0)]
        )
        (T.Location 0 0)
    ),
    ( "Equality type mismatch",
      emptySymTab,
      ASTNode
        ( Apply
            (ASTNode (IdentifierAST "==") (T.Location 0 0))
            [ASTNode (IntegerLiteral 1) (T.Location 0 0), ASTNode (BooleanLiteral True) (T.Location 0 0)]
        )
        (T.Location 0 0)
    ),
    ( "Inequality type mismatch",
      emptySymTab,
      ASTNode
        ( Apply
            (ASTNode (IdentifierAST "!=") (T.Location 0 0))
            [ASTNode (IntegerLiteral 1) (T.Location 0 0), ASTNode (BooleanLiteral True) (T.Location 0 0)]
        )
        (T.Location 0 0)
    ),
    ( "Redeclaring variable",
      SymTab {parent = Nothing, symbols = [("x", Int)]},
      ASTNode
        ( VarDecl
            (ASTNode (IdentifierAST "x") (T.Location 0 0))
            (ASTNode (IntegerLiteral 1) (T.Location 0 0))
        )
        (T.Location 0 0)
    ),
    ( "If then else mismatch",
      emptySymTab,
      ASTNode
        ( If
            (ASTNode (BooleanLiteral True) (T.Location 0 0))
            (ASTNode (BooleanLiteral False) (T.Location 0 0))
            (ASTNode (IntegerLiteral 1) (T.Location 0 0))
        )
        (T.Location 0 0)
    )
  ]

equalityUnitTests =
  testGroup
    "Equality unit tests"
    $ map
      ( \(name, symtab, input, expected) -> testCase name $ do
          let result = typeCheck symtab input
          case result of
            Right t -> assertEqual "" expected t
            Left err -> assertFailure $ show err
      )
      equalityTestCases

failureUnitTests =
  testGroup
    "Expect failure unit tests"
    $ map
      ( \(name, symtab, input) -> testCase name $ do
          let result = typeCheck symtab input
          assertEqual "" True (isLeft result)
      )
      failureTestCases