module TestParser where

import Data.Bool
import Data.Either (isLeft, isRight)
import Data.List.NonEmpty (fromList)
import Debug.Trace (traceM)
import Parser (AST (..), ASTNode (..), ParserError (..), parse)
import Test.Falsify.Generator qualified as Gen
import Test.Falsify.Predicate as P
import Test.Falsify.Range qualified as Range
import Test.Tasty
import Test.Tasty.Falsify
import Test.Tasty.HUnit (assertEqual, assertFailure, testCase)
import Tokenizer qualified as T (Location (..), Token (..), tokenize)

parserTests = testGroup "Parser" [propertyTests, unitTests, equalityUnitTests]

propertyTests =
  testGroup
    "Property tests"
    [ testProperty "Positive integer literal" $
        do
          value <- gen $ Gen.inRange $ Range.between (0, maxBound)
          let literal = show value
          let tokens = T.tokenize literal
          result <- case parse tokens of
            Left (ParserError {message}) -> testFailed message
            Right result -> return result
          assert $ P.expect 1 .$ ("number of tokens", length tokens)
          assert $ P.expect (IntegerLiteral value) .$ ("ast", ast $ head result),
      testProperty "Negative integer literal" $
        do
          value <- gen $ Gen.inRange $ Range.between (0, maxBound)
          let literal = show value
          let tokens = T.tokenize $ "-" ++ literal
          result <- case parse tokens of
            Left (ParserError {message}) -> testFailed message
            Right result -> return result
          let expected =
                [ ASTNode
                    ( Apply
                        (ASTNode (IdentifierAST "-") (T.Location 0 0))
                        [ASTNode (IntegerLiteral value) (T.Location 0 1)]
                    )
                    (T.Location 0 0)
                ]
          assert $ P.expect expected .$ ("ast", result),
      testProperty
        "Binary operations supported"
        $ do
          op <- gen $ Gen.elem $ fromList ["+", "-", "*", "/", "%", "==", "!=", "<", "<=", ">", ">=", "and", "or"]
          let tokens = T.tokenize $ "1 " ++ op ++ " 2"
          let expected =
                [ ASTNode
                    ( Apply
                        (ASTNode (IdentifierAST op) (T.Location 0 2))
                        [ ASTNode (IntegerLiteral 1) (T.Location 0 0),
                          ASTNode (IntegerLiteral 2) (T.Location 0 (length op + 3))
                        ]
                    )
                    (T.Location 0 2)
                ]
          ast <- case parse tokens of
            Left (ParserError {message}) -> testFailed message
            Right result -> return result
          assert $ P.expect expected .$ ("ast", ast),
      testProperty
        "Binary operation expressions do not fail"
        $ do
          let genIdentifier = Gen.elem $ fromList ["a", "b", "c"]
          let genExpression depth = do
                op <- Gen.elem $ fromList ["+", "-", "*", "/", "%", "==", "!=", "<", "<=", ">", ">=", "and", "or", "="]
                let subExpressions = if depth < 10 then [genIdentifier, genExpression (depth + 1)] else [genIdentifier]
                left <- Gen.oneof $ fromList subExpressions
                right <- Gen.oneof $ fromList subExpressions

                return $ left ++ " " ++ op ++ " " ++ right

          expr <- gen $ genExpression 0

          let tokens = T.tokenize expr
          case parse tokens of
            Left (ParserError {message}) -> testFailed message
            Right _ -> return ()
    ]

parseSuccess :: [(T.Token, T.Location)] -> IO [ASTNode]
parseSuccess tokens = do
  case parse tokens of
    Right ast -> return ast
    Left err -> assertFailure $ show err

equalityUnitTests =
  testGroup
    "Equality unit tests"
    $ map
      ( \(name, tokens, expected) -> testCase name $ do
          ast <- parseSuccess tokens
          assertEqual "" expected ast
      )
      equalityTestCases

equalityTestCases :: [(String, [(T.Token, T.Location)], [ASTNode])]
equalityTestCases =
  [ ( "Boolean true literal",
      [(T.Identifier "true", T.Location 0 0)],
      [ASTNode (BooleanLiteral True) (T.Location 0 0)]
    ),
    ( "Boolean false literal",
      [(T.Identifier "false", T.Location 0 0)],
      [ASTNode (BooleanLiteral False) (T.Location 0 0)]
    ),
    ( "Unit value",
      [(T.Identifier "unit", T.Location 0 0)],
      [ASTNode (Unit) (T.Location 0 0)]
    ),
    ( "Addition with integers",
      T.tokenize "1 + 2",
      [ ASTNode
          ( Apply
              (ASTNode (IdentifierAST "+") (T.Location 0 2))
              [ ASTNode (IntegerLiteral 1) (T.Location 0 0),
                ASTNode (IntegerLiteral 2) (T.Location 0 4)
              ]
          )
          (T.Location 0 2)
      ]
    ),
    ( "Addition with identifiers",
      T.tokenize "a + b",
      [ ASTNode
          ( Apply
              (ASTNode (IdentifierAST "+") (T.Location 0 2))
              [ ASTNode (IdentifierAST "a") (T.Location 0 0),
                ASTNode (IdentifierAST "b") (T.Location 0 4)
              ]
          )
          (T.Location 0 2)
      ]
    ),
    ( "Left associative addition of three numbers",
      T.tokenize "1 + 2 + 3",
      [ ASTNode
          ( Apply
              (ASTNode (IdentifierAST "+") (T.Location 0 6))
              [ ASTNode
                  ( Apply
                      (ASTNode (IdentifierAST "+") (T.Location 0 2))
                      [ ASTNode (IntegerLiteral 1) (T.Location 0 0),
                        ASTNode (IntegerLiteral 2) (T.Location 0 4)
                      ]
                  )
                  (T.Location 0 2),
                ASTNode (IntegerLiteral 3) (T.Location 0 8)
              ]
          )
          (T.Location 0 6)
      ]
    ),
    ( "Left associative addition of four numbers",
      T.tokenize "1 + 2 + 3 + 4",
      let onePlusTwo =
            ASTNode
              ( Apply
                  (ASTNode (IdentifierAST "+") (T.Location 0 2))
                  [ ASTNode (IntegerLiteral 1) (T.Location 0 0),
                    ASTNode (IntegerLiteral 2) (T.Location 0 4)
                  ]
              )
              (T.Location 0 2)
          onePlusTwoPlusThree =
            ASTNode
              ( Apply
                  (ASTNode (IdentifierAST "+") (T.Location 0 6))
                  [ onePlusTwo,
                    ASTNode (IntegerLiteral 3) (T.Location 0 8)
                  ]
              )
              (T.Location 0 6)
       in [ ASTNode
              ( Apply
                  (ASTNode (IdentifierAST "+") (T.Location 0 10))
                  [ onePlusTwoPlusThree,
                    ASTNode (IntegerLiteral 4) (T.Location 0 12)
                  ]
              )
              (T.Location 0 10)
          ]
    ),
    ( "Multiply precedence over addition",
      T.tokenize "1 + 2 * 3",
      let twoTimesThree =
            ASTNode
              ( Apply
                  (ASTNode (IdentifierAST "*") (T.Location 0 6))
                  [ ASTNode (IntegerLiteral 2) (T.Location 0 4),
                    ASTNode (IntegerLiteral 3) (T.Location 0 8)
                  ]
              )
              (T.Location 0 6)
       in [ ASTNode
              ( Apply
                  (ASTNode (IdentifierAST "+") (T.Location 0 2))
                  [ ASTNode (IntegerLiteral 1) (T.Location 0 0),
                    twoTimesThree
                  ]
              )
              (T.Location 0 2)
          ]
    ),
    ( "Parentheses precedence",
      T.tokenize "(a + b) * c",
      let aPlusB =
            ASTNode
              ( Apply
                  (ASTNode (IdentifierAST "+") (T.Location 0 3))
                  [ ASTNode (IdentifierAST "a") (T.Location 0 1),
                    ASTNode (IdentifierAST "b") (T.Location 0 5)
                  ]
              )
              (T.Location 0 3)
       in [ ASTNode
              ( Apply
                  (ASTNode (IdentifierAST "*") (T.Location 0 8))
                  [ aPlusB,
                    ASTNode (IdentifierAST "c") (T.Location 0 10)
                  ]
              )
              (T.Location 0 8)
          ]
    ),
    ( "if then else",
      T.tokenize "if true then 1 else 2",
      [ ASTNode
          ( If
              (ASTNode (BooleanLiteral True) (T.Location 0 3))
              (ASTNode (IntegerLiteral 1) (T.Location 0 13))
              (ASTNode (IntegerLiteral 2) (T.Location 0 20))
          )
          (T.Location 0 0)
      ]
    ),
    ( "if then",
      T.tokenize "if true then 1",
      [ ASTNode
          ( If
              (ASTNode (BooleanLiteral True) (T.Location 0 3))
              (ASTNode (IntegerLiteral 1) (T.Location 0 13))
              (ASTNode Unit T.NoLocation)
          )
          (T.Location 0 0)
      ]
    ),
    ( "No argument function call",
      T.tokenize "f()",
      [ ASTNode
          ( Apply
              (ASTNode (IdentifierAST "f") (T.Location 0 0))
              []
          )
          (T.Location 0 0)
      ]
    ),
    ( "Single argument function call",
      T.tokenize "f(1)",
      [ ASTNode
          ( Apply
              (ASTNode (IdentifierAST "f") (T.Location 0 0))
              [ASTNode (IntegerLiteral 1) (T.Location 0 2)]
          )
          (T.Location 0 0)
      ]
    ),
    ( "Two argument function call",
      T.tokenize "f(1, 2)",
      [ ASTNode
          ( Apply
              (ASTNode (IdentifierAST "f") (T.Location 0 0))
              [ ASTNode (IntegerLiteral 1) (T.Location 0 2),
                ASTNode (IntegerLiteral 2) (T.Location 0 5)
              ]
          )
          (T.Location 0 0)
      ]
    ),
    ( "Expression argument function call",
      T.tokenize "f(1, 2 + 3)",
      let twoPlusThree =
            ASTNode
              ( Apply
                  (ASTNode (IdentifierAST "+") (T.Location 0 7))
                  [ ASTNode (IntegerLiteral 2) (T.Location 0 5),
                    ASTNode (IntegerLiteral 3) (T.Location 0 9)
                  ]
              )
              (T.Location 0 7)
       in [ ASTNode
              ( Apply
                  (ASTNode (IdentifierAST "f") (T.Location 0 0))
                  [ ASTNode (IntegerLiteral 1) (T.Location 0 2),
                    twoPlusThree
                  ]
              )
              (T.Location 0 0)
          ]
    ),
    ( "Unary not operator",
      T.tokenize "not true",
      [ ASTNode
          ( Apply
              (ASTNode (IdentifierAST "not") (T.Location 0 0))
              [ASTNode (BooleanLiteral True) (T.Location 0 4)]
          )
          (T.Location 0 0)
      ]
    ),
    ( "Unary not operator chaining",
      T.tokenize "not not true",
      let notTrue =
            ASTNode
              ( Apply
                  (ASTNode (IdentifierAST "not") (T.Location 0 4))
                  [ASTNode (BooleanLiteral True) (T.Location 0 8)]
              )
              (T.Location 0 4)
       in [ ASTNode
              ( Apply
                  (ASTNode (IdentifierAST "not") (T.Location 0 0))
                  [notTrue]
              )
              (T.Location 0 0)
          ]
    ),
    ( "Expressions pass with semicolon",
      T.tokenize "a; c",
      [ ASTNode (IdentifierAST "a") (T.Location 0 0),
        ASTNode (IdentifierAST "c") (T.Location 0 3)
      ]
    ),
    ( "Empty block expression",
      T.tokenize "{}",
      [ASTNode (Block [] (ASTNode Unit T.NoLocation)) (T.Location 0 0)]
    ),
    ( "Block expression without result expression",
      T.tokenize "{ a; b; c; }",
      [ ASTNode
          ( Block
              [ ASTNode (IdentifierAST "a") (T.Location 0 2),
                ASTNode (IdentifierAST "b") (T.Location 0 5),
                ASTNode (IdentifierAST "c") (T.Location 0 8)
              ]
              (ASTNode Unit T.NoLocation)
          )
          (T.Location 0 0)
      ]
    ),
    ( "Block expression with result expression",
      T.tokenize "{ a; b; c; d }",
      [ ASTNode
          ( Block
              [ ASTNode (IdentifierAST "a") (T.Location 0 2),
                ASTNode (IdentifierAST "b") (T.Location 0 5),
                ASTNode (IdentifierAST "c") (T.Location 0 8)
              ]
              (ASTNode (IdentifierAST "d") (T.Location 0 11))
          )
          (T.Location 0 0)
      ]
    ),
    ( "While expression",
      T.tokenize "while true do { a; b; }",
      let block =
            ASTNode
              ( Block
                  [ ASTNode (IdentifierAST "a") (T.Location 0 16),
                    ASTNode (IdentifierAST "b") (T.Location 0 19)
                  ]
                  (ASTNode Unit T.NoLocation)
              )
              (T.Location 0 14)
       in [ ASTNode
              ( Apply
                  (ASTNode (IdentifierAST "while") (T.Location 0 0))
                  [ ASTNode (BooleanLiteral True) (T.Location 0 6),
                    block
                  ]
              )
              (T.Location 0 0)
          ]
    ),
    ( "Variable declaration in top level",
      T.tokenize "var x = 123",
      [ ASTNode
          ( VarDecl
              (ASTNode {ast = IdentifierAST "x", loc = T.Location 0 4})
              (ASTNode {ast = IntegerLiteral 123, loc = T.Location 0 8})
          )
          (T.Location 0 0)
      ]
    ),
    ( "Variable declaration in block",
      T.tokenize "{var x = 123}",
      [ ASTNode
          ( Block
              []
              ( ASTNode
                  ( VarDecl
                      (ASTNode {ast = IdentifierAST "x", loc = T.Location 0 5})
                      (ASTNode {ast = IntegerLiteral 123, loc = T.Location 0 9})
                  )
                  (T.Location 0 1)
              )
          )
          (T.Location 0 0)
      ]
    ),
    ( "Right-associative variable assignment",
      T.tokenize "x = y = 123",
      [ ASTNode
          ( Apply
              (ASTNode (IdentifierAST "=") (T.Location 0 2))
              [ ASTNode (IdentifierAST "x") (T.Location 0 0),
                ASTNode
                  ( Apply
                      (ASTNode (IdentifierAST "=") (T.Location 0 6))
                      [ ASTNode (IdentifierAST "y") (T.Location 0 4),
                        ASTNode (IntegerLiteral 123) (T.Location 0 8)
                      ]
                  )
                  (T.Location 0 6)
              ]
          )
          (T.Location 0 2)
      ]
    ),
    ( "Typed variable assignment",
      T.tokenize "var x: Int = 123",
      [ ASTNode
          ( TypedVarDecl
              (ASTNode {ast = IdentifierAST "x", loc = T.Location 0 4})
              (ASTNode {ast = IdentifierAST "Int", loc = T.Location 0 7})
              (ASTNode {ast = IntegerLiteral 123, loc = T.Location 0 13})
          )
          (T.Location 0 0)
      ]
    )
  ]

unitTests =
  testGroup
    "Unit tests"
    [ testCase
        "Expressions fail without semicolon"
        $ do
          let ast = parse $ T.tokenize "a + b c"
          assertEqual "" True (isLeft ast),
      testCase
        "Block expression without intermediate semicolon fails"
        $ do
          let ast = parse $ T.tokenize "{ a; b c; }"
          assertEqual "" True (isLeft ast),
      testCase
        "Test program passes"
        $ do
          let code =
                unlines
                  [ "{",
                    "    while f() do {",
                    "        x = 10;",
                    "        y = if g(x) then {",
                    "            x = x + 1;",
                    "            x",
                    "        } else {",
                    "            g(x)",
                    "        };  # <-- (this semicolon will become optional later)",
                    "        g(y);",
                    "    };  # <------ (this too)",
                    "    123",
                    "}"
                  ]
          let ast = parse $ T.tokenize code
          case ast of
            Left err -> do
              assertFailure $ show err
            Right _ -> return (),
      testCase "Infer semicolon after block in block" $
        do
          let expr1 = "{ { a }  { b } }"
          let expr2 = "{ { a }; { b } }"
          ast1 <- parseSuccess $ T.tokenize expr1
          ast2 <- parseSuccess $ T.tokenize expr2
          assertEqual "" ast1 ast2,
      testCase "Infer semicolon after block in if then" $
        do
          let expr1 = "{if true then { a }  b}"
          let expr2 = "{if true then { a }; b}"
          ast1 <- parseSuccess $ T.tokenize expr1
          ast2 <- parseSuccess $ T.tokenize expr2
          assertEqual "" ast1 ast2,
      testCase
        "Infer semicolon after block in if then else"
        $ do
          let expr1 = "{ if true then { a } else { b }  3 }"
          let expr2 = "{ if true then { a } else { b }; 3 }"
          ast1 <- parseSuccess $ T.tokenize expr1
          ast2 <- parseSuccess $ T.tokenize expr2
          assertEqual "" ast1 ast2,
      testCase
        "Infer semicolon after block in while"
        $ do
          let expr1 = "{ while true do { a }  3 }"
          let expr2 = "{ while true do { a }; 3 }"
          ast1 <- parseSuccess $ T.tokenize expr1
          ast2 <- parseSuccess $ T.tokenize expr2
          assertEqual "" ast1 ast2,
      testCase
        "Variable declaration in if then does not pass"
        $ do
          let ast = parse $ T.tokenize "if true then var x = 3"
          assertEqual "" True (isLeft ast)
    ]