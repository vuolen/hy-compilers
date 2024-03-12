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

mkASTNode :: AST -> T.Location -> ASTNode
mkASTNode ast loc = ASTNode {ast = ast, loc = loc}

parserTests = testGroup "Parser" [propertyTests, unitTests, equalityUnitTests]

propertyTests =
  testGroup
    "Property tests"
    [ testProperty "Positive integer literal" $
        do
          value <- gen $ Gen.inRange $ Range.between (0, maxBound)
          let literal = show value
          let tokens = T.tokenize literal
          ast <- case parse tokens of
            Left (ParserError {message}) -> testFailed message
            Right result -> return result
          assert $ P.expect 1 .$ ("number of tokens", length tokens)
          assert $ P.expect (IntegerLiteral value) .$ ("ast", fst $ head ast),
      testProperty "Negative integer literal" $
        do
          value <- gen $ Gen.inRange $ Range.between (0, maxBound)
          let literal = show value
          let tokens = T.tokenize $ "-" ++ literal
          ast <- case parse tokens of
            Left (ParserError {message}) -> testFailed message
            Right result -> return result
          let expected =
                [ ( Apply
                      (ASTNode {ast = IdentifierAST "-", loc = T.Location 0 0})
                      [ASTNode {ast = IntegerLiteral value, loc = T.Location 0 1}],
                    T.Location 0 0
                  )
                ]
          assert $ P.expect expected .$ ("ast", ast),
      testProperty "Binary operations supported" $
        do
          op <- gen $ Gen.elem $ fromList ["+", "-", "*", "/", "%", "==", "!=", "<", "<=", ">", ">=", "and", "or", "="]
          let tokens = T.tokenize $ "1 " ++ op ++ " 2"
          let expected =
                [ ( Apply
                      (ASTNode {ast = IdentifierAST op, loc = T.Location 0 2})
                      [ ASTNode {ast = IntegerLiteral 1, loc = T.Location 0 0},
                        ASTNode {ast = IntegerLiteral 2, loc = T.Location 0 (length op + 3)}
                      ],
                    T.Location 0 2
                  )
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

parseSuccess :: [(T.Token, T.Location)] -> IO [(AST, T.Location)]
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

equalityTestCases :: [(String, [(T.Token, T.Location)], [(AST, T.Location)])]
equalityTestCases =
  [ ( "Boolean true literal",
      [(T.Identifier "true", T.Location 0 0)],
      [(BooleanLiteral True, T.Location 0 0)]
    ),
    ( "Boolean false literal",
      [(T.Identifier "false", T.Location 0 0)],
      [(BooleanLiteral False, T.Location 0 0)]
    ),
    ( "Unit value",
      [(T.Identifier "unit", T.Location 0 0)],
      [(Unit, T.Location 0 0)]
    ),
    ( "Addition with integers",
      T.tokenize "1 + 2",
      [ ( Apply
            (mkASTNode (IdentifierAST "+") (T.Location 0 2))
            [ mkASTNode (IntegerLiteral 1) (T.Location 0 0),
              mkASTNode (IntegerLiteral 2) (T.Location 0 4)
            ],
          T.Location 0 2
        )
      ]
    ),
    ( "Addition with identifiers",
      T.tokenize "a + b",
      [ ( Apply
            (mkASTNode (IdentifierAST "+") (T.Location 0 2))
            [ mkASTNode (IdentifierAST "a") (T.Location 0 0),
              mkASTNode (IdentifierAST "b") (T.Location 0 4)
            ],
          T.Location 0 2
        )
      ]
    ),
    ( "Left associative addition of three numbers",
      T.tokenize "1 + 2 + 3",
      [ ( Apply
            (mkASTNode (IdentifierAST "+") (T.Location 0 6))
            [ mkASTNode
                ( Apply
                    (mkASTNode (IdentifierAST "+") (T.Location 0 2))
                    [ mkASTNode (IntegerLiteral 1) (T.Location 0 0),
                      mkASTNode (IntegerLiteral 2) (T.Location 0 4)
                    ]
                )
                (T.Location 0 2),
              mkASTNode (IntegerLiteral 3) (T.Location 0 8)
            ],
          T.Location 0 6
        )
      ]
    ),
    ( "Left associative addition of four numbers",
      T.tokenize "1 + 2 + 3 + 4",
      let onePlusTwo =
            mkASTNode
              ( Apply
                  (mkASTNode (IdentifierAST "+") (T.Location 0 2))
                  [ mkASTNode (IntegerLiteral 1) (T.Location 0 0),
                    mkASTNode (IntegerLiteral 2) (T.Location 0 4)
                  ]
              )
              (T.Location 0 2)
          onePlusTwoPlusThree =
            mkASTNode
              ( Apply
                  (mkASTNode (IdentifierAST "+") (T.Location 0 6))
                  [ onePlusTwo,
                    mkASTNode (IntegerLiteral 3) (T.Location 0 8)
                  ]
              )
              (T.Location 0 6)
       in [ ( Apply
                (mkASTNode (IdentifierAST "+") (T.Location 0 10))
                [ onePlusTwoPlusThree,
                  mkASTNode (IntegerLiteral 4) (T.Location 0 12)
                ],
              T.Location 0 10
            )
          ]
    ),
    ( "Multiply precedence over addition",
      T.tokenize "1 + 2 * 3",
      let twoTimesThree =
            mkASTNode
              ( Apply
                  (mkASTNode (IdentifierAST "*") (T.Location 0 6))
                  [ mkASTNode (IntegerLiteral 2) (T.Location 0 4),
                    mkASTNode (IntegerLiteral 3) (T.Location 0 8)
                  ]
              )
              (T.Location 0 6)
       in [ ( Apply
                (mkASTNode (IdentifierAST "+") (T.Location 0 2))
                [ mkASTNode (IntegerLiteral 1) (T.Location 0 0),
                  twoTimesThree
                ],
              T.Location 0 2
            )
          ]
    ),
    ( "Parentheses precedence",
      T.tokenize "(a + b) * c",
      let aPlusB =
            mkASTNode
              ( Apply
                  (mkASTNode (IdentifierAST "+") (T.Location 0 3))
                  [ mkASTNode (IdentifierAST "a") (T.Location 0 1),
                    mkASTNode (IdentifierAST "b") (T.Location 0 5)
                  ]
              )
              (T.Location 0 3)
       in [ ( Apply
                (mkASTNode (IdentifierAST "*") (T.Location 0 8))
                [ aPlusB,
                  mkASTNode (IdentifierAST "c") (T.Location 0 10)
                ],
              T.Location 0 8
            )
          ]
    ),
    ( "if then else",
      T.tokenize "if true then 1 else 2",
      [ ( If
            (mkASTNode (BooleanLiteral True) (T.Location 0 3))
            (mkASTNode (IntegerLiteral 1) (T.Location 0 13))
            (mkASTNode (IntegerLiteral 2) (T.Location 0 20)),
          T.Location 0 0
        )
      ]
    ),
    ( "if then",
      T.tokenize "if true then 1",
      [ ( If
            (mkASTNode (BooleanLiteral True) (T.Location 0 3))
            (mkASTNode (IntegerLiteral 1) (T.Location 0 13))
            (mkASTNode Unit T.NoLocation),
          T.Location 0 0
        )
      ]
    ),
    ( "No argument function call",
      T.tokenize "f()",
      [ ( Apply
            (mkASTNode (IdentifierAST "f") (T.Location 0 0))
            [mkASTNode Unit (T.Location 0 1)],
          T.Location 0 0
        )
      ]
    ),
    ( "Single argument function call",
      T.tokenize "f(1)",
      [ ( Apply
            (mkASTNode (IdentifierAST "f") (T.Location 0 0))
            [mkASTNode (IntegerLiteral 1) (T.Location 0 2)],
          T.Location 0 0
        )
      ]
    ),
    ( "Two argument function call",
      T.tokenize "f(1, 2)",
      [ ( Apply
            (mkASTNode (IdentifierAST "f") (T.Location 0 0))
            [ mkASTNode (IntegerLiteral 1) (T.Location 0 2),
              mkASTNode (IntegerLiteral 2) (T.Location 0 5)
            ],
          T.Location 0 0
        )
      ]
    ),
    ( "Expression argument function call",
      T.tokenize "f(1, 2 + 3)",
      let twoPlusThree =
            mkASTNode
              ( Apply
                  (mkASTNode (IdentifierAST "+") (T.Location 0 7))
                  [ mkASTNode (IntegerLiteral 2) (T.Location 0 5),
                    mkASTNode (IntegerLiteral 3) (T.Location 0 9)
                  ]
              )
              (T.Location 0 7)
       in [ ( Apply
                (mkASTNode (IdentifierAST "f") (T.Location 0 0))
                [ mkASTNode (IntegerLiteral 1) (T.Location 0 2),
                  twoPlusThree
                ],
              T.Location 0 0
            )
          ]
    ),
    ( "Unary not operator",
      T.tokenize "not true",
      [ ( Apply
            (mkASTNode (IdentifierAST "not") (T.Location 0 0))
            [mkASTNode (BooleanLiteral True) (T.Location 0 4)],
          T.Location 0 0
        )
      ]
    ),
    ( "Unary not operator chaining",
      T.tokenize "not not true",
      let notTrue =
            mkASTNode
              ( Apply
                  (mkASTNode (IdentifierAST "not") (T.Location 0 4))
                  [mkASTNode (BooleanLiteral True) (T.Location 0 8)]
              )
              (T.Location 0 4)
       in [ ( Apply
                (mkASTNode (IdentifierAST "not") (T.Location 0 0))
                [notTrue],
              T.Location 0 0
            )
          ]
    ),
    ( "Expressions pass with semicolon",
      T.tokenize "a; c",
      [ ( IdentifierAST "a",
          T.Location 0 0
        ),
        (IdentifierAST "c", T.Location 0 3)
      ]
    ),
    ( "Empty block expression",
      T.tokenize "{}",
      [(Block [] Unit, T.Location 0 0)]
    ),
    ( "Block expression without result expression",
      T.tokenize "{ a; b; c; }",
      [ ( Block
            [IdentifierAST "a", IdentifierAST "b", IdentifierAST "c"]
            Unit,
          T.Location 0 0
        )
      ]
    ),
    ( "Block expression with result expression",
      T.tokenize "{ a; b; c; d }",
      [ ( Block
            [IdentifierAST "a", IdentifierAST "b", IdentifierAST "c"]
            (IdentifierAST "d"),
          T.Location 0 0
        )
      ]
    ),
    ( "While expression",
      T.tokenize "while true do { a; b; }",
      let block = mkASTNode (Block [IdentifierAST "a", IdentifierAST "b"] Unit) (T.Location 0 14)
       in [ ( Apply
                (mkASTNode (IdentifierAST "while") (T.Location 0 0))
                [ mkASTNode (BooleanLiteral True) (T.Location 0 6),
                  block
                ],
              T.Location 0 0
            )
          ]
    ),
    ( "Variable declaration in top level",
      T.tokenize "var x = 123",
      [ ( VarDecl
            (ASTNode {ast = IdentifierAST "x", loc = T.Location 0 4})
            (ASTNode {ast = IntegerLiteral 123, loc = T.Location 0 8}),
          T.Location 0 0
        )
      ]
    ),
    ( "Variable declaration in block",
      T.tokenize "{var x = 123}",
      [ ( Block
            []
            ( VarDecl
                (ASTNode {ast = IdentifierAST "x", loc = T.Location 0 5})
                (ASTNode {ast = IntegerLiteral 123, loc = T.Location 0 9})
            ),
          T.Location 0 0
        )
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
          let expr1 = "{ { a } { b } }"
          let expr2 = "{ { a }; { b } }"
          ast1 <- parseSuccess $ T.tokenize expr1
          ast2 <- parseSuccess $ T.tokenize expr2
          assertEqual "" ast1 ast2,
      testCase "Infer semicolon after block in if then" $
        do
          let expr1 = "{if true then { a } b}"
          let expr2 = "{if true then { a }; b}"
          ast1 <- parseSuccess $ T.tokenize expr1
          ast2 <- parseSuccess $ T.tokenize expr2
          assertEqual "" ast1 ast2,
      testCase
        "Infer semicolon after block in if then else"
        $ do
          let expr1 = "{ if true then { a } else { b } 3 }"
          let expr2 = "{ if true then { a } else { b }; 3 }"
          ast1 <- parseSuccess $ T.tokenize expr1
          ast2 <- parseSuccess $ T.tokenize expr2
          assertEqual "" ast1 ast2,
      testCase
        "Infer semicolon after block in while"
        $ do
          let expr1 = "{ while true do { a } 3 }"
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