module TestParser where

import Data.Bool
import Data.Either (isLeft, isRight)
import Data.List.NonEmpty (fromList)
import Debug.Trace (traceM)
import Parser (AST (..), ParserError (..), applyArgs, applyTwo, parse)
import Test.Falsify.Generator qualified as Gen
import Test.Falsify.Predicate as P
import Test.Falsify.Range qualified as Range
import Test.Tasty
import Test.Tasty.Falsify
import Test.Tasty.HUnit (assertEqual, assertFailure, testCase)
import Tokenizer (Location (..), Token (..), tokenize)

parserTests = testGroup "Parser" [propertyTests, unitTests]

propertyTests =
  testGroup
    "Property tests"
    [ testProperty "Positive integer literal" $
        do
          value <- gen $ Gen.inRange $ Range.between (0, maxBound)
          let literal = show value
          let tokens = tokenize literal
          ast <- case parse tokens of
            Left (ParserError {message}) -> testFailed message
            Right result -> return result
          assert $ P.expect 1 .$ ("number of tokens", length tokens)
          assert $ P.expect (IntegerLiteralAST value) .$ ("ast", fst $ head ast),
      testProperty "Negative integer literal" $
        do
          value <- gen $ Gen.inRange $ Range.between (0, maxBound)
          let literal = show value
          let tokens = tokenize $ "-" ++ literal
          ast <- case parse tokens of
            Left (ParserError {message}) -> testFailed message
            Right result -> return result
          assert $ P.expect (Apply (IdentifierAST "-") (IntegerLiteralAST value)) .$ ("ast", fst $ head ast),
      testProperty "Binary operations supported" $
        do
          op <- gen $ Gen.elem $ fromList ["+", "-", "*", "/", "%", "==", "!=", "<", "<=", ">", ">=", "and", "or", "="]
          let tokens = tokenize $ "1 " ++ op ++ " 2"
          let expected = [(foldl Apply (IdentifierAST op) [IntegerLiteralAST 1, IntegerLiteralAST 2], Location 0 0)]
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

          let tokens = tokenize expr
          case parse tokens of
            Left (ParserError {message}) -> testFailed message
            Right _ -> return ()
    ]

parseSuccess :: [(Token, Location)] -> IO [(AST, Location)]
parseSuccess tokens = do
  case parse tokens of
    Right ast -> return ast
    Left err -> assertFailure $ show err

unitTests =
  testGroup
    "Unit tests"
    [ testCase "Boolean true literal" $
        do
          ast <- parseSuccess $ [(Identifier "true", Location 0 0)]
          assertEqual
            ""
            [(BooleanLiteralAST True, Location 0 0)]
            ast,
      testCase "Boolean false literal" $
        do
          ast <- parseSuccess $ [(Identifier "false", Location 0 0)]
          assertEqual
            ""
            [(BooleanLiteralAST False, Location 0 0)]
            ast,
      testCase
        "Unit value"
        $ do
          ast <- parseSuccess $ [(Identifier "unit", Location 0 0)]
          assertEqual
            ""
            [(UnitAST, Location 0 0)]
            ast,
      testCase "Addition with integers" $
        do
          ast <- parseSuccess $ tokenize "1 + 2"
          assertEqual
            ""
            [(applyArgs "+" [IntegerLiteralAST 1, IntegerLiteralAST 2], Location 0 0)]
            ast,
      testCase "Addition with identifiers" $
        do
          ast <- parseSuccess $ tokenize "a + b"
          assertEqual
            ""
            [(applyArgs "+" [IdentifierAST "a", IdentifierAST "b"], Location 0 0)]
            ast,
      testCase "Left associative addition of three numbers" $
        do
          let expected = foldl (applyTwo "+") (IntegerLiteralAST 1) $ map IntegerLiteralAST [2, 3]
          ast <- parseSuccess $ tokenize "1 + 2 + 3"
          assertEqual
            ""
            [(expected, Location 0 0)]
            ast,
      testCase "Left associative addition of four numbers" $
        do
          let expected = foldl (applyTwo "+") (IntegerLiteralAST 1) $ map IntegerLiteralAST [2, 3, 4]
          ast <- parseSuccess $ tokenize "1 + 2 + 3 + 4"
          assertEqual
            ""
            [(expected, Location 0 0)]
            ast,
      testCase "Multiply precedence over addition" $
        do
          let twoTimesThree = applyArgs "*" [IntegerLiteralAST 2, IntegerLiteralAST 3]
          ast <- parseSuccess $ tokenize "1 + 2 * 3"
          assertEqual
            ""
            [(applyArgs "+" [IntegerLiteralAST 1, twoTimesThree], Location 0 0)]
            ast,
      testCase "Parentheses precedence" $
        do
          let aPlusB = applyArgs "+" [IdentifierAST "a", IdentifierAST "b"]
          ast <- parseSuccess $ tokenize "(a + b) * c"
          assertEqual
            ""
            [(applyArgs "*" [aPlusB, IdentifierAST "c"], Location 0 0)]
            ast,
      testCase "if then else" $
        do
          let ifThenElse = IfAST (BooleanLiteralAST True) (IntegerLiteralAST 1) (IntegerLiteralAST 2)
          ast <- parseSuccess $ tokenize "if true then 1 else 2"
          assertEqual
            ""
            [(ifThenElse, Location 0 0)]
            ast,
      testCase "if then" $
        do
          let ifThen = IfAST (BooleanLiteralAST True) (IntegerLiteralAST 1) UnitAST
          ast <- parseSuccess $ tokenize "if true then 1"
          assertEqual "" [(ifThen, Location 0 0)] ast,
      testCase "No argument function call" $
        do
          ast <- parseSuccess $ tokenize "f()"
          assertEqual "" [(Apply (IdentifierAST "f") UnitAST, Location 0 0)] ast,
      testCase "Single argument function call" $
        do
          ast <- parseSuccess $ tokenize "f(1)"
          assertEqual "" [(Apply (IdentifierAST "f") (IntegerLiteralAST 1), Location 0 0)] ast,
      testCase "Two argument function call" $
        do
          ast <- parseSuccess $ tokenize "f(1, 2)"
          assertEqual "" [(Apply (Apply (IdentifierAST "f") (IntegerLiteralAST 1)) (IntegerLiteralAST 2), Location 0 0)] ast,
      testCase
        "Expression argument function call"
        $ do
          let twoPlusThree = applyArgs "+" [IntegerLiteralAST 2, IntegerLiteralAST 3]
          ast <- parseSuccess $ tokenize "f(1, 2 + 3)"
          assertEqual "" [(Apply (Apply (IdentifierAST "f") (IntegerLiteralAST 1)) twoPlusThree, Location 0 0)] ast,
      testCase
        "Unary not operator"
        $ do
          ast <- parseSuccess $ tokenize "not true"
          assertEqual "" [(Apply (IdentifierAST "not") (BooleanLiteralAST True), Location 0 0)] ast,
      testCase
        "Unary not operator chaining"
        $ do
          ast <- parseSuccess $ tokenize "not not true"
          assertEqual "" [(Apply (IdentifierAST "not") (Apply (IdentifierAST "not") (BooleanLiteralAST True)), Location 0 0)] ast,
      testCase
        "Expressions fail without semicolon"
        $ do
          let ast = parse $ tokenize "a + b c"
          assertEqual "" True (isLeft ast),
      testCase
        "Expressions pass with semicolon"
        $ do
          ast <- parseSuccess $ tokenize "a; c"
          assertEqual "" [(IdentifierAST "a", Location 0 0), (IdentifierAST "c", Location 0 3)] ast,
      testCase
        "Empty block expression"
        $ do
          ast <- parseSuccess $ tokenize "{}"
          assertEqual "" [(BlockAST [] UnitAST, Location 0 0)] ast,
      testCase
        "Block expression without result expression"
        $ do
          ast <- parseSuccess $ tokenize "{ a; b; c; }"
          assertEqual "" [(BlockAST [IdentifierAST "a", IdentifierAST "b", IdentifierAST "c"] UnitAST, Location 0 0)] ast,
      testCase
        "Block expression with result expression"
        $ do
          ast <- parseSuccess $ tokenize "{ a; b; c; d }"
          assertEqual "" [(BlockAST [IdentifierAST "a", IdentifierAST "b", IdentifierAST "c"] (IdentifierAST "d"), Location 0 0)] ast,
      testCase
        "Block expression without intermediate semicolon fails"
        $ do
          let ast = parse $ tokenize "{ a; b c; }"
          assertEqual "" True (isLeft ast),
      testCase
        "While expression"
        $ do
          let while = applyArgs "while" [BooleanLiteralAST True, BlockAST [IdentifierAST "a"] (IdentifierAST "b")]
          ast <- parseSuccess $ tokenize "while true do { a; b }"
          assertEqual "" [(while, Location 0 0)] ast,
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
          let ast = parse $ tokenize code
          case ast of
            Left err -> do
              assertFailure $ show err
            Right _ -> return (),
      testCase "Infer semicolon after block in block" $
        do
          let expr1 = "{ { a } { b } }"
          let expr2 = "{ { a }; { b } }"
          ast1 <- parseSuccess $ tokenize expr1
          ast2 <- parseSuccess $ tokenize expr2
          assertEqual "" ast1 ast2,
      testCase "Infer semicolon after block in if then" $
        do
          let expr1 = "{if true then { a } b}"
          let expr2 = "{if true then { a }; b}"
          ast1 <- parseSuccess $ tokenize expr1
          ast2 <- parseSuccess $ tokenize expr2
          assertEqual "" ast1 ast2,
      testCase
        "Infer semicolon after block in if then else"
        $ do
          let expr1 = "{ if true then { a } else { b } 3 }"
          let expr2 = "{ if true then { a } else { b }; 3 }"
          ast1 <- parseSuccess $ tokenize expr1
          ast2 <- parseSuccess $ tokenize expr2
          assertEqual "" ast1 ast2,
      testCase
        "Infer semicolon after block in while"
        $ do
          let expr1 = "{ while true do { a } 3 }"
          let expr2 = "{ while true do { a }; 3 }"
          ast1 <- parseSuccess $ tokenize expr1
          ast2 <- parseSuccess $ tokenize expr2
          assertEqual "" ast1 ast2,
      testCase
        "Variable declaration in top level"
        $ do
          ast <- parseSuccess $ tokenize "var x = 123"
          assertEqual "" [(VarDeclAST (IdentifierAST "x") (IntegerLiteralAST 123), Location 0 0)] ast,
      testCase
        "Variable declaration in if then does not pass"
        $ do
          let ast = parse $ tokenize "if true then var x = 3"
          assertEqual "" True (isLeft ast),
      testCase
        "Variable declaration in block"
        $ do
          ast <- parseSuccess $ tokenize "{var x = 123}"
          assertEqual "" [(BlockAST [] (VarDeclAST (IdentifierAST "x") (IntegerLiteralAST 123)), Location 0 0)] ast
    ]