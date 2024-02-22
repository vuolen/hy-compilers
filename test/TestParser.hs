module TestParser where

import Data.Bool
import Data.List.NonEmpty (fromList)
import Parser
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
            Left err -> testFailed err
            Right result -> return result
          assert $ P.expect 1 .$ ("number of tokens", length tokens)
          assert $ P.expect (IntegerLiteralAST value) .$ ("ast", fst $ head ast),
      testProperty "Negative integer literal" $
        do
          value <- gen $ Gen.inRange $ Range.between (0, maxBound)
          let literal = show value
          let tokens = tokenize $ "-" ++ literal
          ast <- case parse tokens of
            Left err -> testFailed err
            Right result -> return result
          assert $ P.expect (IntegerLiteralAST (-value)) .$ ("ast", fst $ head ast),
      testProperty "Binary operations supported" $
        do
          op <- gen $ Gen.elem $ fromList ["+", "-", "*", "/", "%", "==", "!=", "<", "<=", ">", ">=", "and", "or", "="]
          let tokens = tokenize $ "1 " ++ op ++ " 2"
          let expected = [(foldl Apply (IdentifierAST op) [IntegerLiteralAST 1, IntegerLiteralAST 2], Location 0 0)]
          ast <- case parse tokens of
            Left err -> testFailed err
            Right result -> return result
          assert $ P.expect expected .$ ("ast", ast)
    ]

parseSuccess :: [(Token, Location)] -> IO [(AST, Location)]
parseSuccess tokens = do
  case parse tokens of
    Right ast -> return ast
    Left err -> assertFailure err

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
            [(mkBinaryApply "+" (IntegerLiteralAST 1) (IntegerLiteralAST 2), Location 0 0)]
            ast,
      testCase "Addition with identifiers" $
        do
          ast <- parseSuccess $ tokenize "a + b"
          assertEqual
            ""
            [(mkBinaryApply "+" (IdentifierAST "a") (IdentifierAST "b"), Location 0 0)]
            ast,
      testCase "Left associative addition of three numbers" $
        do
          let onePlusTwo = mkBinaryApply "+" (IntegerLiteralAST 1) (IntegerLiteralAST 2)
          ast <- parseSuccess $ tokenize "1 + 2 + 3"
          assertEqual
            ""
            [(mkBinaryApply "+" onePlusTwo (IntegerLiteralAST 3), Location 0 0)]
            ast,
      testCase "Left associative addition of four numbers" $
        do
          let onePlusTwo = mkBinaryApply "+" (IntegerLiteralAST 1) (IntegerLiteralAST 2)
          let onePlusTwoPlusThree = mkBinaryApply "+" onePlusTwo (IntegerLiteralAST 3)
          ast <- parseSuccess $ tokenize "1 + 2 + 3 + 4"
          assertEqual
            ""
            [(mkBinaryApply "+" onePlusTwoPlusThree (IntegerLiteralAST 4), Location 0 0)]
            ast,
      testCase "Multiply precedence over addition" $
        do
          let twoTimesThree = mkBinaryApply "*" (IntegerLiteralAST 2) (IntegerLiteralAST 3)
          ast <- parseSuccess $ tokenize "1 + 2 * 3"
          assertEqual
            ""
            [(mkBinaryApply "+" (IntegerLiteralAST 1) twoTimesThree, Location 0 0)]
            ast,
      testCase "Parentheses precedence" $
        do
          let aPlusB = mkBinaryApply "+" (IdentifierAST "a") (IdentifierAST "b")
          ast <- parseSuccess $ tokenize "(a + b) * c"
          assertEqual
            ""
            [(mkBinaryApply "*" aPlusB (IdentifierAST "c"), Location 0 0)]
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
          let twoPlusThree = mkBinaryApply "+" (IntegerLiteralAST 2) (IntegerLiteralAST 3)
          ast <- parseSuccess $ tokenize "f(1, 2 + 3)"
          assertEqual "" [(Apply (Apply (IdentifierAST "f") (IntegerLiteralAST 1)) twoPlusThree, Location 0 0)] ast,
      testCase
        "Unary not operator"
        $ do
          ast <- parseSuccess $ tokenize "not true"
          assertEqual "" [(Apply (IdentifierAST "not") (BooleanLiteralAST True), Location 0 0)] ast
    ]