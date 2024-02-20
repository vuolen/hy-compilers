module TestParser where

import Test.Tasty
import Test.Tasty.Falsify
import Test.Falsify.Generator qualified as Gen
import Test.Falsify.Range qualified as Range
import Test.Falsify.Predicate as P
import Test.Tasty.HUnit (assertEqual, testCase)

import Tokenizer (Token(..), Location(..), tokenize)
import Parser
import Data.Bool

parserTests = testGroup "Parser" [propertyTests, unitTests]

propertyTests = testGroup "Property tests"
    [
        testProperty "Positive integer literal" $
            do
                value <- gen $ Gen.inRange $ Range.between (0, maxBound)
                let literal = show value
                let tokens = tokenize literal
                    ast = parse tokens
                assert $ P.expect 1 .$ ("number of tokens", length tokens) 
                assert $ P.expect (IntegerLiteralAST value) .$ ("ast", fst $ head ast),

        testProperty "Negative integer literal" $
            do
                value <- gen $ Gen.inRange $ Range.between (0, maxBound)
                let literal = show value
                let tokens = tokenize $ "-" ++ literal
                    ast = parse tokens
                assert $ P.expect (IntegerLiteralAST (-value)) .$ ("ast", fst $ head ast)
    ]

unitTests = testGroup "Unit tests"
    [
        testCase "Boolean true literal" $
            do assertEqual "" 
                [(BooleanLiteralAST True, Location 0 0)]
                (parse $ [(Identifier "true", Location 0 0)]),

        testCase "Boolean false literal" $
            do assertEqual "" 
                [(BooleanLiteralAST False, Location 0 0)]
                (parse $ [(Identifier "false", Location 0 0)]),

        testCase "Unit value" $
            do assertEqual "" 
                [(UnitAST, Location 0 0)]
                (parse $ [(Identifier "unit", Location 0 0)]),

        testCase "Addition with integers" $
            do assertEqual ""
                [(Apply (Apply (IdentifierAST "+") (IntegerLiteralAST 1)) (IntegerLiteralAST 2), Location 0 0)]
                (parse $ tokenize "1 + 2"),

        testCase "Addition with identifiers" $
            do assertEqual ""
                [(Apply (Apply (IdentifierAST "+") (IdentifierAST "a")) (IdentifierAST "b"), Location 0 0)]
                (parse $ tokenize "a + b"),

        testCase "Left associative addition" $
            do 
                let onePlusTwo = Apply (Apply (IdentifierAST "+") (IntegerLiteralAST 1)) (IntegerLiteralAST 2)
                assertEqual ""
                    [(Apply (Apply (IdentifierAST "+") onePlusTwo) (IntegerLiteralAST 3), Location 0 0)]
                    (parse $ tokenize "1 + 2 + 3")
    ]