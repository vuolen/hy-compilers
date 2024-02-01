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
            do
                let tokens = tokenize "true"
                    ast = parse tokens
                
                assertEqual "" (fst $ head ast) (BooleanLiteralAST True)
    ]