module TestTokenizer where

import Control.Monad (unless)
import Data.Int (Int64)
import Data.List.NonEmpty (append, fromList)
import Data.Text qualified as T
import Test.Falsify.Generator qualified as Gen
import Test.Falsify.Predicate as P
import Test.Falsify.Range qualified as Range
import Test.Tasty
import Test.Tasty.Falsify
import Test.Tasty.HUnit (assertEqual, testCase)
import Tokenizer (IntLit, Location (..), Token (..), integerLiteralRegex, tokenize, tokenizers)

tokenizerTests = testGroup "Tokenizer" [propertyTests, unitTests]

genIdentifierString :: Gen String
genIdentifierString = do
  let startingChars = fromList "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_"
  let followingChars = append startingChars (fromList "0123456789")

  firstChar <- Gen.elem startingChars
  -- Lets draw the line at 100 characters? Maybe
  restChars <- Gen.list (Range.between (0, 100)) $ Gen.elem followingChars

  return $ firstChar : restChars

propertyTests =
  testGroup
    "Property tests"
    [ testProperty "Integer string tokenized as integer literal" $
        do
          n <- gen $ Gen.inRange $ Range.between (0, maxBound)
          let literal = show n

          let tokens = tokenize literal

          let (token, location) = head tokens

          assert $ P.expect 1 .$ ("number of tokens", length tokens)
          assert $ P.expect (IntegerLiteral n) .$ ("token", fst $ head tokens),
      testProperty "Identifier string tokenized as identifier" $
        do
          identifier <- gen genIdentifierString

          let tokens = tokenize identifier

          let (token, location) = head tokens

          assert $ P.expect 1 .$ ("number of tokens", length tokens)
          assert $ P.expect (Identifier identifier) .$ ("token", fst $ head tokens),
      testProperty "Operator string tokenized as operator" $
        do
          operator <- gen $ Gen.elem $ fromList ["+", "-", "*", "/", "==", "!=", "<=", ">=", "=", "<", ">"]

          let tokens = tokenize operator

          let (token, location) = head tokens

          assert $ P.expect 1 .$ ("number of tokens", length tokens)
          assert $ P.expect (Operator operator) .$ ("token", fst $ head tokens),
      testProperty "Punctuation string tokenized as punctuation" $
        do
          punctuation <- gen $ Gen.elem $ fromList ["(", ")", "{", "}", ";", ","]

          let tokens = tokenize punctuation

          assert $ P.expect 1 .$ ("number of tokens", length tokens)
          assert $ P.expect (Punctuation punctuation) .$ ("token", fst $ head tokens),
      testProperty "Comment string hash is ignored" $
        do
          randomString <- gen $ Gen.list (Range.between (0, 100)) $ Gen.inRange $ Range.enum (minBound, maxBound)
          identifier <- gen genIdentifierString

          let comment = filter (== '\n') randomString
              source = "# " ++ comment ++ "\n" ++ identifier
              tokens = tokenize source

          assert $ P.expect 1 .$ ("number of tokens", length tokens)
          assert $ P.expect (Identifier identifier) .$ ("token", fst $ head tokens),
      testProperty "Comment string forward slashes is ignored" $
        do
          randomString <- gen $ Gen.list (Range.between (0, 100)) $ Gen.inRange $ Range.enum (minBound, maxBound)
          identifier <- gen genIdentifierString
          let comment = filter (== '\n') randomString
              source = "// " ++ comment ++ "\n" ++ identifier
              tokens = tokenize source
          assert $ P.expect 1 .$ ("number of tokens", length tokens)
    ]

unitTests =
  testGroup
    "Unit tests"
    [ testCase "Token starts at location 0 0" $
        do
          let tokens = tokenize "asd"
              (token, location) = head tokens
          assertEqual "location" location $ Location 0 0,
      testCase "Spaces increase tokens column" $
        do
          let tokens = tokenize "    asd"
              (token, location) = head tokens
          assertEqual "location" location $ Location 0 4,
      testCase "Line breaks increase tokens lines" $
        do
          let tokens = tokenize "\n\n\n\nasd"
              (token, location) = head tokens
          assertEqual "location" location $ Location 4 0,
      testCase "Line break resets column" $
        do
          let tokens = tokenize "    \nasd"
              (token, location) = head tokens
          assertEqual "location" location $ Location 1 0,
      testCase "No identifier and 'and' operator clash" $
        do
          let tokens = tokenize "andIdentifier"
              (token, location) = head tokens
          assertEqual "location" location $ Location 0 0
          assertEqual "token" token $ Identifier "andIdentifier",
      testCase "No identifier and 'or' operator clash" $
        do
          let tokens = tokenize "orIdentifier"
              (token, location) = head tokens
          assertEqual "location" location $ Location 0 0
          assertEqual "token" token $ Identifier "orIdentifier"
    ]