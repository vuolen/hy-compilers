import Control.Monad (unless)
import qualified Data.Text as T
import Data.List.NonEmpty (fromList, append)

import Test.Tasty
import Test.Tasty.Falsify
import Test.Falsify.Generator qualified as Gen
import Test.Falsify.Range qualified as Range
import Test.Falsify.Predicate as P
import Test.Tasty.HUnit (assertEqual, testCase)
import Tokenizer (tokenize, Token(..), Location(..))

main = defaultMain $ testGroup "Tokenizer" [propertyTests, unitTests]

genIdentifierString :: Gen String
genIdentifierString = do
    let startingChars = fromList "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_"
    let followingChars = append startingChars (fromList "0123456789")

    firstChar <- Gen.elem startingChars
    -- Lets draw the line at 100 characters? Maybe
    restChars <- Gen.list (Range.between (0, 100)) $ Gen.elem followingChars

    return $ firstChar : restChars


propertyTests = testGroup "Property tests"
    [
        testProperty "Integer string tokenized as integer literal" $
            do
                n <- gen $ Gen.int $ Range.between (0, maxBound)
                let literal = show n

                let tokens = tokenize literal 

                let (token, location) = head tokens

                assert $ P.eq .$ ("token", token) .$ ("expected", IntegerLiteral n),

        testProperty "Identifier string tokenized as identifier" $
            do
                identifier <- gen genIdentifierString

                let tokens = tokenize identifier

                let (token, location) = head tokens
                
                assert $ P.eq .$ ("token", token) .$ ("expected", Identifier identifier),
        
        testProperty "Operator string tokenized as operator" $
            do
                operator <- gen $ Gen.elem $ fromList ["+", "-", "*", "/", "==", "!=", "<=", ">=", "=", "<", ">"]

                let tokens = tokenize operator

                let (token, location) = head tokens
                
                assert $ P.eq .$ ("token", token) .$ ("expected", Operator operator),

        testProperty "Punctuation string tokenized as punctuation" $
            do
                punctuation <- gen $ Gen.elem $ fromList ["(", ")", "{", "}", ";", ","]

                let tokens = tokenize punctuation

                let (token, location) = head tokens
                
                assert $ P.eq .$ ("token", token) .$ ("expected", Punctuation punctuation)

    ] 

unitTests = testGroup "Unit tests"
    [ 
        testCase "Token starts at location 0 0" $
            do
                let 
                    tokens = tokenize "asd"
                    (token, location) = head tokens
                assertEqual "location" location $ Location 0 0,

        testCase "Spaces increase tokens column" $
            do
                let 
                    tokens = tokenize "    asd"
                    (token, location) = head tokens
                assertEqual "location" location $ Location 0 4,

        testCase "Line breaks increase tokens lines" $
            do
                let 
                    tokens = tokenize "\n\n\n\nasd"
                    (token, location) = head tokens
                assertEqual "location" location $ Location 4 0,

        testCase "Line break resets column" $
            do
                let 
                    tokens = tokenize "    \nasd"
                    (token, location) = head tokens
                assertEqual "location" location $ Location 1 0
    ]