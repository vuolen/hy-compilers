module Main where
    
import Test.Tasty
import TestTokenizer (tokenizerTests)
import TestParser (parserTests)

main = defaultMain $ testGroup "Tests" [tokenizerTests, parserTests]