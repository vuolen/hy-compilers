module Main where

import Test.Tasty
import TestIR (irTests)
import TestParser (parserTests)
import TestTokenizer (tokenizerTests)
import TestTypeChecker (typeCheckerTests)

main = defaultMain $ testGroup "Tests" [tokenizerTests, parserTests, typeCheckerTests, irTests]