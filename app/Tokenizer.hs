module Tokenizer where

import Prelude
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import Data.Char (isSpace)
import Text.Regex (mkRegex, matchRegex, Regex)
import Data.Maybe (Maybe(..))
import Control.Applicative (asum)

type SourceCode = String

data Location = Location Int Int deriving (Eq, Show)

data Token = IntegerLiteral Int | Identifier String | Operator String | Punctuation String deriving (Eq, Show)

integerLiteralRegex = mkRegex "^([0-9]+)"
identifierRegex = mkRegex "^([a-zA-Z_][a-zA-Z_0-9]*)"
operatorRegex = mkRegex "^(\\+|-|\\*|\\/|==|!=|<=|>=|=|<|>)"
punctuationRegex = mkRegex "^(\\(|\\)|\\{|\\}|;|,)"

-- if the regex matches, return the match and the rest of the source
readRegex :: Regex -> SourceCode -> Maybe (String, SourceCode)
readRegex regex source = case matchRegex regex source of
    Just [match] -> Just (match, drop (length match) source)
    _ -> Nothing 

regexTokenizer :: Regex -> (SourceCode -> Token) -> SourceCode -> Maybe (Token, String, SourceCode)
regexTokenizer regex matchHandler source = case readRegex regex source of
    Just (match, rest) -> Just (matchHandler match, match, rest)
    _ -> Nothing

tokenizers = [
    regexTokenizer integerLiteralRegex (\match -> IntegerLiteral (read match)),
    regexTokenizer identifierRegex (\match -> Identifier match),
    regexTokenizer operatorRegex (\match -> Operator match),
    regexTokenizer punctuationRegex (\match -> Punctuation match)]


tokenize :: SourceCode -> [(Token, Location)]
tokenize source = tokenize' source [] (Location 0 0)
    where
        tokenize' "" tokens location = tokens
        tokenize' source tokens location = 
            case source of
                ('\n':rest) -> tokenize' rest tokens (Location (line + 1) 0)
                (' ':rest) -> tokenize' rest tokens (Location line (column + 1))
                _ -> case asum $ map (\tokenizer -> tokenizer source) tokenizers of
                        Just (token, match, rest) -> tokenize' rest (tokens ++ [(token, location)]) (Location line (column + (length match)))
                        Nothing -> error $ "No tokenizer matched source " ++ (show source)
            where
                (Location line column) = location