module Tokenizer where

import Prelude
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import Data.Char (isSpace)
import Data.Maybe (Maybe(..))
import Control.Applicative (asum)
import Text.Regex.PCRE ((=~))
import Data.Int (Int64)

type SourceCode = String

data Location = Location Int Int deriving (Eq, Show)

data Source = IntLit String deriving (Eq, Show)

type IntLit = String
mkIntLit :: String -> IntLit
mkIntLit string = case string =~ integerLiteralRegex of
    True -> string
    False -> error "Invalid integer literal"

data Token = IntegerLiteral Int64 | Identifier String | Operator String | Punctuation String deriving (Eq, Show)

integerLiteralRegex = "^([0-9]+)"
identifierRegex = "^([a-zA-Z_][a-zA-Z_0-9]*)"
operatorRegex = "^(\\+|-|\\*|\\/|==|!=|<=|>=|=|<|>)"
punctuationRegex = "^(\\(|\\)|\\{|\\}|;|,)"
commentRegex = "^((?:\\/\\/|#).*?)(?:\n|$)"

regexTokenizer :: String -> (SourceCode -> Maybe Token) -> SourceCode -> Maybe (Maybe Token, String, SourceCode)
regexTokenizer regex matchHandler source = case source =~ regex :: (String, String, String) of
    (_, "", _) -> Nothing
    (_, match, rest) -> Just (matchHandler match, match, rest)

tokenizers = [
    regexTokenizer commentRegex (\match -> Nothing),
    regexTokenizer integerLiteralRegex (\match -> Just $ IntegerLiteral (read match)),
    regexTokenizer identifierRegex (\match -> Just $ Identifier match),
    regexTokenizer operatorRegex (\match -> Just $ Operator match),
    regexTokenizer punctuationRegex (\match -> Just $ Punctuation match)]


tokenize :: SourceCode -> [(Token, Location)]
tokenize source = tokenize' source [] (Location 0 0)
    where
        tokenize' "" tokens location = tokens
        tokenize' source tokens location = 
            case source of
                ('\n':rest) -> tokenize' rest tokens (Location (line + 1) 0)
                (' ':rest) -> tokenize' rest tokens (Location line (column + 1))
                _ -> case asum $ map (\tokenizer -> tokenizer source) tokenizers of
                        Just (Just token, match, rest) -> tokenize' rest (tokens ++ [(token, location)]) (Location line (column + (length match)))
                        Just (Nothing, match, rest) -> tokenize' rest tokens (Location line (column + (length match)))
                        Nothing -> error $ "No tokenizer matched source " ++ (show source)
            where
                (Location line column) = location