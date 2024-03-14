module SymTab where

data SymTab a = SymTab
  { parent :: Maybe (SymTab a),
    symbols :: [(String, a)]
  }
  deriving (Show, Eq)

lookup :: String -> SymTab a -> Maybe a
lookup name (SymTab {symbols, parent}) = case Prelude.lookup name symbols of
  Just a -> Just a
  Nothing -> case parent of
    Just parentSymTab -> SymTab.lookup name parentSymTab
    Nothing -> Nothing

insert :: String -> a -> SymTab a -> SymTab a
insert name value symTab = symTab {symbols = newSymbols name value (symbols symTab)}
  where
    newSymbols name value [] = [(name, value)]
    newSymbols name value ((n, v) : xs) =
      if name == n
        then (name, value) : xs
        else (n, v) : newSymbols name value xs