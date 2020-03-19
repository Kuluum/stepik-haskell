import Text.Read (readMaybe)

data Token = Number Int | Plus | Minus | LeftBrace | RightBrace     
    deriving (Eq, Show)

asToken :: String -> Maybe Token
asToken "+" = Just Plus
asToken "-" = Just Minus
asToken "(" = Just LeftBrace
asToken ")" = Just RightBrace
asToken str = case mi of
        Just i -> Just (Number i)
        _ -> Nothing
    where 
        mi = (readMaybe str :: Maybe Int)

tokenize :: String -> Maybe [Token]
tokenize input = sequence $  map asToken (words input)