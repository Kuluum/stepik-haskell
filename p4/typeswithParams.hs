import Data.Char (isDigit, isSpace)

data Coord a = Coord a a deriving Show

distance :: Coord Double -> Coord Double -> Double
distance (Coord x1 y1) (Coord x2 y2) = sqrt ( (x1 - x2)^2 + (y1-y2)^2 ) 

manhDistance :: Coord Int -> Coord Int -> Int
manhDistance (Coord x1 y1) (Coord x2 y2) = abs (x1 - x2) + abs (y1-y2)

getCenter :: Double -> Coord Int -> Coord Double
getCenter size (Coord x y) = Coord ((fromIntegral x + 0.5) * size) ((fromIntegral y + 0.5) * size)

getCell :: Double -> Coord Double -> Coord Int
getCell size (Coord x y) = Coord (floor (x / size)) (floor (y / size))


findDigit :: [Char] -> Maybe Char
findDigit [] = Nothing
findDigit (x:xs) = if isDigit x then Just x else findDigit xs

findDigitOrX :: [Char] -> Char
findDigitOrX s = helper (findDigit s) where
    helper (Just x) = x
    helper Nothing = 'X'

maybeToList :: Maybe a -> [a]
maybeToList (Just x) = [x]
maybeToList Nothing = []

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x:xs) = Just x

data Error = ParsingError | IncompleteDataError | IncorrectDataError String deriving Show

data Person = Person { firstName :: String, lastName :: String, age :: Int } deriving Show

type PersonData = (Maybe String, Maybe String, Maybe Int)

parsePerson :: String -> Either Error Person
parsePerson str =
    case pData of
      Left error -> Left error
      Right (Just fn, Just ln, Just a) -> Right Person { firstName = fn, lastName = ln, age = a }
      Right (_, _, _) -> Left IncompleteDataError
    where
      acc = Right (Nothing, Nothing, Nothing)
      pData = foldl parsePersonData acc $ lines str



parsePersonData :: Either Error PersonData -> String -> Either Error PersonData
parsePersonData (Left e) _ = Left e
parsePersonData (Right (fn, ln, a)) line =
    case parseTuple line of
      Just ("firstName", val) -> Right (Just val, ln, a)
      Just ("lastName", val) -> Right (fn, Just val, a)
      Just ("age", val) ->
          case parseAge val of
            Left error -> Left error
            Right age -> Right (fn, ln, Just age)
      Just (_, _) -> Right (fn, ln, a)
      Nothing -> Left ParsingError


parseTuple :: String -> Maybe (String, String)
parseTuple str =
    case span (/= '=') str of
      (k, ('=':v)) -> Just (trim k, trim v)
      _ -> Nothing


parseAge :: String -> Either Error Int
parseAge str | all isDigit str = Right (read str :: Int)
             | otherwise = Left (IncorrectDataError str)


trimLeft :: String -> String
trimLeft = dropWhile isSpace

trimRight :: String -> String
trimRight = reverse . trimLeft. reverse

trim :: String -> String
trim = trimLeft . trimRight

eitherToMaybe :: Either a b -> Maybe a
eitherToMaybe (Left a) = Just a
eitherToMaybe (Right _) = Nothing