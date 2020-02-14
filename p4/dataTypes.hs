data Color = Red | Green | Blue

instance Show Color where
    show Red = "Red"
    show Green = "Green"
    show Blue = "Blue"

charToInt :: Char -> Int
charToInt '0' = 0
charToInt '1' = 1
charToInt '2' = 2
charToInt '3' = 3
charToInt '4' = 4
charToInt '5' = 5
charToInt '6' = 6
charToInt '7' = 7
charToInt '8' = 8
charToInt '9' = 9

stringToColor :: String -> Color
stringToColor "Red" = Red
stringToColor "Green" = Green
stringToColor "Blue" = Blue

data LogLevel = Error | Warning | Info
cmp :: LogLevel -> LogLevel -> Ordering
cmp Error Error = EQ
cmp Error _ = GT
cmp _ Error = LT
cmp Warning Warning = EQ
cmp Warning Info = GT
cmp Info Warning = LT
cmp Info Info = EQ

data Result = Fail | Success

-- doSomeWork :: SomeData -> (Result,Int)

doSomeWork d = (Fail, 2)

-- processData :: SomeData -> String
processData d = 
    case doSomeWork d of
        (Success, _) -> "Success"
        (Fail, n) -> "Fail: " ++ show n