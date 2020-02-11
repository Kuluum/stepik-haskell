import Data.List

fibStream :: [Integer]
fibStream = 0 : zipWith (+) fibStream (1 : fibStream)

repeat2 = iterate repeatHelper
repeatHelper x = x

data Odd = Odd Integer 
    deriving (Eq, Show)

instance Enum Odd where
    succ (Odd x) = Odd $ x + 2
    pred (Odd x) = Odd $ x - 2
    toEnum x = Odd $ toInteger x * 2 + 1
    fromEnum (Odd x) = quot (fromInteger x - 1) 2
    enumFrom = iterate succ
    enumFromThen (Odd x) (Odd y) = map Odd [x, y ..]
    enumFromTo (Odd x) (Odd y) = map Odd [x, x + 2 .. y]
    enumFromThenTo (Odd x) (Odd y) (Odd z) = map Odd [x , y .. z]


coins = [2, 3, 7]

-- change :: (Ord a, Num a) => a -> [[a]]
change n
    | n == 0 = [[]]
    | otherwise = [ x:y | x <- coins, n-x >= 0, y <- change (n-x) ]

concatList :: [[a]] -> [a]
concatList = foldr (++) []

lengthList :: [a] -> Int
lengthList = foldr (\x s -> s + 1) 0

sumOdd :: [Integer] -> Integer
sumOdd = foldr (\x s -> if mod x 2 == 1 then s + x else s) 0

meanList :: [Double] -> Double
meanList xs = (/ (fromIntegral $ length xs)) $ (foldr (+) 0) xs



evenOnly :: [a] -> [a]
evenOnly = fst . foldl' (\(xs, p) x -> if mod p 2 == 0 then (xs ++ [x], p+1) else (xs, p+1)) ([], 1)

evenOnly' :: [a] -> [a]
evenOnly' xs = bar xs 1
bar [] _ = []
bar (x:xs) c = if (even c) then x:(bar xs (c+1)) else (bar xs (c+1))

lastElem :: [a] -> a
lastElem = foldl1 (\_ x -> x)

revRange :: (Char,Char) -> [Char]
revRange (a,b) = unfoldr g b
    where g = (\b' -> if b' < a then Nothing else Just (b', pred b'))