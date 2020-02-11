import Data.Char

sign x = if x == 0 then 0 else (if x > 0 then 1 else -1)

infixl 6 *+*
a *+* b = a^2 + b^2

infixl 6 |-|
a |-| b = abs (a-b)



twoDigits2Int :: Char -> Char -> Int
twoDigits2Int x y = if (isDigit x && isDigit y) then 10 * digitToInt x + digitToInt y else 100

dist :: (Double, Double) -> (Double, Double) -> Double
dist p1 p2 = sqrt $ (fst p1 - fst p2)^2 + (snd p1 - snd p2)^2

factorial 0 = 1
factorial n = n * factorial (n-1)


doubleFact :: Integer -> Integer
doubleFact 1 = 1
doubleFact 2 = 2
doubleFact n = n * doubleFact (n-2)


fibonacci 0 = 0
fibonacci (-1) = 1
fibonacci (-2) = -1
fibonacci 1 = 1
fibonacci n | n > 0 = fibonacci (n - 1) + fibonacci (n - 2)
            | n < 0 = - fibonacci (n + 1) + fibonacci (n + 2)

            
fibfast :: Integer -> Integer

fibfast 0 = 0
fibfast n | n >= 0 = fib' 0 1 n
          | otherwise = fib' 0 1 (abs n) * (if even (abs n) then -1 else 1)

fib' a b n | n <= 1 = b
           | otherwise = fib' b (a+b) (n-1)



seqA 0 = 1
seqA 1 = 2
seqA 2 = 3
seqA n = seqA' 1 2 3 n

seqA' a1 a2 a3 n | n <= 2 = a3
                 | otherwise = seqA' a2 a3 (a3 + a2 - 2 * a1 ) (n-1)


--sumncount :: Integer -> (Integer, Integer)
sumncount x = (a1, a2) where
    a1 = sum $ digs (abs x)
    a2 = length $ digs (abs x)
    digs :: Integer -> [Integer]
    digs 0 = [0]
    digs x = digs (x `div` 10) ++ [x `mod` 10]
                 
digs1 :: Integer -> [Integer]
digs1 0 = []
digs1 x = digs1 (x `div` 10) ++ [x `mod` 10]

range :: (Double, Double, Double) -> [Double]
range (x, y, step) | (x >= y) = [x]
                   | otherwise = range (x, (y - step), step) ++ [y]

integration :: (Double -> Double) -> Double -> Double -> Double
integration f a b | (a == b) = 0
                  | a < b = integration' f a b
                  | a > b = - integration' f b a

integration' f a b =  step * ( h/2 + l/2 + m) where
        h = f a
        l = f b
        step = (b-a) / (fromIntegral 10000)
        m = sum $ map f (init $ tail $ range (a, b, step))


f a = logBase 2 a
g a = a ** 3
h a = max a 42