data Point = Point Double Double

origin :: Point
origin = Point 0.0 0.0

distanceToOrigin :: Point -> Double
distanceToOrigin (Point x y) = sqrt (x ^ 2 + y ^ 2)

distance :: Point -> Point -> Double
distance (Point x1 y1) (Point x2 y2) = sqrt (  (x1-x2)^2 + (y1 - y2)^2 ) 

data Shape = Circle Double | Rectangle Double Double

area :: Shape -> Double
area (Circle r) = pi * r^2
area (Rectangle x y) = x * y

data Result' = Success' | Fail' Int
instance Show Result' where
    show Success' = "Success"
    show (Fail' n) = "Fail: " ++ show n

-- doSomeWork' :: SomeData -> Result'
-- doSomeWork' some = cxse doSomeWork some of
--     (_, 0) -> Success'
--     (_, n) -> Fail' n


square :: Double -> Shape
square a = Rectangle a a

isSquare :: Shape -> Bool
isSquare (Circle _) = False
isSquare (Rectangle a b) = a == b 

data Bit = Zero | One deriving Eq
data Sign = Minus | Plus deriving Eq
data Z = Z Sign [Bit] deriving (Show, Eq)

instance Show Bit where
    show Zero = "0"
    show One = "1"

instance Show Sign where
    show Minus = "-"
    show Plus = "+"


add :: Z -> Z -> Z
add (Z Plus a) (Z Plus b) = Z Plus (addSameHelper a b Zero)
add (Z Minus a) (Z Minus b) = Z Minus (addSameHelper a b Zero)
add (Z Plus a) (Z Minus b) = plusMinusDiffHelper a b
add za@(Z Minus _) zb@(Z Plus _) = add zb za

mul :: Z -> Z -> Z
mul (Z Plus a) (Z s b) = if res == [] then Z Plus [] else Z s res where res = (mulHelper a b)
mul (Z Minus a) (Z s b) = if res == [] then Z Plus [] else Z (snot s) res where res = (mulHelper a b)


addSameHelper :: [Bit] -> [Bit] -> Bit -> [Bit]
addSameHelper [] [] Zero = []
addSameHelper [] [] One = [One]

addSameHelper [] (Zero:ys) i = i : (addSameHelper [] ys Zero) 
addSameHelper [] (One:ys) One = Zero : (addSameHelper [] ys One) 
addSameHelper [] (One:ys) Zero = One : (addSameHelper [] ys Zero)

addSameHelper (Zero:xs) [] i = i : (addSameHelper xs [] Zero) 
addSameHelper (One:xs) [] One = Zero : (addSameHelper xs [] One) 
addSameHelper (One:xs) [] Zero = One : (addSameHelper xs [] Zero)

addSameHelper (Zero:xs) (Zero:ys) i = i : (addSameHelper xs ys Zero)
addSameHelper (One:xs)  (Zero:ys)  One =  Zero : (addSameHelper xs ys One)
addSameHelper (Zero:xs) (One:ys) One = Zero : (addSameHelper xs ys One)
addSameHelper (One:xs)  (Zero:ys)  Zero =  One : (addSameHelper xs ys Zero)
addSameHelper (Zero:xs) (One:ys) Zero = One : (addSameHelper xs ys Zero)
addSameHelper (One:xs)  (One:ys) i = i : (addSameHelper xs ys One)


-- https://www.math-only-math.com/subtraction-by-2s-complement.html
plusMinusDiffHelper :: [Bit] -> [Bit] -> Z
plusMinusDiffHelper pl mn = if length cs > length mn then Z Plus (removeZeros $ init cs) else Z Minus (removeZeros $ addSameHelper [] (negComplement cs) One)
    where 
        cs = addSameHelper pl (negComplement amn) One
        amn = appendZeros mn (length pl)
    
appendZeros :: [Bit] -> Int -> [Bit]
appendZeros xs len = if length xs >= len then xs else xs ++ (take (len - (length xs)) (repeat Zero))

bnot :: Bit -> Bit
bnot One = Zero
bnot Zero = One

negComplement :: [Bit] -> [Bit]
negComplement = map bnot

removeZeros :: [Bit] -> [Bit]
removeZeros [] = []
removeZeros xs = if (last xs) == Zero then removeZeros (init xs) else xs

mulHelper :: [Bit] -> [Bit] -> [Bit]
mulHelper [] _ = []
mulHelper _ [] = []
mulHelper (Zero:xs) ys = addSameHelper [] nxt Zero where nxt = [Zero] ++ (mulHelper xs ys)
mulHelper (One:xs) ys = addSameHelper ys nxt Zero where nxt = [Zero] ++ (mulHelper xs ys)

snot :: Sign -> Sign
snot Plus = Minus
snot Minus = Plus

test001 = (add (Z Plus []) (Z Plus [])) == Z Plus []
test002 = (add (Z Plus []) (Z Plus [One])) == Z Plus [One]
test003 = (add (Z Plus []) (Z Minus [One])) == Z Minus [One]

test011 = (add (Z Plus [Zero, One, One]) (Z Plus [One])) == Z Plus [One, One, One]
test012 = (add (Z Plus [Zero, One, One]) (Z Plus [Zero, One])) == Z Plus [Zero, Zero, Zero, One]
test013 = (add (Z Plus [Zero, One, One]) (Z Plus [Zero, One, One])) == Z Plus [Zero, Zero, One, One]

test021 = (add (Z Minus [Zero, One, One]) (Z Minus [One])) == Z Minus [One, One, One]
test022 = (add (Z Minus [Zero, One, One]) (Z Minus [Zero, One])) == Z Minus [Zero, Zero, Zero, One]
test023 = (add (Z Minus [Zero, One, One]) (Z Minus [Zero, One, One])) == Z Minus [Zero, Zero, One, One]

test031 = (add (Z Minus [Zero, One, One]) (Z Plus [One])) == Z Minus [One, Zero, One]
test032 = (add (Z Minus [Zero, One, One]) (Z Plus [Zero, One])) == Z Minus [Zero, Zero, One]
test033 = (add (Z Minus [Zero, One, One]) (Z Plus [Zero, One, One])) == Z Plus []

test041 = (add (Z Plus [Zero, One, One]) (Z Minus [One])) == Z Plus [One, Zero, One]
test042 = (add (Z Plus [Zero, One, One]) (Z Minus [Zero, One])) == Z Plus [Zero, Zero, One]
test043 = (add (Z Plus [Zero, One, One]) (Z Minus [Zero, One, One])) == Z Plus []

test051 = (add (Z Plus [One]) (Z Minus [One])) == Z Plus []
test052 = (add (Z Plus [One]) (Z Minus [One, One])) == Z Minus [Zero, One]
test053 = (add (Z Plus [One]) (Z Minus [Zero, One])) == Z Minus [One]
test054 = (add (Z Plus [One]) (Z Minus [Zero, Zero, Zero, One])) == Z Minus [One, One, One]
test055 = (add (Z Plus [One]) (Z Minus [Zero, One, Zero, One])) == Z Minus [One, Zero, Zero, One]
test056 = (add (Z Plus [Zero, One]) (Z Minus [Zero, One, One])) == Z Minus [Zero, Zero, One]
test057 = (add (Z Plus [Zero, One]) (Z Minus [Zero, Zero, One])) == Z Minus [Zero, One]
test058 = (add (Z Plus [One, Zero, One]) (Z Minus [Zero, One, Zero, One])) == Z Minus [One, Zero, One]

testAdd = test001 && test002 && test003 && test011 && test012 && test013 && test021 && test022 && test023 && test031 && test032 && test033 && test041 && test042 && test043 && test051 && test052 && test053 && test054 && test055 && test056 && test057 && test058

test101 = (mul (Z Plus []) (Z Plus [])) == Z Plus []
test102 = (mul (Z Plus []) (Z Plus [One])) == Z Plus []
test103 = (mul (Z Plus []) (Z Minus [One])) == Z Plus []
test104 = (mul (Z Plus [One]) (Z Plus [])) == Z Plus []
test105 = (mul (Z Minus [One]) (Z Plus [])) == Z Plus []

test111 = (mul (Z Plus [One]) (Z Plus [One])) == Z Plus [One]
test112 = (mul (Z Minus [One]) (Z Plus [One])) == Z Minus [One]
test113 = (mul (Z Plus [One]) (Z Minus [One])) == Z Minus [One]
test114 = (mul (Z Minus [One]) (Z Minus [One])) == Z Plus [One]

test121 = (mul (Z Plus [One]) (Z Plus [Zero, One])) == Z Plus [Zero, One]
test122 = (mul (Z Plus [Zero, Zero, One]) (Z Plus [Zero, Zero, One])) == Z Plus [Zero, Zero, Zero, Zero, One]

test131 = (mul (Z Plus [One, Zero, One, Zero, One]) (Z Plus [One, One, One])) == Z Plus [One, One, Zero, Zero, One, Zero, Zero, One]

testMul = test101 && test102 && test103 && test104 && test105 && test111 && test112 && test113 && test114 && test121 && test122 && test131


foo :: Bool -> Int
foo ~True = 1
foo False = 0