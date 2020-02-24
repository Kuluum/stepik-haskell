
data List a = Nil | Cons a (List a)

fromList :: List a -> [a]
fromList Nil = []
fromList (Cons x xs) = x : fromList xs

toList :: [a] -> List a
toList [] = Nil
toList (x:xs) = Cons x (toList xs)



data Nat = Zero | Suc Nat deriving Show

toNat :: Integer -> Nat
toNat 0 = Zero
toNat a = add (Suc Zero) (toNat (a-1))

fromNat :: Nat -> Integer
fromNat Zero = 0
fromNat (Suc n) = fromNat n + 1

add :: Nat -> Nat -> Nat
add Zero y = y
add x Zero = x
add x (Suc b) = add (Suc x) b

mul :: Nat -> Nat -> Nat
mul Zero _ = Zero
mul _ Zero = Zero
mul x (Suc Zero) = x
mul (Suc Zero) y = y
mul x (Suc b) = add x (mul x b)

fac :: Nat -> Nat
fac Zero = (Suc Zero)
fac (Suc x) = mul (fac x) (Suc x)


data Tree a = Leaf a | Node (Tree a) (Tree a)

height :: Tree a -> Int
height (Leaf _) = 0
height (Node t1 t2) = 1 + max (height t1) (height t2)

size :: Tree a -> Int
size (Leaf _) = 1
size (Node t1 t2) = 1 + (size t1) + (size t2)

avg :: Tree Int -> Int
avg t =
    let (c,s) = go t
    in s `div` c
  where
    go :: Tree Int -> (Int,Int)
    go (Leaf a) = (1, a)
    go (Node t1 t2) = ((fst gt1) + (fst gt2), (snd gt1) + (snd gt2))
        where 
            gt1 = go t1
            gt2 = go t2


infixl 6 :+:
infixl 7 :*:
data Expr = Val Int | Expr :+: Expr | Expr :*: Expr
    deriving (Show, Eq)

expand :: Expr -> Expr
expand ((e1 :+: e2) :*: e) = expand (expand e1 :*: expand e) :+: expand (expand e2 :*: expand e)
expand (e :*: (e1 :+: e2)) = expand (expand e :*: expand e1) :+: expand (expand e :*: expand e2)
expand (e1 :+: e2) = expand e1 :+: expand e2
expand (e1 :*: e2) = let
    ee1 = expand e1
    ee2 = expand e2
  in if ee1 == e1 && ee2 == e2 then e1 :*: e2 else expand $ ee1 :*: ee2
expand e = e


newtype Xor = Xor { getXor :: Bool }
    deriving (Eq,Show)

instance Semigroup Xor where
    Xor a <> Xor b = Xor (a /= b)

instance Monoid Xor where
    mempty = Xor False

-- For old haskell/stepik
-- instance Monoid Xor where
--     mempty = Xor False
--     mappend (Xor a) (Xor b) = Xor (a /= b)
    
newtype Maybe' a = Maybe' { getMaybe :: Maybe a }
    deriving (Eq,Show)

instance Semigroup a => Semigroup (Maybe' a) where
    Maybe' Nothing <> _ = Maybe' Nothing
    _ <> Maybe' Nothing = Maybe' Nothing
    Maybe' l <> Maybe' r = Maybe' (l <> r)

instance Monoid a => Monoid (Maybe' a) where
    mempty = Maybe' $ Just mempty  

-- For old
-- instance Monoid a => Monoid (Maybe' a) where
--   mempty = Maybe' $ Just mempty
--   mappend (Maybe' Nothing) _ = Maybe' Nothing
--   mappend _ (Maybe' Nothing) = Maybe' Nothing
--   mappend (Maybe' a) (Maybe' b) = Maybe' (mappend a b)



