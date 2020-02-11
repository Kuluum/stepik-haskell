import Data.List 
import Data.Char

readDigits :: String -> (String, String)
readDigits s = span isDigit s

filterDisj :: (a -> Bool) -> (a -> Bool) -> [a] -> [a]
filterDisj _ _ [] = []
filterDisj p1 p2 (x:xs)
    | (p1 x) || (p2 x) = x : filterDisj p1 p2 xs
    | otherwise = filterDisj p1 p2 xs


pairAppend:: ([a], [a]) -> ([a], [a]) -> ([a], [a])
pairAppend p1 p2 = (fst p1 ++ fst p2, snd p1 ++ snd p2)

filterSpan :: (a -> Bool) -> [a] -> ([a], [a])
filterSpan _ [] = ([], [])
filterSpan p (x:xs)
    | p x = pairAppend ([x], []) (filterSpan p xs)
    | otherwise = pairAppend ([], [x]) (filterSpan p xs)

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = (qsort $ fst fs) ++ [x] ++ (qsort $ snd fs)
    where fs = filterSpan (< x) xs

squares'n'cubes :: Num a => [a] -> [a]
squares'n'cubes [] = []
squares'n'cubes (x:xs) = [x ^ 2, x ^ 3] ++ squares'n'cubes xs 
    

perms :: [a] -> [[a]]
perms [] = [[]]
perms xs = concatMap (\(y, ys) -> map ([y] ++) (perms ys)) elemWithRest
    where elemWithRest = (permsHelper xs [])
    
permsHelper :: [a] -> [a] -> [(a, [a])]
permsHelper [] _ = []
permsHelper [x] [] = [(x, [])]
permsHelper (x:xs) ys = [(x, ys ++ xs)] ++ permsHelper xs (ys ++ [x])

delAllUpper :: String -> String
delAllUpper = unwords . filter (any isLower) . words

max3 :: Ord a => [a] -> [a] -> [a] -> [a]
max3 = zipWith3 (\a b c -> max a $ max b c)