addTwoElements :: a -> a -> [a] -> [a]
addTwoElements a1 a2 l = a1 : a2 : l 

nTimes:: a -> Int -> [a]
nTimes a1 l | l <= 0 = []
            | l == 1 = [a1]
            | otherwise = a1 : (nTimes a1 (l-1))


oddsOnly :: Integral a => [a] -> [a]
oddsOnly [] = []
oddsOnly (x:xs) | odd x = x : oddsOnly xs
                | otherwise = oddsOnly xs
            

isPalindrome :: Eq a => [a] -> Bool
isPalindrome x = x == reverse x

groupElems :: Eq a => [a] -> [[a]]
groupElems elems = groupHelper [] [] elems

groupHelper :: Eq a => [[a]] -> [a] -> [a] -> [[a]]
groupHelper group [] [] = group
groupHelper group acc [] = group ++ [acc]
groupHelper group [] (x:rest) = groupHelper group [x] rest
groupHelper group acc (x:rest) | x == last acc  = groupHelper group (x:acc) rest
                               | otherwise = groupHelper (group ++ [acc]) [x] rest