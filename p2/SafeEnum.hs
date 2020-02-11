class (Enum a, Bounded a, Eq a) => SafeEnum a where
    ssucc :: a -> a
    ssucc s | s == maxBound  = minBound
            | otherwise = succ s  
  
    spred :: a -> a
    spred s | s == minBound = maxBound
            | otherwise = pred s  