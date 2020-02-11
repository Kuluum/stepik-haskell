class Printable p where
    toString :: p -> [Char]

instance Printable Bool where
    toString True = "true"
    toString False = "false"

instance Printable () where
    toString _ = "unit type"

instance (Printable p1, Printable p2) => Printable (p1, p2) where
    toString (p1, p2) = "(" ++ toString p1 ++ "," ++ toString p2 ++ ")"