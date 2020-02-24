import Prelude hiding (lookup)
import qualified Data.List as L

class MapLike m where
    empty :: m k v
    lookup :: Ord k => k -> m k v -> Maybe v
    insert :: Ord k => k -> v -> m k v -> m k v
    delete :: Ord k => k -> m k v -> m k v
    fromList :: Ord k => [(k,v)] -> m k v
    fromList [] = empty
    fromList ((k,v):xs) = insert k v (fromList xs)

newtype ListMap k v = ListMap { getListMap :: [(k,v)] }
    deriving (Eq,Show)
    
instance MapLike ListMap where  
    empty = ListMap []
    lookup _ (ListMap []) = Nothing
    lookup key (ListMap ((k,v):xs))
        | k == key = Just v
        | otherwise = lookup key (ListMap xs)

    insert key newValue (ListMap []) = ListMap [(key, newValue)]
    insert key newValue (ListMap ((k,v):xs))
        | k == key = ListMap ((k,newValue):xs)
        | otherwise = let
            (ListMap t) = insert key newValue (ListMap xs)
            in ListMap ((k,v):t)

    delete key (ListMap []) = ListMap []
    delete key (ListMap (x:xs))
        | key == fst x = ListMap xs
        | otherwise = ListMap (x : ds)  where
            ListMap ds = delete key (ListMap xs)
