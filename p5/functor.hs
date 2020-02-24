data Point3D a = Point3D a a a deriving Show

instance Functor Point3D where
    fmap f (Point3D x y z) = Point3D (f x) (f y) (f z) 


data GeomPrimitive a = Point (Point3D a) | LineSegment (Point3D a) (Point3D a)

instance Functor GeomPrimitive where
    fmap f (Point x) = Point (fmap f x)
    fmap f (LineSegment x y) = LineSegment (fmap f x) (fmap f y)


data Tree a = Leaf (Maybe a) | Branch (Tree a) (Maybe a) (Tree a) deriving Show

instance Functor Tree where
    fmap _ (Leaf Nothing) = Leaf Nothing
    fmap f (Leaf (Just a)) = Leaf (Just (f a))
    fmap f (Branch t1 Nothing t2) = Branch (f <$> t1) Nothing (f <$> t2)
    fmap f (Branch t1 (Just x) t2) = Branch (f <$> t1) (Just (f x)) (f <$> t2)


data Entry k1 k2 v = Entry (k1, k2) v  deriving Show
data Map k1 k2 v = Map [Entry k1 k2 v]  deriving Show

instance Functor (Entry k1 k2) where
    fmap f (Entry p v) = Entry p (f v)

instance Functor (Map k1 k2) where
    fmap f (Map es) = Map (map (fmap f) es)