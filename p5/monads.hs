import Control.Monad (liftM, ap)

data Log a = Log [String] a deriving Show

toLogger :: (a -> b) -> String -> (a -> Log b)
toLogger f msg = (Log [msg]) . f

execLoggers :: a -> (a -> Log b) -> (b -> Log c) -> Log c
execLoggers x f g =  Log (s1 ++ s2) c'
    where 
        (Log s2 c') = g b'
        (Log s1 b') = f x


returnLog :: a -> Log a
returnLog = Log []


add1Log = toLogger (+1) "added one"
mult2Log = toLogger (* 2) "multiplied by 2"


bindLog :: Log a -> (a -> Log b) -> Log b
bindLog (Log sa a) f = Log (sa ++ sb) b
    where (Log sb b) = f a


instance Functor Log where
  fmap = liftM

instance Applicative Log where
  pure = return
  (<*>) = ap

instance Monad Log where
    return = returnLog
    (>>=) = bindLog

execLoggersList :: a -> [a -> Log a] -> Log a
execLoggersList elem list = foldl bindLog (returnLog elem) list 