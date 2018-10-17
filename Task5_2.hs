module Task5_2 where

import Todo(todo)
import Data.Ratio

data Stream a = Cons {
                    shead :: a,
                    stail :: Stream a
                }

srepeat :: a -> Stream a
srepeat x =
    let rec = Cons x rec in
    rec

generate :: a -> (a -> a) -> Stream a
generate x f =
    Cons x $ generate (f x) f

instance Functor Stream where
    fmap f (Cons h t) = Cons (f h) (fmap f t)

diag (Cons h t) = Cons (shead h) $ diag (stail <$> t)
sflatten = diag

instance Applicative Stream where
    pure x = srepeat x
    f <*> x = do { f' <- f ; x' <- x ; return $ f' x' }

instance Monad Stream where
    return x = srepeat x
    ss >>= f = sflatten (f <$> ss)

sTake :: Int -> Stream a -> [a]
sTake 0 _ = []
sTake n stream = (shead stream: sTake (n - 1) (stail stream))

sLookup :: Int -> Stream a -> a
sLookup 0 stream = shead stream
sLookup i stream = sLookup (i - 1) (stail stream)

sinPrecisions :: Double -> Stream Double
sinPrecisions x = enumGenerate 0 (shift x) 0 component where
    shift x = x - 2 * pi * (realToFrac $ ceiling $ x / (2 * pi))
    enumGenerate n x y f = Cons y' $ enumGenerate (n + 1) x y' f where y' = f n x y
    component n x y = y + (-1)^n * (x ^ (2 * n + 1)) / (fromIntegral (factorial (2 * n + 1)))
    factorial n = product [1..n]

ePrecisions :: Stream Rational
ePrecisions = enumGenerate 0 0 component where
    enumGenerate n y f = Cons y' $ enumGenerate (n + 1) y' f where y' = f n y
    component n y = y + 1 / (fromIntegral (factorial n))
    factorial n = product [1..n]
