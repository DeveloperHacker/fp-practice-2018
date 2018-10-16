module Task4_2 where

data FourOf a = FourOf a a a a deriving(Show, Eq)

qFst (FourOf e _ _ _) = e
qSnd (FourOf _ e _ _) = e
qThd (FourOf _ _ e _) = e
qFrs (FourOf _ _ _ e) = e

-- реализуйте классы `Functor`, `Applicative` и `Monad` для типа `FourOf`
-- таким образом, что 
-- do { x <- FourOf 1 2 3 4; y <- FourOf 4 6 7 8; return $ x + y } === FourOf 5 8 10 12

instance Functor FourOf where
    fmap f (FourOf e1 e2 e3 e4) = FourOf (f e1) (f e2) (f e3) (f e4)

instance Applicative FourOf where
    pure x = FourOf x x x x
    (<*>) (FourOf f1 f2 f3 f4) (FourOf e1 e2 e3 e4) = FourOf (f1 e1) (f2 e2) (f3 e3) (f4 e4)

instance Monad FourOf where
    return = pure
    (>>=) (FourOf e1 e2 e3 e4) f = FourOf (qFst (f e1)) (qSnd (f e2)) (qThd (f e3)) (qFrs (f e4))
