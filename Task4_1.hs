module Task4_1 where

-- Монада над функцией. В качестве входного значения `fun` может быть что угодно
-- Собственно, почему бы не `String`?
data FunMonad a = FunMonad { fun :: String -> a }

-- реализуйте классы `Functor`, `Applicative` и `Monad` для типа `FunMonad`

instance Functor FunMonad where
    fmap transform f = FunMonad (transform . (fun f))

instance Applicative FunMonad where
    pure x = FunMonad (\s -> x)
    (<*>) f1 f2 = FunMonad (\s -> fun f1 s $ fun f2 s)

instance Monad FunMonad where
    return = pure
    (>>=) f1 f2 = FunMonad (\s -> fun (f2 $ fun f1 s) s)
