module Task3_2 where

import Todo(todo)

data ReverseList a = RNil | RCons (ReverseList a) a


rlistToList :: ReverseList a -> [a]
rlistToList lst = foldl f [] lst where f t h = h:t

listToRList :: [a] -> ReverseList a
listToRList lst = foldl f RNil lst where f t h = RCons t h

-- Реализуйте классы Eq, Ord, Show, Monoid, Functor

instance (Eq a) => Eq (ReverseList a) where
    left == right = case (left, right) of
        (RNil, RNil) -> True
        (RCons lt lh, RCons rt rh) -> lh == rh && lt == rt 
        _ -> False

instance (Ord a) => Ord (ReverseList a) where
    left <= right = case (left, right) of
        (RNil, RNil) -> True
        (RNil, RCons _ _) -> True
        (RCons _ _, RNil) -> False
        (RCons lt lh, RCons rt rh) | lh == rh -> lt <= rt
        (RCons lt lh, RCons rt rh) | lh < rh -> True
        _ -> False

instance (Show a) => Show (ReverseList a) where
    show list = case list of 
        RNil -> "RNil"
        RCons t h -> concat ["RCons (", show t, ") ", show h]

instance Foldable ReverseList where
    foldr merge acc lst = case lst of 
        RNil -> acc
        RCons t h -> merge h (foldr merge acc t) 

instance Semigroup (ReverseList a) where
    left <> right = foldr (\e acc -> RCons acc e) right left

instance Monoid (ReverseList a) where
    mempty = RNil

instance Functor ReverseList where
    fmap transform lst = case lst of
        RNil -> RNil
        RCons t h -> RCons (fmap transform t) (transform h)
