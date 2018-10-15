module Task3_3 where

-- Реализуйте классы Monoid и Functor
-- Объясните в комментариях, почему они реализованы именно так


data Inversable a b = Inversable { forward :: a -> b, backward :: b -> a }

reverse :: Inversable a b -> Inversable b a
reverse attitude = Inversable (backward attitude) (forward attitude)

class Mapper f where
    mmap :: Inversable a b -> f a -> f b

-- Mapper --- это Functor работающий только с обратимыми функциями
-- Без обратной функции невозможно сделать реализовать функтор, так как 
-- имея две функции a -> Bool и a -> b нужно получить функцию b -> a
-- Но это не возможно сделатть без функции из b -> a.
-- Для докозательства данного факта используем аппарат алгебры логики.
-- Сделаем предположение (a):
-- (a)  Если существует a, то можно создать Bool и если существует a, то можно создать b,
--      то истинно, что если существует b, то можно создать Bool.
-- Переведём предположение (a) к формальному виду (b):
-- (b)  (((a → Bool) ∧ (a → b)) → (b → Bool))
-- Если высказывание (b) является тавтологией, то предположение (a) является тавтологией
-- Для выяснения этого факта построим таблицу истинности:
--   a   b  Bool (((a → Bool) ∧ (a → b)) → (b → Bool))
--   F   F   F                    T
--   F   F   T                    T
--   F   T   F                    F
--   F   T   T                    T
--   T   F   F                    T
--   T   F   T                    T
--   T   T   F                    T
--   T   T   T                    T
-- Так как есть ложный пример, то данное высказывание не является тавтологией
-- Данным способом можно проверить правильность предположения (с):
-- (c)  Если существует a, то можно создать Bool и если существует b, то можно создать a,
--      то истинно, что если существует b, то можно создать Bool.
-- (d)  (((a → Bool) ∧ (b → a)) → (b → Bool))
--   a   b  Bool (((a → Bool) ∧ (b → a)) → (b → Bool))
--   F   F   F                    T
--   F   F   T                    T
--   F   T   F                    T
--   F   T   T                    T
--   T   F   F                    T
--   T   F   T                    T
--   T   T   F                    T
--   T   T   T                    T
-- Следовательно предположение (c) верно
--                                                              ч.т.д

newtype PUnionSet a = PUnionSet{ unionContains :: (a -> Bool) }

instance Semigroup (PUnionSet a) where
    (PUnionSet left) <> (PUnionSet right) = PUnionSet (\e -> (left e) || (right e))

instance Monoid (PUnionSet a) where
    mempty = PUnionSet (\e -> False)

instance Mapper PUnionSet where
    mmap transform set = PUnionSet ((unionContains set) . (backward transform))


newtype PIntersectionSet a = PIntersectionSet{ intersectionContains :: (a -> Bool) }

instance Semigroup (PIntersectionSet a) where
    (PIntersectionSet left) <> (PIntersectionSet right) = PIntersectionSet (\e -> (left e) && (right e))

instance Monoid (PIntersectionSet a) where
    mempty = PIntersectionSet (\e -> True)

instance Mapper PIntersectionSet where
    mmap transform set = PIntersectionSet ((intersectionContains set) . (backward transform))


newtype PPlusSet a = PPlusSet{ plusContains :: (a -> Bool) }

instance Semigroup (PPlusSet a) where
    (PPlusSet left) <> (PPlusSet right) = PPlusSet (\e -> (left e) /= (right e))

instance Monoid (PPlusSet a) where
    mempty = PPlusSet (\e -> False)

instance Mapper PPlusSet where
    mmap transform set = PPlusSet ((plusContains set) . (backward transform))
