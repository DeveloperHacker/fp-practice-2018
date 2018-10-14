module Task2_2 where

import Todo(todo)

import Prelude hiding (foldl, foldr, unfoldr, map, concatMap, 
    filter, maxBy, minBy, reverse, sum, product, elem)

foldl :: (b -> a -> b) -> b -> [a] -> b
foldl merge acc lst = case lst of 
    [] -> acc
    (h: t) -> foldl merge (merge acc h) t

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr merge acc lst = case lst of 
    [] -> acc
    (h: t) -> merge h (foldr merge acc t) 

unfoldr :: (b -> Maybe (a, b)) -> b -> [a]
unfoldr generate dis = case generate dis of
    Nothing -> []
    Just (element, dis) -> (element : unfoldr generate dis)

-- Сумма всех элементов списка (пример)
sum :: [Integer] -> Integer
sum lst = foldl (+) 0 lst

-- Переворот списка (Пример)
reverse :: [a] -> [a]
reverse lst = foldl f [] lst where f t h = h:t

-- Отображение элементов списка
map :: (a -> b) -> [a] -> [b]
map transform lst = unfoldr f lst where 
    f t = case t of
        [] -> Nothing
        (h: t) -> Just (transform h, t)

-- Произведение всех элементов списка
product :: [Integer] -> Integer
product lst = foldl (*) 1 lst

-- Выделение из списка Maybe всех существующих значений
catMaybes :: [Maybe a] -> [a]
catMaybes lst = foldr f [] lst where 
    f e acc = case e of
        Nothing -> acc
        Just v -> (v: acc)

-- Диагональ матрицы
diagonal :: [[a]] -> [a]
diagonal mat = reverse $ foldl f [] mat where 
    f acc e = if length e <= length acc then acc else (e !! length acc: acc)

-- Фильтр для всех элементов, не соответствующих предикату
filterNot :: (a -> Bool) -> [a] -> [a]
filterNot prd lst = foldr f [] lst where 
    f e acc = if prd e then acc else (e: acc)

-- Поиск элемента в списке
elem :: (Eq a) => a -> [a] -> Bool
elem e lst = case filterNot (\b -> e /= b) lst of
    [] -> False
    _ -> True

-- Список чисел в диапазоне [from, to) с шагом step
rangeTo :: Integer -> Integer -> Integer -> [Integer]
rangeTo from to step = unfoldr f from where 
    f dis = if dis < to then Just (dis, dis + step) else Nothing

-- Конкатенация двух списков
append :: [a] -> [a] -> [a]
append left right = foldr (\e acc -> (e: acc)) right left

-- Разбиение списка lst на куски размером n
-- (последний кусок может быть меньше)
groups :: [a] -> Integer -> [[a]]
groups lst n = map (\e -> reverse e) $ catMaybes $ unfoldr f (0, [], lst) where 
    f (cnt, hs, lst) = case lst of
        _ | cnt == -1 -> Nothing
        [] -> Just (Just hs, (-1, [], []))
        (h: t) | cnt == n -> Just (Just hs, (1, [h], t))
        (h: t) -> Just (Nothing, (cnt + 1, (h: hs), t))

