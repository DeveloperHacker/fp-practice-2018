module Task2_1 where

import Todo(todo)
import Prelude hiding (lookup)

-- Ассоциативный массив на основе бинарного дерева поиска
-- Ключи - Integer, значения - произвольного типа
data TreeMap v = EmptyTreeMap | TreeMapNode Integer v (TreeMap v) (TreeMap v) deriving(Show, Eq) 

-- Пустое дерево
emptyTree :: TreeMap v
emptyTree = EmptyTreeMap

-- Содержится ли заданный ключ в дереве?
contains :: TreeMap v -> Integer -> Bool
contains t k = case (t) of
    EmptyTreeMap -> False
    TreeMapNode key value _ _ | key == k -> True
    TreeMapNode key _ _ right | key < k -> contains right k
    TreeMapNode key _ left _ | key > k -> contains left k

-- Значение для заданного ключа
lookup :: Integer -> TreeMap v -> v
lookup k t = case (t) of
    EmptyTreeMap -> error $ concat ["Key ", show k, " not found in tree map"]
    TreeMapNode key value _ _ | key == k -> value
    TreeMapNode key _ _ right | key < k -> lookup k right
    TreeMapNode key _ left _ | key > k -> lookup k left

-- Вставка пары (ключ, значение) в дерево
insert :: (Integer, v) -> TreeMap v -> TreeMap v
insert (k, v) t = case (t) of
    EmptyTreeMap -> TreeMapNode k v EmptyTreeMap EmptyTreeMap
    TreeMapNode key _ _ _ | key == k -> error $ concat ["Key ", show k, " already exists in tree map"]
    TreeMapNode key value left right | key < k -> TreeMapNode key value left (insert (k, v) right)
    TreeMapNode key value left right | key > k -> TreeMapNode key value (insert (k, v) left) right

-- Удаление элемента по ключу
remove :: Integer -> TreeMap v -> TreeMap v
remove k t = case (t) of
    EmptyTreeMap -> error $ concat ["Key ", show k, " not found in tree map"]
    TreeMapNode key _ left right | key == k -> treeFromList $ listFromTree left ++ listFromTree right
    TreeMapNode key value left right | key < k -> TreeMapNode key value left (remove k right)
    TreeMapNode key value left right | key > k -> TreeMapNode key value (remove k left) right

-- Поиск ближайшего снизу ключа относительно заданного
nearestLE :: Integer -> TreeMap v -> (Integer, v)
nearestLE k t = case (t) of
    EmptyTreeMap -> error $ concat ["Key ", show k, " not found in tree map"]
    TreeMapNode key _ EmptyTreeMap EmptyTreeMap | key == k -> error $ concat ["Nearest for ", show k, " not found in tree map"]
    TreeMapNode key _ EmptyTreeMap (TreeMapNode rk rv _ _) | key == k -> (rk, rv)
    TreeMapNode key _ (TreeMapNode lk lv _ _) EmptyTreeMap | key == k -> (lk, lv)
    TreeMapNode key _ (TreeMapNode lk lv _ _) (TreeMapNode rk rv _ _) | key == k -> if abs (lk - k) < abs (rk - k) then (lk, lv) else (rk, rv)
    TreeMapNode key _ _ right | key < k -> nearestLE k right
    TreeMapNode key _ left _ | key > k -> nearestLE k left

-- Построение дерева из списка пар
treeFromList :: [(Integer, v)] -> TreeMap v
treeFromList lst = foldr insert EmptyTreeMap lst

-- Построение списка пар из дерева
listFromTree :: TreeMap v -> [(Integer, v)]
listFromTree t = case (t) of 
    EmptyTreeMap -> []
    TreeMapNode key value left right -> listFromTree left ++ [(key, value)] ++ listFromTree right

-- Поиск k-той порядковой статистики дерева 
kMean :: Integer -> TreeMap v -> (Integer, v)
kMean i t = (listFromTree t) !! (fromIntegral i)
