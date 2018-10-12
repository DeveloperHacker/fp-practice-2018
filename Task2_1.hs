module Task2_1 where

import Todo(todo)
import Prelude hiding (lookup)

-- Ассоциативный массив на основе бинарного дерева поиска
-- Ключи - Integer, значения - произвольного типа
data TreeMap v = EmptyTreeMap | TreeMapNode Integer Integer v (TreeMap v) (TreeMap v) deriving(Show, Eq) 

-- Пустое дерево
emptyTree :: TreeMap v
emptyTree = EmptyTreeMap

-- Содержится ли заданный ключ в дереве?
contains :: TreeMap v -> Integer -> Bool
contains t k = case (t) of
    EmptyTreeMap -> False
    TreeMapNode _ key value _ _ | key == k -> True
    TreeMapNode _ key _ _ right | key < k -> contains right k
    TreeMapNode _ key _ left _ | key > k -> contains left k

-- Значение для заданного ключа
lookup :: Integer -> TreeMap v -> v
lookup k t = case (t) of
    EmptyTreeMap -> error $ concat ["Key ", show k, " not found"]
    TreeMapNode _ key value _ _ | key == k -> value
    TreeMapNode _ key _ _ right | key < k -> lookup k right
    TreeMapNode _ key _ left _ | key > k -> lookup k left

-- Вставка пары (ключ, значение) в дерево
insert :: (Integer, v) -> TreeMap v -> TreeMap v
insert (k, v) t = case (t) of
    EmptyTreeMap -> TreeMapNode 1 k v EmptyTreeMap EmptyTreeMap
    TreeMapNode _ key _ _ _ | key == k -> error $ concat ["Key ", show k, " already exists"]
    TreeMapNode size key value left right | key < k -> TreeMapNode (size + 1) key value left (insert (k, v) right)
    TreeMapNode size key value left right | key > k -> TreeMapNode (size + 1) key value (insert (k, v) left) right

-- Удаление элемента по ключу
remove :: Integer -> TreeMap v -> TreeMap v
remove k t = case (t) of
    EmptyTreeMap -> error $ concat ["Key ", show k, " not found"]
    TreeMapNode _ key _ EmptyTreeMap EmptyTreeMap | key == k -> EmptyTreeMap
    TreeMapNode _ key _ left EmptyTreeMap | key == k -> left
    TreeMapNode size key _ left right | key == k -> let 
            min t = case (t) of 
                TreeMapNode _ key _ EmptyTreeMap EmptyTreeMap -> key
                TreeMapNode _ key _ left _ -> min left
            key = min right
        in TreeMapNode (size - 1) key (lookup key right) left (remove key right)
    TreeMapNode size key value left right | key < k -> TreeMapNode (size - 1) key value left (remove k right)
    TreeMapNode size key value left right | key > k -> TreeMapNode (size - 1) key value (remove k left) right

-- Поиск ближайшего снизу ключа относительно заданного
nearestLE :: Integer -> TreeMap v -> (Integer, v)
nearestLE k t = case (t) of
    EmptyTreeMap -> error $ concat ["Key ", show k, " not found"]
    TreeMapNode _ key _ EmptyTreeMap EmptyTreeMap | key == k -> error $ concat ["Nearest for ", show k, " not found"]
    TreeMapNode _ key _ EmptyTreeMap (TreeMapNode _ rk rv _ _) | key == k -> (rk, rv)
    TreeMapNode _ key _ (TreeMapNode _ lk lv _ _) EmptyTreeMap | key == k -> (lk, lv)
    TreeMapNode _ key _ (TreeMapNode _ lk lv _ _) (TreeMapNode _ rk rv _ _) | key == k
            -> if abs (lk - k) < abs (rk - k) then (lk, lv) else (rk, rv)
    TreeMapNode _ key _ _ right | key < k -> nearestLE k right
    TreeMapNode _ key _ left _ | key > k -> nearestLE k left

-- Построение дерева из списка пар
treeFromList :: [(Integer, v)] -> TreeMap v
treeFromList lst = foldr insert EmptyTreeMap lst

-- Построение списка пар из дерева
listFromTree :: TreeMap v -> [(Integer, v)]
listFromTree t = case (t) of 
    EmptyTreeMap -> []
    TreeMapNode _ key value left right -> listFromTree left ++ [(key, value)] ++ listFromTree right

-- Количество элементов в дереве 
size :: TreeMap v -> Integer
size t = case (t) of 
    EmptyTreeMap -> 0
    TreeMapNode size _ _ _ _ -> size

-- Поиск k-той порядковой статистики дерева 
kMean :: Integer -> TreeMap v -> (Integer, v)
kMean k t = case (t) of 
    _ | k < 0 || k >= size t -> error $ concat ["K mean for ", show k, " not found"]
    TreeMapNode _ _ _ left _ | k < size left -> kMean k left
    TreeMapNode _ _ _ left right | k > size left -> kMean (k - size left - 1) right
    TreeMapNode _ key value _ _ -> (key, value)
