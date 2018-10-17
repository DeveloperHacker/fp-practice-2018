module Task5_1 where

import Todo(todo)

data DList a = DNil 
             | DCons { 
                left :: (DList a), 
                current :: a, 
                right :: (DList a) 
             }

instance (Show a) => Show (DList a) where
    show it = "[" ++ showBody it ++ "]"
              where showBody DNil = ""
                    showBody (DCons _ h DNil) = show h
                    showBody (DCons _ h t) = show h ++ ", " ++ showBody t

instance (Eq a) => Eq (DList a) where
    DNil == DNil = True
    (DCons _ h1 t1) == (DCons _ h2 t2) = h1 == h2 && t1 == t2
    _ == _ = False

list2dlist :: [a] -> DList a
list2dlist lst = list2dlist' DNil lst

list2dlist' :: DList a -> [a] -> DList a
list2dlist' _ [] = DNil
list2dlist' left (h: t) = 
    let rec = DCons left h (list2dlist' rec t)
    in rec

dlist2list :: DList a -> [a]
dlist2list DNil = []
dlist2list (DCons _ value right) = (value: dlist2list right)

index :: DList a -> Int -> a
index list i = case list of
    DNil -> error "IndexOfBoundException"
    DCons _ v _ | i == 0 -> v
    DCons _ _ right -> index right $ i - 1

insertAt :: DList a -> Int -> a -> DList a
insertAt list index value = insertAt' DNil list index value where 
    insertAt' left DNil 0 value = DCons left value DNil
    insertAt' left DNil index value = DNil
    insertAt' left right 0 value = rec where 
        rec = DCons left value $ insertAt' rec right (-1) value
    insertAt' left (DCons _ v right) index value = rec where 
        rec = DCons left v $ insertAt' rec right (index - 1) value

removeAt :: DList a -> Int -> DList a
removeAt list index = removeAt' DNil list index where
    removeAt' left DNil index = DNil
    removeAt' left (DCons _ v right) 0 = removeAt' left right (-1)
    removeAt' left (DCons _ v right) index = rec where 
        rec = DCons left v $ removeAt' rec right (index - 1)
