{-# LANGUAGE ViewPatterns #-}
module Task6 where

import Todo(todo)

data LinkedTree a = LTEmpty | LTNode (LinkedTree a) a (LinkedTree a) (LinkedTree a)

instance (Show a) => Show (LinkedTree a) where
    show tree = case tree of
        LTEmpty -> "LTEmpty"
        LTNode _ (show -> element) (show -> left) (show -> right) ->
            concat ["LTNode ", element, " (", left, ")", " (", right, ")"]

instance (Eq a) => Eq (LinkedTree a) where
    (==) tree1 tree2 = case (tree1, tree2) of
        (LTEmpty, LTEmpty) -> True
        (LTNode _ v1 l1 r1, LTNode _ v2 l2 r2) -> v1 == v2 && l1 == l2 && r1 == r2
        _ -> False

ltEmpty :: LinkedTree a
ltEmpty = LTEmpty

ltFind :: (Ord a) => LinkedTree a -> a -> Bool
ltFind tree element = case tree of
    LTEmpty -> False
    LTNode _ value _ _ | value == element -> True
    LTNode _ value _ right | value < element -> ltFind right element
    LTNode _ value left _ | value > element -> ltFind left element


ltReplaceParent :: LinkedTree a -> LinkedTree a -> LinkedTree a
ltReplaceParent parent node = case node of
    LTEmpty -> LTEmpty
    LTNode _ value left right -> node where
        left' = ltReplaceParent node left
        right' = ltReplaceParent node right
        node = LTNode parent value left' right'

ltInsert :: (Ord a, Show a) => a -> LinkedTree a -> LinkedTree a
ltInsert element tree = insert' LTEmpty tree element where
    insert' parent tree element = case tree of
        LTEmpty -> LTNode parent element LTEmpty LTEmpty
        LTNode _ value _ _ | value == element ->
            error $ concat ["Element ", show element, " already exists"]
        LTNode _ value left right | value < element -> node where
            left' = ltReplaceParent node left
            right' = insert' node right element
            node = LTNode parent value left' right'
        LTNode _ value left right | value > element -> node where
            right' = ltReplaceParent node right
            left' = insert' node left element
            node = LTNode parent value left' right'

ltRemove :: (Ord a, Show a) => LinkedTree a -> a -> LinkedTree a
ltRemove tree element = remove' LTEmpty tree element where
    remove' parent tree element = case tree of
        LTEmpty -> error $ concat ["Element ", show element, " already exists"]
        LTNode _ value LTEmpty LTEmpty | value == element -> LTEmpty
        LTNode _ value left LTEmpty | value == element -> left
        LTNode _ value left right | value == element -> node where
            value' = min right
            min (LTNode _ value LTEmpty _) = value
            min (LTNode _ _ left _) = min left
            left' = ltReplaceParent node left
            right' = remove' node right value'
            node = LTNode parent value' left' right'
        LTNode _ value left right | value < element -> node where
            left' = ltReplaceParent node left
            right' = remove' node right element
            node = LTNode parent value left' right'
        LTNode _ value left right | value > element -> node where
            right' = ltReplaceParent node right
            left' = remove' node left element
            node = LTNode parent value left' right'

fromLinkedTree :: LinkedTree a -> [a]
fromLinkedTree tree = case tree of
    LTEmpty -> []
    LTNode _ value left right -> fromLinkedTree left ++ [value] ++ fromLinkedTree right
