module Main where

import Prelude hiding (sin, cos, gcd, lookup, foldl, foldr, unfoldr, map, concatMap,
    filter, maxBy, minBy, reverse, sum, product, elem)
import Task1_1 (Term(IntConstant, Variable))
import Task1_1 (Term, (|+|), (|-|), (|*|), replaceVar, evaluate)
import Task1_2 (sin, cos, pow, isPrime, gcd)
import Task2_1
import Task2_2
import Task3_1
import Task3_2
import Task3_3
import Task4_1
import Task4_2
import Task5_1
import Task5_2
import Task6

assertEquals actual expected | actual == expected = putStr ""
assertEquals actual expected = error $ concat ["expected: ", (show expected), " but was: ", (show actual)]

assertEqualsEps eps actual expected | expected - eps < actual && actual < expected + eps = putStr ""
assertEqualsEps eps actual expected = error $ concat ["expected: ", (show expected), " ± ", show eps ," but was: ", (show actual)]

assertTrue False = error "assertion failed!"
assertTrue _     = putStr ""

test1_1 =
    let
        expr1 = Variable "a" |+| Variable "b" |*| Variable "c"
        expr2 = Variable "a" |-| Variable "b" |-| Variable "c"
        expr3 = Variable "a" |-| Variable "b" |*| IntConstant 10 |-| Variable "c"
        expr4 = expr1 |*| expr2 |+| expr3
        value1 = replaceVar "a" (IntConstant 2) $
                    replaceVar "b" (IntConstant 3) $
                        replaceVar "c" (IntConstant 4) expr1
        value2 = replaceVar "a" (IntConstant 2) $
                        replaceVar "c" (IntConstant 0) expr1
        value3 = replaceVar "a" (IntConstant 2) $
                    replaceVar "b" (IntConstant 3) $
                        replaceVar "c" (IntConstant 4) expr2
        value4 = replaceVar "a" (IntConstant 2) $
                    replaceVar "b" (IntConstant 3) $
                        replaceVar "c" (IntConstant 4) expr3
        value5 = replaceVar "a" (IntConstant 2) $
                    replaceVar "b" (IntConstant 3) $
                        replaceVar "c" (IntConstant 4) expr4
    in
    do
        assertEquals (evaluate value1) (IntConstant 14)
        assertEquals (evaluate value2) (IntConstant 2)
        assertEquals (evaluate value3) (IntConstant (-5))
        assertEquals (evaluate value4) (IntConstant (-32))
        assertEquals (evaluate value5) (IntConstant (-102))

test1_2 = do
    assertEquals (pow (-1) 10) 1
    assertEquals (pow 3 3) 27
    assertEquals (pow 9 2) 81
    assertEquals (pow 3 4) 81
    assertEquals (pow 2 64) (4294967296 * 4294967296)
    -- putStrLn $ show $ pow 2 10000000
    -- putStrLn "sin"
    -- putStrLn $ show $ sin $ pi / 4
    -- putStrLn $ show $ sin $ pi / 4 + pi * 100
    -- putStrLn $ show $ sin $ pi / 4 - pi * 100
    -- putStrLn "cos"
    -- putStrLn $ show $ cos $ pi / 4
    -- putStrLn $ show $ cos $ pi / 4 + pi * 100
    -- putStrLn $ show $ cos $ pi / 4 - pi * 100
    assertTrue $ not $ isPrime 1
    assertTrue $ isPrime 2
    assertTrue $ isPrime 3
    assertTrue $ not $ isPrime 4
    assertTrue $ isPrime 5
    assertTrue $ not $ isPrime 6
    assertTrue $ isPrime 7
    assertTrue $ not $ isPrime 8
    assertTrue $ not $ isPrime 9
    assertTrue $ not $ isPrime 10
    assertTrue $ isPrime 11
    assertTrue $ not $ isPrime 21
    assertTrue $ isPrime 31
    assertTrue $ isPrime 41
    assertTrue $ not $ isPrime 111
    assertTrue $ not $ isPrime 112
    assertEquals (gcd 1 1) 1
    assertEquals (gcd 1 10) 1
    assertEquals (gcd 10 10) 10
    assertEquals (gcd 50 130) 10
    assertEquals (gcd 130 50) 10
    assertEquals (gcd 111 41) 1
    assertEquals (gcd 31 41) 1
    assertEquals (gcd 80 20) 20
    assertEquals (gcd 30 18) 6
    assertEquals (gcd 18 30) 6


test2_1 = let
        tree1 = insert (10, 9) $
                    insert (2, 8) $
                        insert (0, 7) $
                            insert (-1, 6) $
                                insert (4, 5) $
                                    insert (3, 4) $
                                        insert (7, 3) $
                                            insert (1, 2) $
                                                insert (8, 1) emptyTree
        tree1m = TreeMapNode 9 8 1 (
                    TreeMapNode 7 1 2 (
                        TreeMapNode 2 (-1) 6 EmptyTreeMap (
                            TreeMapNode 1 0 7 EmptyTreeMap EmptyTreeMap
                        )
                    ) (
                        TreeMapNode 4 7 3 (
                            TreeMapNode 3 3 4 (
                                TreeMapNode 1 2 8 EmptyTreeMap EmptyTreeMap
                            ) (
                                TreeMapNode 1 4 5 EmptyTreeMap EmptyTreeMap
                            )
                        ) EmptyTreeMap
                    )
                ) (
                    TreeMapNode 1 10 9 EmptyTreeMap EmptyTreeMap
                )
        list = [(-1,6),(0,7),(1,2),(2,8),(3,4),(4,5),(7,3),(8,1),(10,9)]
        tree2 = remove 3 tree1
        tree2m = TreeMapNode 8 8 1 (
                    TreeMapNode 6 1 2 (
                        TreeMapNode 2 (-1) 6 EmptyTreeMap (
                            TreeMapNode 1 0 7 EmptyTreeMap EmptyTreeMap
                        )
                    ) (
                        TreeMapNode 3 7 3 (
                            TreeMapNode 2 4 5 (
                                TreeMapNode 1 2 8 EmptyTreeMap EmptyTreeMap
                            ) EmptyTreeMap
                        ) EmptyTreeMap
                    )
                ) (
                    TreeMapNode 1 10 9 EmptyTreeMap EmptyTreeMap
                )
        list2 = [(-1,6),(0,7),(1,2),(2,8),(4,5),(7,3),(8,1),(10,9)]
    in do
        assertEquals (emptyTree :: (TreeMap Integer)) EmptyTreeMap
        assertEquals tree1 tree1m
        assertEquals tree2 tree2m
        assertTrue $ contains tree1 3
        assertTrue $ contains tree1 2
        assertTrue $ contains tree1 4
        assertTrue $ not $ contains tree2 3
        assertTrue $ contains tree2 2
        assertTrue $ contains tree2 4
        assertEquals (listFromTree tree1) list
        assertEquals (listFromTree tree2) list2
        assertEquals (lookup 2 tree1) 8
        assertEquals (lookup 4 tree1) 5
        assertEquals (lookup (-1) tree1) 6
        assertEquals (lookup 3 tree1) 4
        assertEquals (lookup 2 tree2) 8
        assertEquals (lookup 4 tree2) 5
        assertEquals (lookup (-1) tree2) 6
        assertEquals (nearestLE 1 tree1) (-1, 6)
        assertEquals (nearestLE 8 tree1) (10, 9)
        assertEquals (nearestLE 7 tree1) (3, 4)
        assertEquals (nearestLE (-1) tree1) (0, 7)
        assertEquals (kMean 3 tree1) (2, 8)
        assertEquals (kMean 4 tree1) (3, 4)
        assertEquals (kMean 5 tree1) (4, 5)
        assertEquals (kMean 7 tree1) (8, 1)
        assertEquals (kMean 3 tree2) (2, 8)
        assertEquals (kMean 4 tree2) (4, 5)
        assertEquals (kMean 6 tree2) (8, 1)
        assertEquals (kMean 7 tree2) (10, 9)

test2_2 = do
    assertEquals (foldl (+) 0 [1, 2, 3, 4, 5]) 15
    assertEquals (foldl (-) 0 [1, 2, 3, 4, 5]) (-15)
    assertEquals (foldr (+) 0 [1, 2, 3, 4, 5]) 15
    assertEquals (foldr (-) 0 [1, 2, 3, 4, 5]) 3
    assertEquals (unfoldr (\a -> if a > 10 then Nothing else Just (a, a + 1)) 0) [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
    assertEquals (unfoldr (\a -> Nothing) 10) ([] :: [Integer])
    assertEquals (map (+1) [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10]) [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11]
    assertEquals (product [1, 2, 3, 4, 5]) 120
    assertEquals (product []) 1
    assertEquals (catMaybes [Just 0, Just 1, Nothing, Just 3, Nothing, Just 5]) [0, 1, 3, 5]
    assertEquals (diagonal [[1, 0, 0], [0, 2, 0], [0, 0, 3]]) [1, 2, 3]
    assertEquals (diagonal [[1, 0, 0], [0, 2, 0], [0, 0, 3], [0, 0, 0]]) [1, 2, 3]
    assertEquals (diagonal [[1, 0, 0, 0], [0, 2, 0, 0], [0, 0, 3, 0]]) [1, 2, 3]
    assertEquals (diagonal [[1], [0, 2], [0, 0, 3]]) [1, 2, 3]
    assertEquals (map (\(Just e) -> e) $ filterNot (\e -> e == Nothing) [Just 0, Just 1, Nothing, Just 3, Nothing, Just 5]) [0, 1, 3, 5]
    assertTrue $ elem 1 [1, 2, 3, 1, 4, 5]
    assertTrue $ elem 2 [1, 2, 3, 1, 4, 5]
    assertTrue $ not $ elem 0 [1, 2, 3, 1, 4, 5]
    assertEquals (rangeTo 0 4 1) [0, 1, 2, 3]
    assertEquals (rangeTo 0 0 1) []
    assertEquals (rangeTo 0 4 3) [0, 3]
    assertEquals (rangeTo 0 6 3) [0, 3]
    assertEquals (rangeTo 0 7 3) [0, 3, 6]
    assertEquals (append [1, 2, 3, 4] [5, 6, 7]) [1, 2, 3, 4, 5, 6, 7]
    assertEquals (append [1, 2, 3, 4] []) [1, 2, 3, 4]
    assertEquals (append [] [5, 6, 7]) [5, 6, 7]
    assertEquals (append [] []) ([] :: [Integer])
    assertEquals (groups [1, 2, 3, 4] 1) [[1], [2], [3], [4]]
    assertEquals (groups [1, 2, 3, 4] 2) [[1, 2], [3, 4]]
    assertEquals (groups [1, 2, 3, 4, 5] 2) [[1, 2], [3, 4], [5]]
    assertEquals (groups [1, 2, 3, 4, 5] 3) [[1, 2, 3], [4, 5]]
    assertEquals (groups [1, 2, 3, 4, 5] 7) [[1, 2, 3, 4, 5]]
    assertEquals (groups [] 7) ([[]] :: [[Integer]])
    assertEquals (groups [] 1) ([[]] :: [[Integer]])
    assertEquals (groups [1, 2, 3, 4, 5] 0) [[], [1, 2, 3, 4, 5]] -- wtf?
    assertEquals (groups [1, 2, 3, 4, 5] (-1)) [[1, 2, 3, 4, 5]] -- yet wtf?

test3_1 = let
        number1 = Pred $ Succ $ Succ $ Pred $ Pred $ Succ $ Succ $ Zero
    in do
        assertEquals number1 (Succ Zero)
        assertEquals number1 (normalize number1)
        assertEquals (normalize number1) (Succ Zero)
        assertTrue $ number1 <= (Succ Zero)
        assertTrue $ number1 >= (Succ Zero)
        assertTrue $ number1 < (Succ number1)
        assertTrue $ number1 > (Pred number1)
        assertEquals (Pred $ Pred $ Pred $ Pred number1 + fromInteger 10) (fromInteger 7 :: WeirdPeanoNumber)
        assertEquals (Pred $ Pred $ Pred $ Pred number1 - fromInteger 10) (fromInteger (-13) :: WeirdPeanoNumber)
        assertEquals (signum number1) (fromInteger 1)
        assertEquals (signum (-number1)) (fromInteger (-1) :: WeirdPeanoNumber)
        assertEquals (toRational number1) 1
        assertEquals (toRational (number1 + fromInteger 10 :: WeirdPeanoNumber)) 11
        assertEquals (toEnum 1) number1
        assertEquals (toEnum 11) (number1 + toEnum 10 :: WeirdPeanoNumber)
        assertEquals (fromEnum number1) 1
        assertEquals (fromEnum (number1 + toEnum 10 :: WeirdPeanoNumber)) 11
        assertEquals (number1 * fromInteger 10) (fromInteger 10 :: WeirdPeanoNumber)
        assertEquals (-number1 * fromInteger 10) (fromInteger (-10) :: WeirdPeanoNumber)
        assertEquals (fromInteger 11 * fromInteger (-10)) (fromInteger (-110) :: WeirdPeanoNumber)
        assertEquals (fromInteger (-11) * fromInteger (-10)) (fromInteger 110 :: WeirdPeanoNumber)
        assertEquals (fromInteger 11 * fromInteger 10) (fromInteger 110 :: WeirdPeanoNumber)
        assertEquals (fromInteger (-20) `quotRem` fromInteger 3) (fromInteger (-6) :: WeirdPeanoNumber, fromInteger (-2) :: WeirdPeanoNumber)
        assertEquals (fromInteger (-20) `quotRem` fromInteger (-3)) (fromInteger 6 :: WeirdPeanoNumber, fromInteger (-2) :: WeirdPeanoNumber)
        assertEquals (fromInteger 20 `quotRem` fromInteger (-3)) (fromInteger (-6) :: WeirdPeanoNumber, fromInteger 2 :: WeirdPeanoNumber)
        assertEquals (fromInteger 20 `quotRem` fromInteger 3) (fromInteger 6 :: WeirdPeanoNumber, fromInteger 2 :: WeirdPeanoNumber)
        assertEquals (fromInteger 0 `quotRem` fromInteger 1) (fromInteger 0 :: WeirdPeanoNumber, fromInteger 0 :: WeirdPeanoNumber)
        assertEquals (fromInteger 5 `quotRem` fromInteger 5) (fromInteger 1 :: WeirdPeanoNumber, fromInteger 0 :: WeirdPeanoNumber)
        assertEquals (fromInteger (-20) `divMod` fromInteger 3) (fromInteger (-7) :: WeirdPeanoNumber, fromInteger 1 :: WeirdPeanoNumber)

test3_2 = let
        list = RCons (RCons (RCons (RCons (RCons RNil 1) 2) 3) 4) 5
    in do
        assertEquals (show list) "RCons (RCons (RCons (RCons (RCons (RNil) 1) 2) 3) 4) 5"
        assertEquals list list
        assertEquals (listToRList [1, 2, 3, 4, 5]) list
        assertEquals (rlistToList list) [1, 2, 3, 4, 5]
        assertTrue $ list <= list
        assertTrue $ (listToRList [1, 2, 3, 4, 5]) <= (listToRList [1, 2, 3, 4, 5])
        assertTrue $ (listToRList [0, 2, 3, 4, 5]) <= (listToRList [1, 2, 3, 4, 5])
        assertTrue $ not $ (listToRList [0, 2, 3, 4, 6]) <= (listToRList [1, 2, 3, 4, 5])
        assertTrue $ (listToRList [2, 2, 3, 4, 4]) <= (listToRList [1, 2, 3, 4, 5])
        assertTrue $ (listToRList [1, 2, 3, 4, 4]) <= (listToRList [1, 2, 3, 4, 5])
        assertTrue $ (listToRList [1, 2, 3, 4]) <= (listToRList [1, 2, 3, 4, 5])
        assertTrue $ (listToRList [2, 3, 4, 5]) <= (listToRList [1, 2, 3, 4, 5])
        assertTrue $ not $ (listToRList [1, 2, 3, 4, 6]) <= (listToRList [1, 2, 3, 4, 5])
        assertTrue $ not $ (listToRList [1, 2, 3, 4, 5]) <= (listToRList [2, 3, 4, 5])
        assertTrue $ not $ (listToRList [1, 2, 3, 4, 5]) <= (listToRList [1, 2, 3, 4])
        assertEquals ((listToRList [5, 6]) <> ((listToRList [4]) <> (listToRList [1, 2, 3]))) (((listToRList [5, 6]) <> (listToRList [4])) <> (listToRList [1, 2, 3]))
        assertEquals ((listToRList [5, 6]) <> ((listToRList [4]) <> (listToRList [1, 2, 3]))) (listToRList [1, 2, 3, 4, 5, 6])
        assertEquals ((listToRList [1, 2, 3, 4, 5, 6]) <> mempty) (listToRList [1, 2, 3, 4, 5, 6])
        assertEquals (mempty <> (listToRList [1, 2, 3, 4, 5, 6])) (listToRList [1, 2, 3, 4, 5, 6])
        assertEquals (mconcat [listToRList [5, 6], listToRList [4], listToRList [1, 2, 3]]) (foldr (<>) mempty [listToRList [5, 6], listToRList [4], listToRList [1, 2, 3]])
        assertEquals (fmap id listToRList [1, 2, 3, 4, 5, 6]) (listToRList [1, 2, 3, 4, 5, 6])
        assertEquals (fmap (+3) (listToRList [1, 2, 3, 4, 5, 6])) (listToRList [4, 5, 6, 7, 8, 9])

test3_3 = let
        unionSet1 = PUnionSet (\e -> e == 1 || e == 2 || e == 3) :: PUnionSet Int
        unionSet2 = PUnionSet (\e -> e == 4) :: PUnionSet Int
        unionSet3 = PUnionSet (\e -> e == 5 || e == 6) :: PUnionSet Int
        unionSet4 = PUnionSet (\e -> e == 1 || e == 2 || e == 3 || e == 4 || e == 5 || e == 6) :: PUnionSet Int
        list = [0, 1, 2, 3, 4, 5, 6, 7]
        mask1 = [False, True, True, True, True, True, True, False]
        intersectionSet1 = PIntersectionSet (\e -> e == 1 || e == 2 || e == 3) :: PIntersectionSet Int
        intersectionSet2 = PIntersectionSet (\e -> e == 2 || e == 3 || e == 4 || e == 5) :: PIntersectionSet Int
        intersectionSet3 = PIntersectionSet (\e -> e == 1 || e == 2 || e == 3 || e == 6) :: PIntersectionSet Int
        intersectionSet4 = PIntersectionSet (\e -> e == 2 || e == 3) :: PIntersectionSet Int
        mask2 = [False, False, True, True, False, False, False, False]
        plusSet1 = PPlusSet (\e -> e == 1 || e == 2 || e == 3) :: PPlusSet Int
        plusSet2 = PPlusSet (\e -> e == 3 || e == 4 || e == 5) :: PPlusSet Int
        plusSet3 = PPlusSet (\e -> e == 5 || e == 6) :: PPlusSet Int
        plusSet4 = PPlusSet (\e -> e == 1 || e == 2 || e == 4 || e == 6) :: PPlusSet Int
        mask3 = [False, True, True, False, True, False, True, False]
        plus b = Inversable (\a -> a + b) (\a -> a - b)
    in do
        assertEquals (map (unionContains $ unionSet3 <> (unionSet2 <> unionSet1)) list) mask1
        assertEquals (map (unionContains $ unionSet4) list) mask1
        assertEquals (map (unionContains $ (unionSet3 <> unionSet2) <> unionSet1) list) mask1
        assertEquals (map (unionContains $ unionSet4 <> mempty) list) mask1
        assertEquals (map (unionContains $ mempty <> unionSet4) list) mask1
        assertEquals (map (unionContains $ mconcat [unionSet3, unionSet2, unionSet1]) list) mask1
        assertEquals (map (unionContains $ foldr (<>) mempty [unionSet3, unionSet2, unionSet1]) list) mask1
        assertEquals (map (intersectionContains $ intersectionSet3 <> (intersectionSet2 <> intersectionSet1)) list) mask2
        assertEquals (map (intersectionContains $ intersectionSet4) list) mask2
        assertEquals (map (intersectionContains $ (intersectionSet3 <> intersectionSet2) <> intersectionSet1) list) mask2
        assertEquals (map (intersectionContains $ intersectionSet4 <> mempty) list) mask2
        assertEquals (map (intersectionContains $ mempty <> intersectionSet4) list) mask2
        assertEquals (map (intersectionContains $ mconcat [intersectionSet3, intersectionSet2, intersectionSet1]) list) mask2
        assertEquals (map (intersectionContains $ foldr (<>) mempty [intersectionSet3, intersectionSet2, intersectionSet1]) list) mask2
        assertEquals (map (plusContains $ plusSet3 <> (plusSet2 <> plusSet1)) list) mask3
        assertEquals (map (plusContains $ plusSet4) list) mask3
        assertEquals (map (plusContains $ (plusSet3 <> plusSet2) <> plusSet1) list) mask3
        assertEquals (map (plusContains $ plusSet4 <> mempty) list) mask3
        assertEquals (map (plusContains $ mempty <> plusSet4) list) mask3
        assertEquals (map (plusContains $ mconcat [plusSet3, plusSet2, plusSet1]) list) mask3
        assertEquals (map (plusContains $ foldr (<>) mempty [plusSet3, plusSet2, plusSet1]) list) mask3
        assertEquals (map (unionContains $ mmap (plus 3) unionSet1) list) [False, False, False, False, True, True, True, False]
        assertEquals (map (intersectionContains $ mmap (plus 3) intersectionSet1) list) [False, False, False, False, True, True, True, False]
        assertEquals (map (plusContains $ mmap (plus 3) plusSet1) list) [False, False, False, False, True, True, True, False]

test4_1 = let
        plus x y = do
            x' <- x
            y' <- y
            return $ x' + y'
    in do
        assertEquals (fun ((FunMonad length) `plus` (FunMonad ((+) 1 . length))) "hello") 11
        assertEquals (fun ((FunMonad length) `plus` (FunMonad ((+) 1 . length))) "world!") 13

test4_2 = let
        plus x y = do
            x' <- x
            y' <- y
            return $ x' + y'
        quadruple1 = FourOf 1 2 3 4
        quadruple2 = FourOf 4 6 7 8
        quadruple3 = FourOf 5 8 10 12
    in do
        assertEquals (quadruple1 `plus` quadruple2) quadruple3


test5_1 = do
    assertEquals (index (list2dlist [1, 2, 3, 4, 5, 6]) 0) 1
    assertEquals (index (list2dlist [1, 2, 3, 4, 5, 6]) 1) 2
    assertEquals (index (list2dlist [1, 2, 3, 4, 5, 6]) 5) 6
    assertEquals (dlist2list (insertAt (list2dlist [1, 2, 3, 4, 5, 6]) 0 3)) [3, 1, 2, 3, 4, 5, 6]
    assertEquals (dlist2list (insertAt (list2dlist [1, 2, 3, 4, 5, 6]) 1 3)) [1, 3, 2, 3, 4, 5, 6]
    assertEquals (dlist2list (insertAt (list2dlist [1, 2, 3, 4, 5, 6]) 5 3)) [1, 2, 3, 4, 5, 3, 6]
    assertEquals (dlist2list (insertAt (list2dlist [1, 2, 3, 4, 5, 6]) 6 3)) [1, 2, 3, 4, 5, 6, 3]
    assertEquals (dlist2list (removeAt (list2dlist [1, 2, 3, 4, 5, 6]) 0)) [2, 3, 4, 5, 6]
    assertEquals (dlist2list (removeAt (list2dlist [1, 2, 3, 4, 5, 6]) 1)) [1, 3, 4, 5, 6]
    assertEquals (dlist2list (removeAt (list2dlist [1, 2, 3, 4, 5, 6]) 4)) [1, 2, 3, 4, 6]
    assertEquals (dlist2list (removeAt (list2dlist [1, 2, 3, 4, 5, 6]) 5)) [1, 2, 3, 4, 5]

test5_2 = do
    assertEqualsEps 1e-6 (sLookup 11 $ sinPrecisions 0) (sin 0)
    assertEqualsEps 1e-6 (sLookup 11 $ sinPrecisions pi) (sin pi)
    assertEqualsEps 1e-6 (sLookup 11 $ sinPrecisions $ 2 * pi) (sin $ 2 * pi)
    assertEqualsEps 1e-6 (sLookup 11 $ sinPrecisions $ (-100) * pi) (sin $ (-100) * pi)
    assertEqualsEps 1e-6 (sLookup 11 $ sinPrecisions $ pi / 3) (sin $ pi / 3)
    assertEqualsEps 1e-6 (sLookup 11 $ sinPrecisions $ pi / 4) (sin $ pi / 4)
    assertEqualsEps 1e-6 (sLookup 11 $ sinPrecisions $ pi / 6) (sin $ pi / 6)
    assertEqualsEps 1e-6 (sLookup 11 $ sinPrecisions $ pi / 2) (sin $ pi / 2)
    assertEqualsEps 1e-3 (fromRational $ sLookup 6 ePrecisions) (exp 1.0)
    assertEqualsEps 1e-4 (fromRational $ sLookup 7 ePrecisions) (exp 1.0)
    assertEqualsEps 1e-5 (fromRational $ sLookup 8 ePrecisions) (exp 1.0)
    assertEqualsEps 1e-6 (fromRational $ sLookup 9 ePrecisions) (exp 1.0)
    assertEqualsEps 1e-7 (fromRational $ sLookup 10 ePrecisions) (exp 1.0)


test6_1 = let
        tree1 = ltInsert 10 $
                    ltInsert 2 $
                        ltInsert 0 $
                            ltInsert (-1) $
                                ltInsert 4 $
                                    ltInsert 3 $
                                        ltInsert 7 $
                                            ltInsert 1 $
                                                ltInsert 8 ltEmpty
        tree2 = ltRemove tree1 3
        tree3 = ltRemove tree1 8
        list1 = [-1, 0, 1, 2, 3, 4, 7, 8, 10]
        list2 = [-1, 0, 1, 2, 4, 7, 8, 10]

        testTopDown tree = let
                setFst val (_, snd) = (val, snd)
                applySnd foo (fst, snd) = (fst, foo snd)
                parent (LTNode parent' _ _ _) = parent'
                value (LTNode _ value' _ _) = value'
                walk tree = let
                        walkSubTree tree child = [(tree, parent child)] ++ 
                            (setFst tree <$> applySnd parent <$> cache) where 
                                cache = walk child
                    in case tree of
                        LTEmpty -> []
                        LTNode _ _ LTEmpty LTEmpty -> []
                        LTNode _ _ left LTEmpty -> walkSubTree tree left
                        LTNode _ _ LTEmpty right -> walkSubTree tree right
                        LTNode _ _ left right -> walkSubTree tree left ++ walkSubTree tree right
                subTest tree = case tree of 
                    LTEmpty -> putStr ""
                    LTNode _ _ left right -> do
                        testTopDown left
                        testTopDown right
            in do
                foldl (>>) mempty $ (\(from, to) -> assertEquals to from) <$> walk tree
                subTest tree
    in do
        assertEquals (ltEmpty :: (LinkedTree Integer)) LTEmpty
        assertEquals (fromLinkedTree tree1) list1
        assertEquals (fromLinkedTree tree2) list2
        assertTrue $ ltFind tree1 8
        assertTrue $ ltFind tree1 10
        assertTrue $ ltFind tree1 2
        assertTrue $ ltFind tree1 3
        assertTrue $ ltFind tree1 4
        assertTrue $ ltFind tree1 (-1)
        assertTrue $ ltFind tree2 8
        assertTrue $ ltFind tree2 10
        assertTrue $ ltFind tree2 2
        assertTrue $ not $ ltFind tree2 3
        assertTrue $ ltFind tree2 4
        assertTrue $ ltFind tree2 (-1)
        assertTrue $ not $ ltFind tree3 8
        assertTrue $ ltFind tree3 10
        assertTrue $ ltFind tree3 2
        assertTrue $ ltFind tree3 3
        assertTrue $ ltFind tree3 4
        assertTrue $ ltFind tree3 (-1)
        testTopDown tree1
        testTopDown tree1
        testTopDown tree3

main :: IO ()
main = do
    putStr "Test 1-1 "
    test1_1
    putStrLn "SUCCESS"
    putStr "Test 1-2 "
    test1_2
    putStrLn "SUCCESS"
    putStr "Test 2-1 "
    test2_1
    putStrLn "SUCCESS"
    putStr "Test 2-2 "
    test2_2
    putStrLn "SUCCESS"
    putStr "Test 3-1 "
    test3_1
    putStrLn "SUCCESS"
    putStr "Test 3-2 "
    test3_2
    putStrLn "SUCCESS"
    putStr "Test 3-3 "
    test3_3
    putStrLn "SUCCESS"
    putStr "Test 4-1 "
    test4_1
    putStrLn "SUCCESS"
    putStr "Test 4-2 "
    test4_2
    putStrLn "SUCCESS"
    putStr "Test 5-1 "
    test5_1
    putStrLn "SUCCESS"
    putStr "Test 5-2 "
    test5_2
    putStrLn "SUCCESS"
    putStr "Test 6-1 "
    test6_1
    putStrLn "SUCCESS"
