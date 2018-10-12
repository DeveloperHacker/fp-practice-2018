module Main where

import Prelude hiding (sin, cos, gcd, lookup, foldl, foldr, unfoldr, map, concatMap, 
    filter, maxBy, minBy, reverse, sum, product, elem)
import Task1_1 (Term(IntConstant, Variable))
import Task1_1 (Term, (|+|), (|-|), (|*|), replaceVar, evaluate)
import Task1_2 (sin, cos, pow, isPrime, gcd)
import Task2_1
import Task2_2

assertEquals actual expected | actual == expected = putStr ""
assertEquals actual expected = error $ concat ["expected: ", (show expected), " but was: ", (show actual)]

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
        putStrLn "Test1-1"
        putStrLn (show $ evaluate value1)
        putStrLn (show $ evaluate value2)
        putStrLn (show $ evaluate value3)
        putStrLn (show $ evaluate value4)
        putStrLn (show $ evaluate value5)

test1_2 = do
    putStrLn "Test1-2"
    assertEquals (pow (-1) 10) 1
    assertEquals (pow 3 3) 27
    assertEquals (pow 9 2) 81
    assertEquals (pow 3 4) 81
    assertEquals (pow 2 64) (4294967296 * 4294967296)
    -- putStrLn $ show $ pow 2 10000000
    putStrLn "sin"
    putStrLn $ show $ sin $ pi / 4
    putStrLn $ show $ sin $ pi / 4 + pi * 100
    putStrLn $ show $ sin $ pi / 4 - pi * 100
    putStrLn "cos"
    putStrLn $ show $ cos $ pi / 4
    putStrLn $ show $ cos $ pi / 4 + pi * 100
    putStrLn $ show $ cos $ pi / 4 - pi * 100
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
        list1 = [(-1,6),(0,7),(1,2),(2,8),(3,4),(4,5),(7,3),(8,1),(10,9)]
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
        putStrLn "Test2-1"
        assertEquals (emptyTree :: (TreeMap Integer)) EmptyTreeMap
        assertEquals tree1 tree1m
        assertEquals tree2 tree2m
        assertTrue $ contains tree1 3
        assertTrue $ contains tree1 2
        assertTrue $ contains tree1 4
        assertTrue $ not $ contains tree2 3
        assertTrue $ contains tree2 2
        assertTrue $ contains tree2 4
        assertEquals (listFromTree tree1) list1
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
    putStrLn "Test2-1"
    assertEquals (foldl (+) 0 [1, 2, 3, 4, 5]) 15
    assertEquals (foldl (-) 0 [1, 2, 3, 4, 5]) (-15)
    assertEquals (foldr (+) 0 [1, 2, 3, 4, 5]) 15
    assertEquals (foldr (-) 0 [1, 2, 3, 4, 5]) 3


main :: IO ()
main = do
    test1_1
    putStrLn ""
    test1_2
    putStrLn ""
    test2_1
    putStrLn ""
    test2_2