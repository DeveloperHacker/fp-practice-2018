module Main where

import Prelude hiding (sin, cos, gcd)
import Task1_1 (Term(IntConstant, Variable))
import Task1_1 (Term, (|+|), (|-|), (|*|), replaceVar, evaluate)
import Task1_2 (sin, cos, pow, isPrime, gcd)

assert False = error "assertion failed!"
assert _     = putStr ""

test1_1 = 
    let 
        expr1 = (Variable "a") |+| Variable "b" |*| Variable "c"
        expr2 = Variable "a" |-| Variable "b" |-| Variable "c"
        expr3 = Variable "a" |-| Variable "b" |*| IntConstant 10 |-| Variable "c"
        expr4 = expr1 |*| expr2 |+| expr3
        value1 = replaceVar "a" (IntConstant 2) $ replaceVar "b" (IntConstant 3) $ replaceVar "c" (IntConstant 4) expr1
        value2 = replaceVar "a" (IntConstant 2) $ replaceVar "c" (IntConstant 0) expr1
        value3 = replaceVar "a" (IntConstant 2) $ replaceVar "b" (IntConstant 3) $ replaceVar "c" (IntConstant 4) expr2
        value4 = replaceVar "a" (IntConstant 2) $ replaceVar "b" (IntConstant 3) $ replaceVar "c" (IntConstant 4) expr3
        value5 = replaceVar "a" (IntConstant 2) $ replaceVar "b" (IntConstant 3) $ replaceVar "c" (IntConstant 4) expr4
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
    assert $ (pow (-1) 10) == 1
    assert $ (pow 3 3) == 27
    assert $ (pow 9 2) == 81
    assert $ (pow 3 4) == 81
    assert $ (pow 2 64) == 4294967296 * 4294967296
    putStrLn $ show $ pow 2 10000000
    putStrLn (show (sin 0.78539816339))
    putStrLn (show (cos 0.78539816339))
    assert $ not $ isPrime 1
    assert $ isPrime 2
    assert $ isPrime 3
    assert $ not $ isPrime 4
    assert $ isPrime 5
    assert $ not $ isPrime 6
    assert $ isPrime 7
    assert $ not $ isPrime 8
    assert $ not $ isPrime 9
    assert $ not $ isPrime 10
    assert $ isPrime 11
    assert $ not $ isPrime 21
    assert $ isPrime 31
    assert $ isPrime 41
    assert $ not $ isPrime 111
    assert $ not $ isPrime 112
    assert $ (gcd 50 130) == 10
    assert $ (gcd 130 50) == 10
    assert $ (gcd 111 41) == 1
    assert $ (gcd 31 41) == 1
    assert $ (gcd 80 20) == 20
    assert $ (gcd 30 18) == 6
    assert $ (gcd 18 30) == 6

main :: IO ()
main = do
    test1_1
    putStrLn ""
    test1_2