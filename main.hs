module Main where

import Prelude hiding (sin, cos, gcd)
import Task1_1 (Term(IntConstant, Variable))
import Task1_1 (Term, (|+|), (|-|), (|*|), replaceVar, evaluate)
import Task1_2 (sin, cos, pow, isPrime, gcd)

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
    putStrLn (show (pow (-1) 10))
    putStrLn (show (pow 3 3))
    putStrLn (show (sin 0.78539816339))
    putStrLn (show (cos 0.78539816339))
    putStrLn (show (isPrime 1))
    putStrLn (show (isPrime 2))
    putStrLn (show (isPrime 3))
    putStrLn (show (isPrime 4))
    putStrLn (show (isPrime 5))
    putStrLn (show (isPrime 6))
    putStrLn (show (isPrime 7))
    putStrLn (show (isPrime 8))
    putStrLn (show (isPrime 9))
    putStrLn (show (isPrime 10))
    putStrLn (show (isPrime 11))
    putStrLn (show (isPrime 21))
    putStrLn (show (isPrime 31))
    putStrLn (show (isPrime 41))
    putStrLn (show (isPrime 111))
    putStrLn (show (gcd 50 130))
    putStrLn (show (gcd 130 50))
    putStrLn (show (gcd 111 41))
    putStrLn (show (gcd 31 41))
    putStrLn (show (gcd 80 20))
    putStrLn (show (gcd 30 18))
    putStrLn (show (gcd 18 30))

main :: IO ()
main = do
    test1_1
    putStrLn ""
    test1_2