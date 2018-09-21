module Task1_2 where

import Prelude hiding (sin, cos, gcd)
import Todo(todo)

-- синус числа (формула Тейлора)
sin :: Double -> Double
sin x = let 
    factor :: Integer -> Integer
    factor 0 = 1
    factor 1 = 1
    factor n | n > 1 = n * factor (n - 1)
    pow :: Double -> Integer -> Double
    pow y 0 = 1
    pow y 1 = y
    pow y n | n > 1 = y * pow y (n - 1)
    component :: Integer -> Double
    component n | n >= 0 = (pow (-1) n) * (pow x (2 * n + 1)) / (fromIntegral (factor (2 * n + 1)))
    solve :: Integer -> Double
    solve 0 = component 0
    solve n | n > 0 = (component n) + (solve (n - 1))
    in
    solve 10

-- косинус числа (формула Тейлора)
cos :: Double -> Double
cos x = let 
    factor :: Integer -> Integer
    factor 0 = 1
    factor 1 = 1
    factor n | n > 1 = n * factor (n - 1)
    pow :: Double -> Integer -> Double
    pow y 0 = 1
    pow y 1 = y
    pow y n | n > 1 = y * pow y (n - 1)
    component :: Integer -> Double
    component n | n >= 0 = (pow (-1) n) * (pow x (2 * n)) / (fromIntegral (factor (2 * n)))
    solve :: Integer -> Double
    solve 0 = component 0
    solve n | n > 0 = (component n) + (solve (n - 1))
    in
    solve 10

-- наибольший общий делитель двух чисел
gcd :: Integer -> Integer -> Integer
gcd 0 0 = 0
gcd 0 y | y > 0 = y
gcd x 0 | x > 0 = x
gcd x y | x > y && x > 0 && y > 0 = gcd (x `mod` y) y
gcd x y | x > 0 && y > 0 = gcd x (y `mod` x)

-- существует ли полный целочисленный квадрат в диапазоне [from, to)?
doesSquareBetweenExist :: Integer -> Integer -> Bool
doesSquareBetweenExist from to = todo

-- является ли дата корректной с учётом количества дней в месяце и
-- вискокосных годов?
isDateCorrect :: Integer -> Integer -> Integer -> Bool
isDateCorrect day month year = todo

-- возведение числа в степень, duh
-- готовые функции и плавающую арифметику использовать нельзя
pow :: Integer -> Integer -> Integer
pow x 0 = 1
pow x 1 = x
pow x y | y >= 2 = x * pow x (y - 1)

-- является ли данное число простым?
isPrime :: Integer -> Bool
isPrime 1 = False
isPrime x | x > 1 = let
    step :: Integer -> Bool
    step 1 = False
    step n | n > 1 = (x `mod` n == 0) || (step (n - 1))
    in
    not (step (x `div` 2))

type Point2D = (Double, Double)

-- рассчитайте площадь многоугольника по формуле Гаусса
-- многоугольник задан списком координат
shapeArea :: [Point2D] -> Double
shapeArea points = todo

-- треугольник задан своими координатами.
-- функция должна вернуть 
--  0, если он тупоугольный
--  1, если он остроугольный
--  2, если он прямоугольный
--  -1, если это не треугольник
triangleKind :: Point2D -> Point2D -> Point2D -> Integer
triangleKind a b c = todo
