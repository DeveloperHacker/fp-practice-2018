{-# LANGUAGE ViewPatterns #-}
module Task1_1 where

import Todo(todo)

data Operation = Plus | Minus | Times 
    deriving(Show, Eq)

data Term = IntConstant{ intValue :: Int }           -- числовая константа
            | Variable{ varName :: String }          -- переменная
            | BinaryTerm{ op :: Operation, lhv :: Term, rhv :: Term } -- бинарная операция
            deriving(Show, Eq)

-- Для бинарных операций необходима не только реализация, но и адекватные
-- ассоциативность и приоритет
(|+|) :: Term -> Term -> Term
(|+|) l r = BinaryTerm Plus l r
(|-|) :: Term -> Term -> Term
(|-|) l r = BinaryTerm Minus l r
(|*|) :: Term -> Term -> Term
(|*|) l r = BinaryTerm Times l r
infixl 6 |+|
infixl 6 |-|
infixl 7 |*|

-- Заменить переменную `varName` на `replacement`
-- во всём выражении `expression`
replaceVar :: String -> Term -> Term -> Term
replaceVar varName replacement expression = case (expression) of
    Variable var | var == varName -> replacement
    BinaryTerm op lhv rhv -> BinaryTerm op (replaceVar varName replacement lhv) (replaceVar varName replacement rhv)
    othervise -> expression

-- Посчитать значение выражения `Term`
-- если оно состоит только из констант
evaluate :: Term -> Term
evaluate expression = case (expression) of 
    Variable _ -> expression
    IntConstant _ -> expression
    BinaryTerm Plus (evaluate -> l) (evaluate -> r) -> case (l, r) of 
        (IntConstant a, IntConstant b) -> IntConstant $ a + b
        (IntConstant 0, _) -> r
        (_, IntConstant 0) -> l
        othervise -> l
    BinaryTerm Minus (evaluate -> l) (evaluate -> r) -> case (l, r) of 
        (IntConstant a, IntConstant b) -> IntConstant $ a - b
        (_, IntConstant 0) -> l
        othervise -> l
    BinaryTerm Times (evaluate -> l) (evaluate -> r) -> case (l, r) of 
        (IntConstant a, IntConstant b) -> IntConstant $ a * b
        (IntConstant 0, _) -> IntConstant 0
        (_, IntConstant 0) -> IntConstant 0
        (IntConstant 1, _) -> r
        (_, IntConstant 1) -> l
        othervise -> l
