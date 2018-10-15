module Task3_1 where

data WeirdPeanoNumber = Zero | Succ WeirdPeanoNumber | Pred WeirdPeanoNumber

-- Реализуйте все классы типов, которым должны отвечать целые числа

weirdPeanoNumberToInteger :: WeirdPeanoNumber -> Integer
weirdPeanoNumberToInteger number = case number of
    Zero -> 0
    Succ child -> weirdPeanoNumberToInteger child + 1
    Pred child -> weirdPeanoNumberToInteger child - 1

weirdPeanoNumberFromInteger :: Integer -> WeirdPeanoNumber
weirdPeanoNumberFromInteger integer = case integer of
    0 -> Zero
    _ | integer > 0 -> Succ $ weirdPeanoNumberFromInteger $ integer - 1
    _ | integer < 0 -> Pred $ weirdPeanoNumberFromInteger $ integer + 1

instance Eq WeirdPeanoNumber where
    left == right = weirdPeanoNumberToInteger left == weirdPeanoNumberToInteger right

instance Ord WeirdPeanoNumber where
    left <= right = weirdPeanoNumberToInteger left <= weirdPeanoNumberToInteger right

instance Num WeirdPeanoNumber where
    left + right = todo
    left - right = todo
    left * right = todo
    negate number = case number of
        Zero -> Zero
        Succ child -> Pred $ negate child
        Pred child -> Succ $ negate child 
    abs = weirdPeanoNumberFromInteger $ abs $ weirdPeanoNumberToInteger number
    signum number = weirdPeanoNumberFromInteger $ signum $ weirdPeanoNumberToInteger number
    fromInteger integer = weirdPeanoNumberFromInteger integer

-- instance Real WeirdPeanoNumber where
    -- toRational :: WeirdPeanoNumber -> Rational

-- instance Enum WeirdPeanoNumber where
    -- succ :: WeirdPeanoNumber -> WeirdPeanoNumber
    -- pred :: WeirdPeanoNumber -> WeirdPeanoNumber
    -- toEnum :: Int -> WeirdPeanoNumber
    -- fromEnum :: WeirdPeanoNumber -> Int

-- instance Integral WeirdPeanoNumber where
    -- quot :: WeirdPeanoNumber -> WeirdPeanoNumber -> WeirdPeanoNumber
    -- rem :: WeirdPeanoNumber -> WeirdPeanoNumber -> WeirdPeanoNumber
    -- toInteger :: WeirdPeanoNumber -> Integer