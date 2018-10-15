module Task3_1 where

data WeirdPeanoNumber = Zero | Succ WeirdPeanoNumber | Pred WeirdPeanoNumber

-- Реализуйте все классы типов, которым должны отвечать целые числа

normalize :: WeirdPeanoNumber -> WeirdPeanoNumber
normalize number = let
        split number ss ps = case number of
            Zero -> (ss, ps)
            Succ child -> split child (Zero: ss) ps
            Pred child -> split child ss (Zero: ps)
        toSucc xs = case xs of
            [] -> Zero
            (_: xs) -> Succ $ toSucc xs
        toPred xs = case xs of
            [] -> Zero
            (_: xs) -> Pred $ toPred xs
        cut (ss, ps) = case (ss, ps) of
            (_, []) -> toSucc ss
            ([], _) -> toPred ps
            (_ : ss, _ : ps) -> cut (ss, ps)
    in cut $ split number [] []


instance Show WeirdPeanoNumber where
    show number = case number of
        Zero -> "Zero"
        Succ child -> concat ["Succ $ ", show child]
        Pred child -> concat ["Pred $ ", show child]

instance Eq WeirdPeanoNumber where
    left == right = normalize left `eq` normalize right where 
        left `eq` right = case (left, right) of
            (Zero, Zero) -> True
            (Succ a, Succ b) -> a `eq` b
            (Pred a, Pred b) -> a `eq` b
            _ -> False

instance Ord WeirdPeanoNumber where
    left <= right = normalize left `leq` normalize right where 
        left `leq` right = case (left, right) of
            (Zero, Zero) -> True
            (Zero, Succ _) -> True
            (Pred _, Zero) -> True
            (Pred _, Succ _) -> True
            (Succ a, Succ b) -> a `leq` b
            (Pred a, Pred b) -> a `leq` b
            _ -> False

instance Num WeirdPeanoNumber where
    left + right = case left of
        Zero -> right
        Succ child -> Succ $ child + right
        Pred child -> Pred $ child + right
    left * right = let 
            mul left right = case left of
                Zero -> 1
                Succ child -> right + child * right 
        in case (normalize left, normalize right) of
            (_, Zero) -> Zero
            (Zero, _) -> Zero
            (left @ (Succ _), right @ (Succ _)) -> mul left right
            (left @ (Pred _), right @ (Succ _)) -> -mul(-left) right
            (left @ (Succ _), right @ (Pred _)) -> -mul left (-right)
            (left @ (Pred _), right @ (Pred _)) -> mul (-left) (-right)
    negate number = case number of
        Zero -> Zero
        Succ child -> Pred $ negate child
        Pred child -> Succ $ negate child 
    abs number = if number < Zero then negate number else number
    signum number = case normalize number of
        Zero -> Zero
        Succ _ -> Succ Zero
        Pred _ -> Pred Zero
    fromInteger integer = case integer of
        0 -> Zero
        _ | integer > 0 -> Succ $ fromInteger $ integer - 1
        _ | integer < 0 -> Pred $ fromInteger $ integer + 1

instance Real WeirdPeanoNumber where
    toRational number = case number of
        Zero -> 0
        Succ child -> toRational child + 1
        Pred child -> toRational child - 1

instance Enum WeirdPeanoNumber where
    succ number = Succ number
    pred number = Pred number
    toEnum int = case int of
        0 -> Zero
        _ | int > 0 -> Succ $ toEnum $ int - 1
        _ | int < 0 -> Pred $ toEnum $ int + 1
    fromEnum number = case number of
        Zero -> 0
        Succ child -> fromEnum child + 1
        Pred child -> fromEnum child - 1

instance Integral WeirdPeanoNumber where
    left `quotRem` right = let 
            quot left right = if left >= right then Succ $ quot (left - right) right else Zero
            safeQuot left right = case (normalize left, normalize right) of
                (_, Zero) -> error "WeirdPeanoNumberZeroDivisionError"
                (Zero, _) -> Zero
                (left @ (Succ _), right @ (Succ _)) -> quot left right
                (left @ (Pred _), right @ (Succ _)) -> -quot (-left) right
                (left @ (Succ _), right @ (Pred _)) -> -quot left (-right)
                (left @ (Pred _), right @ (Pred _)) -> quot (-left) (-right)
            quotValue = safeQuot left right
        in (quotValue, left - quotValue * right)
    toInteger number = case number of
        Zero -> 0
        Succ child -> toInteger child + 1
        Pred child -> toInteger child - 1
