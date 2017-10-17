module EnumFromTo where

eftBool :: Bool -> Bool -> [Bool]
eftBool False False = [False]
eftBool False True = [False, True]
eftBool True False = []
eftBool True True = [True]

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd LT LT = [LT]
eftOrd LT EQ = [LT, EQ]
eftOrd LT GT = [LT, EQ, GT]
eftOrd EQ EQ = [EQ]
eftOrd EQ GT = [EQ, GT]
eftOrd GT GT = [GT]
eftOrd _ _ = []

eftInt :: Int -> Int -> [Int]
eftInt from to
  | to < from  = []
  | from == to = [from]
  | otherwise  = [from] ++ eftInt (succ from) to

eftChar :: Char -> Char -> [Char]
eftChar from to
  | to < from = []
  | from == to = [from]
  | otherwise = [from] ++ eftChar (succ from) to
