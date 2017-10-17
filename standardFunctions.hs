module StandardFunctions where

myOr :: [Bool] -> Bool
myOr bools
  | bools == [] = False
  | (head bools) == True = True
  | otherwise = myOr (tail bools)

myAny :: (a -> Bool) -> [a] -> Bool
myAny f as
  | length as == 0 = False
  | f (head as) == True = True
  | otherwise = myAny f (tail as)

myElem :: Eq a => a -> [a] -> Bool
myElem e es
  | length es == 0 = False
  | e == (head es) = True
  | otherwise = myElem e (tail es)

myElemAny :: Eq a => a -> [a] -> Bool
myElemAny e es = myAny (\x -> x == e) es

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x : xs) = myReverse xs ++ [x]

squish :: [[a]] -> [a]
squish [] = []
squish (x : xs) = x ++ squish xs

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f [] = []
squishMap f (x : xs) = (f x) ++ squishMap f xs

squishAgain :: [[a]] -> [a]
squishAgain xs = squishMap (\x -> x) xs

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy comparisonFn as =
  inner comparisonFn (tail as) (head as)  where
  inner f xs currentMax =
    case xs of
      [] -> currentMax
      (y:ys) -> inner comparisonFn ys (if comparisonFn y currentMax == GT then y else currentMax)

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy comparisonFn as =
  inner comparisonFn (tail as) (head as)  where
  inner f xs currentMin =
    case xs of
      [] -> currentMin
      (y:ys) -> inner comparisonFn ys (if comparisonFn y currentMin == LT then y else currentMin)

myMaximum :: (Ord a) => [a] -> a
myMaximum xs = myMaximumBy compare xs

myMinimum :: (Ord a) => [a] -> a
myMinimum xs = myMinimumBy compare xs
