module Zipping where

myzip :: [a] -> [b] -> [(a, b)]
myzip a b
  | length a == 0 = []
  | length b == 0 = []
  | otherwise = [(head a, head b)] ++ myzip (tail a) (tail b)

myzipwith :: (a -> b -> c) -> [a] -> [b] -> [c]
myzipwith f a b
  | length a == 0 = []
  | length b == 0 = []
  | otherwise = [f (head a) (head b)] ++ myzipwith f (tail a) (tail b)
  
myzip2 :: [a] -> [b] -> [(a, b)]
myzip2 a b = myzipwith (\x -> \y -> (x, y)) a b
