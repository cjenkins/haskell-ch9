module FearfulSymmetry where

--1
spaceToWord :: String -> [String]
spaceToWord s
  | elem ' ' s == False = [s]
  | otherwise = [takeWhile (\c -> c /= ' ') s] ++ spaceToWord (tail (dropWhile (\c -> c /= ' ') s))
