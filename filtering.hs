module Filtering where

--1
multiplesOf3 :: [Integer] -> [Integer]
multiplesOf3 l = filter (\x  -> (rem x 3) == 0) l

--2
lengthOfMultiplesOf3 = length . multiplesOf3

--3

stringBreaker :: Char -> String -> [String]
stringBreaker breaker input
  | elem breaker input == False = [input]
  | otherwise = [takeWhile (\c -> c /= breaker) input] ++
    stringBreaker breaker (tail (dropWhile (\c -> c /= breaker) input))

myFilter s = filter (\x -> not (elem x ["the", "a", "an"])) (stringBreaker ' ' s)
