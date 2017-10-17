module MoreBottoms where

import Data.Bool

--1 Bottom
--2 Value
--3 Bottom
--4 Returns a list of Bool where value is True if string has vowel in that spot
itIsMystery :: [Char] -> [Bool]
itIsMystery xs = map (\x -> elem x "aeiou") xs
--5a List of squared values from 1 - 10
--5b [1, 10, 20]
--5c [15, 15, 15]
numberSix = map (\x -> bool x (negate x) (x == 3)) [1..10]
