module DataChar where

import Data.Char

--2
filterUppers :: String -> String
filterUppers s = filter (\x -> isUpper x) s

--3
capitalize :: String -> String
capitalize s = (toUpper (head s)): tail s

--4
yell :: String -> String
yell s
  | length s == 0 = ""
  | otherwise = (toUpper (head s)): yell (tail s)

--5
firstCap :: String -> Char
firstCap s = (toUpper (head s))

--6
firstCapComposed s = toUpper . head s

firstCapPointFree = toUpper . head
