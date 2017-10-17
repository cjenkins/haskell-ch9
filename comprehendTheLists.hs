module ComprehendThyLists where

--1 Only evens
--2 Tuples of all possible pairs where first element in tuple is < 50, second is > 50
--3 Same as 2 expect we only take the first 5 tuples

--SquareCube

mySqr = [x^2 | x <- [1..5]]
myCube = [x^3 | x <- [1..5]]

myTuples = [(x, y) | x <- mySqr, y <- myCube]
myTuples2 = [(x, y) | x <- mySqr, y <- myCube, x < 50, y < 50]
myTuples3 = length myTuples2
