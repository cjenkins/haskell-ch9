module Cipher where

import Data.Char

rotateCharRight :: Char -> Int -> Char
rotateCharRight c steps
  | steps == 0 = c
  | c == 'z' = rotateCharRight 'a' (steps - 1)
  | c == 'Z' = rotateCharRight 'A' (steps - 1)
  | otherwise = rotateCharRight (succ c) (steps - 1)

rotateCharLeft :: Char -> Int -> Char
rotateCharLeft c steps
  | steps == 0 = c
  | c == 'a' = rotateCharLeft 'z' (steps - 1)
  | c == 'A' = rotateCharLeft 'Z' (steps - 1)
  | otherwise = rotateCharLeft (pred c) (steps - 1)

caeser :: String -> Int -> String
caeser s steps = map (\c -> rotateCharRight c steps) s

uncaeser :: String -> Int -> String
uncaeser s steps = map (\c -> rotateCharLeft c steps) s
