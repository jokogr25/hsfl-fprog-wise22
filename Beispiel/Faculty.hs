module Beispiel.Faculty where

faculty :: Int -> Int
faculty 0 = 1
faculty n = n * faculty (n - 1)

facultyIf :: Int -> Int
facultyIf n =
  if n == 0
    then 1
    else n * facultyIf (n - 1)

facultyGuard :: Int -> Int
facultyGuard n
  | n == 0 = 1
  | n > 0 = n * facultyGuard (n - 1)
  | otherwise = error "negative Zahl"