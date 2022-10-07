module Beispiel.BreakLock where

breakLock :: Bool
breakLock = breakLock' 0
  where
    breakLock' counter
      | counter == 125 = False
      | tryCombination (counter `div` 25) ((counter `div` 5) `mod` 5) (counter `mod` 5) = True
      | otherwise = breakLock' (counter + 1)

tryCombination :: Integer -> Integer -> Integer -> Bool
tryCombination rad1 rad2 rad3
  | rad1 == 4 && rad2 == 4 && rad3 == 4 = True
  | otherwise = False
