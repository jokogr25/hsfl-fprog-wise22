{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}
module Labor6.Labor6 where

-- Aufgabe 1
-- mapWithFoldr :: (a -> b) -> [a] -> [b]
-- mapWithFoldr f l = foldr (\x xs -> (f x) : xs) [] l

-- Aufgabe 2
take_While :: (Int -> Bool) -> [Int] -> [Int]
take_While pred l = take_While' [] pred l
  where
    take_While' :: [Int] -> (Int -> Bool) -> [Int] -> [Int]
    take_While' akk _ [] = akk
    take_While' akk pred (x : xs) =
      if pred x
        then take_While' (akk ++ [x]) pred xs
        else akk

take_WhileWithFoldr :: (Int -> Bool) -> [Int] -> [Int]
take_WhileWithFoldr pred = foldr (\x xs -> if pred x then xs ++ [x] else []) []

--