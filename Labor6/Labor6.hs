{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Labor6.Labor6 where

import Data.Char (isLower, toLower, toUpper)

-- Aufgabe 1
-- mapWithFoldr :: (a -> b) -> [a] -> [b]
-- mapWithFoldr f l = foldr (\x xs -> (f x) : xs) [] l

-- Aufgabe 2
takeWhile :: (Int -> Bool) -> [Int] -> [Int]
takeWhile pred l = take_While' [] pred l
  where
    take_While' :: [Int] -> (Int -> Bool) -> [Int] -> [Int]
    take_While' akk _ [] = akk
    take_While' akk pred (x : xs) =
      if pred x
        then take_While' (akk ++ [x]) pred xs
        else akk

takeWhileWithFoldr :: (Int -> Bool) -> [Int] -> [Int]
takeWhileWithFoldr pred = foldr (\x xs -> if pred x then xs ++ [x] else []) []

-- Aufgabe 3
makeCamelCase :: [Char] -> [Char]
makeCamelCase text = concatMap wordToCamelCase (words text)

wordToCamelCase :: [Char] -> [Char]
wordToCamelCase [] = ""
wordToCamelCase (x : xs) =
  ( if Data.Char.isLower x
      then Data.Char.toUpper x
      else x
  ) :
  map Data.Char.toLower xs

wordToCamelCaseFoldr :: [Char] -> [Char]
wordToCamelCaseFoldr [] = ""
wordToCamelCaseFoldr (x : xs) =
  ( if Data.Char.isLower x
      then Data.Char.toUpper x
      else x
  ) :
  foldr (\x l -> Data.Char.toLower x : l) [] xs

-- Aufgabe 4
pruefKlammern :: [Char] -> Bool
pruefKlammern = pruefKlammern' 0
  where
    pruefKlammern' summeKlammern [] = summeKlammern == 0
    pruefKlammern' summeKlammern (x : xs)
      | x == ')' && summeKlammern == 0 = False
      | x == '(' = pruefKlammern' (summeKlammern + 1) xs
      | x == ')' = pruefKlammern' (summeKlammern - 1) xs
      | otherwise = pruefKlammern' summeKlammern xs
