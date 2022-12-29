{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}
module Labor8.Labor8 where

import Binary (Bin)
import Data.Graph (Tree)
import GhcPlugins (OverridingBool (Auto))

data Zahl
  = Eins
  | Zwei
  | Drei
  deriving (Show, Eq)

-- ohne deriving (Show) ist es nicht möglich, die Funktion plus im ghci auszuführen
--

-- nur zwei Fälle werden abgedeckt, patterns nicht genau genug
plus Eins Eins = Zwei
plus Zwei Eins = Drei

-- der Operator == ist nur verwendbar für Typen der Typklasse Eq
-- Lösung: deriving (Eq)
-- der Operator + ist nur verwendbar für Typen der Typklasse Num
-- Lösung: statt des Operators die Funktion plus verwenden

-- f :: [Zahl] -> Zahl -> Zahl
-- f xs y = if xs == [] then y else x + y
--   where
--     x :: Zahl
--     x = head xs

data Tree a
  = BinaryTree (BinaryTree a)

data BinaryTree a
  = Node a (BinaryTree a) (BinaryTree a)
  | Nil
  deriving (Show)

tree = Node 1 (Node 2 Nil Nil) (Node 3 Nil Nil)

checkNumInTree :: (Num a, Ord a) => a -> BinaryTree a -> Bool
checkNumInTree _ Nil = False
checkNumInTree num (Node value t1 t2) = (num == value) || (checkNumInTree num t1 || checkNumInTree num t2)

convertTreeToList :: BinaryTree a -> [a]
convertTreeToList Nil = []
convertTreeToList (Node val t1 t2) = [val] ++ convertTreeToList t1 ++ convertTreeToList t2

convertListToTree :: [a] -> BinaryTree a
convertListToTree [] = Nil
convertListToTree (x : xs) =
  let len = length xs
      l = take (len `div` 2) xs
      r = drop (length l) xs
   in Node x (convertListToTree l) (convertListToTree r)