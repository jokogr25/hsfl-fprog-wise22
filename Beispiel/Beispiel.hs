module Beispiel where

import Test (lol)

greet :: String -> String
greet "joscha" = "Moin Joscha"
greet "Marie" = "Hallo Marie"
greet a = "Was geht ab " ++ a

mapso :: (a -> b) -> [a] -> [b]
mapso = map

reverso :: [a] -> [a]
reverso = foldarrr addi []

addi :: a -> [a] -> [a]
addi a [] = [a]
addi el (x : xs) = x : addi el xs

foldarrr :: (a -> b -> b) -> b -> [a] -> b
foldarrr _ acc [] = acc
foldarrr f acc (x : xs) = f x (foldarrr f acc xs)
