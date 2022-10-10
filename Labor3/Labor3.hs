module Labor3.Labor3 where

-- Aufgabe 1
-- hat keinen Typ, da Liste von verschiedenen Typen nicht möglich ist
-- [1.3, 'a']

-- es handelt sich um eine leere Liste. Der Typ ist [a]
-- eine Beispielfunktion wäre:
listy :: [a]
listy = []

-- Aufgabe 2
unterListe :: Int -> Int -> [a] -> [a]
unterListe n m l = takey (m - n + 1) (droppy (n -1) l)

droppy :: Int -> [a] -> [a]
droppy _ [] = []
droppy 0 l = l
droppy n l
  | n >= lenghty l = []
droppy n (x : xs) = droppy (n -1) xs

takey :: Int -> [a] -> [a]
takey _ [] = []
takey 0 l = []
takey n l
  | n >= lenghty l = l
takey n (x : xs) = x : takey (n - 1) xs

lenghty :: [a] -> Int
lenghty [] = 0
lenghty (_ : xs) = 1 + lenghty xs

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fibList :: Int -> [Int]
fibList 0 = [0]
fibList n = fibList (n - 1) ++ [fib n]

listeZuPaar :: [(a, b)] -> ([a], [b])
listeZuPaar l = (firsts l, seconds l)

listeZuPaar' :: [(a, b)] -> ([a], [b])
listeZuPaar' l = (map first l, map second l)

listeZuPaar2 :: [(a, b)] -> ([a], [b])
listeZuPaar2 l = listeZuPaar2'' l ([], [])

listeZuPaar2'' :: [(a, b)] -> ([a], [b]) -> ([a], [b])
listeZuPaar2'' [] pairOfLists = pairOfLists
listeZuPaar2'' ((a, b) : xs) (as, bs) = listeZuPaar2'' xs (as ++ [a], bs ++ [b])

firsts :: [(a, b)] -> [a]
firsts [] = []
firsts ((a, _) : xs) = a : firsts xs

seconds :: [(a, b)] -> [b]
seconds [] = []
seconds ((_, b) : xs) = b : seconds xs

first :: (a, b) -> a
first (a, _) = a

second :: (a, b) -> b
second (_, b) = b