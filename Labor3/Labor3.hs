module Labor3.Labor3 where

-- Aufgabe 1
-- Möglicher Typ des Tupels: Fractional, Char
f1 :: Fractional a => [(a, Char)]
f1 = [(1.3, 'a')]

-- der Typ ist: a -> [b]
f2 :: a -> [b]
f2 _ = []

-- f3 :: [Int] -> Int -> [Int]
-- im ersten pattern wird eine Liste erzeugt, dadurch muss der Rückgabetyp [a] sein
f3 :: (Eq a, Num a) => [a] -> a -> [a]
f3 x 0 = 0 : x
f3 x 1 = x

-- f3 _ _ = []

-- Aufgabe 2
-- im zweiten pattern kommt zwei mal x vor, das ist nicht erlaubt
-- f 0 x = x
-- f x x = []

-- es handelt sich um eine leere Liste. Der Typ kann sein [a]
-- eine Beispielfunktion wäre:
listy :: [a]
listy = []

-- Aufgabe 3
unterListe :: Int -> Int -> [a] -> [a]
unterListe n m l = droppy (n -1) (takey m l)

droppy :: Int -> [a] -> [a]
droppy _ [] = []
droppy 0 l = l
droppy n l
  | n >= lenghty l = []
droppy n (x : xs) = droppy (n - 1) xs

takey :: Int -> [a] -> [a]
takey _ [] = []
takey 0 l = []
takey n l
  | n >= lenghty l = l
takey n (x : xs) = x : takey (n - 1) xs

lenghty :: [a] -> Int
lenghty [] = 0
lenghty (_ : xs) = 1 + lenghty xs

-- Aufgabe 4
fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fibList :: Int -> [Int]
fibList 0 = [0]
fibList n = fibList (n - 1) ++ [fib n]

-- Aufgabe 5
listeZuPaar :: [(a, b)] -> ([a], [b])
listeZuPaar l = (firsts l, seconds l)

listeZuPaar' :: [(a, b)] -> ([a], [b])
listeZuPaar' l = (map first l, map second l)

listeZuPaar2 :: [(a, b)] -> ([a], [b])
listeZuPaar2 l = listeZuPaar2'' l ([], [])

listeZuPaar2'' :: [(a, b)] -> ([a], [b]) -> ([a], [b])
listeZuPaar2'' [] pairOfLists = pairOfLists
listeZuPaar2'' ((a, b) : xs) (as, bs) = listeZuPaar2'' xs (as ++ [a], bs ++ [b])

listeZuPaar3 :: [(a, b)] -> ([a], [b])
listeZuPaar3 [] = ([], [])
listeZuPaar3 ((a, b) : xs) = (a : fst rest, b : snd rest)
  where
    rest = listeZuPaar3 xs

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

filtery :: (a -> Bool) -> [a] -> [a]
filtery _ [] = []
filtery f (x : xs)
  | f x = x : filtery f xs
  | otherwise = filtery f xs
