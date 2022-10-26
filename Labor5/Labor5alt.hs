module Labor5.Labor5alt where

-- Aufgabe 1
f :: Ord a => a -> a -> Bool
f = (<)

f' :: Integral a => [a] -> [a]
f' = (++ [1 .. 10])

f'' :: Num a => a -> a
f'' x = (\x -> x + 1) x

-- Aufgabe 2
-- f x y = (== 3) x y
-- (== 3) ist ein Ausdruck, welcher nur einen weiteren Parameter entgegennehmen kann;

-- g (x:xs) = (\x -> (++) [x] xs)
-- funktioniert

-- Aufgabe 3
potenz :: Integer -> Integer -> Integer
potenz _ 0 = 1
potenz base power = base * potenz base (power -1)

potenz' :: Integer -> Integer -> Integer
potenz' base power = potenz'' power 1
  where
    potenz'' 0 akk = akk
    potenz'' power akk = potenz'' (power -1) (akk * base)
