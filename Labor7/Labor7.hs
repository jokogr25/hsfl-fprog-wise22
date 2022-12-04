module Labor7.Labor7 where

g :: (Num a, Eq a) => [a] -> a -> [a] -> a
g (x : xs) y z = x + y
g xs y z = if xs == z then y else head xs + y

-- Aufgabe 2
-- mult x = \y -> x * y
-- call by value
-- parameter werden berechnet, bevor sie an die Funktion übergeben werden
-- (\x -> mult x x) (2+3)
-- > (\x -> mult x x) 5
-- > mult 5 5
-- > (\y -> 5 * y) 5
-- > (5 * 5)
-- > 25

-- call by name
-- parameter werden ohne berechnet zu werden, an die Funktion übergeben
-- (\x -> mult x x) (2+3)
-- > mult (2+3) (2+3)
-- > (\y -> (2+3) * y) (2+3)
-- > (2+3) * (2+3)
-- > 5 * (2+3)
-- > 5 * 5
-- > 25

-- call by need
-- call by name mit sharing (lazy evaluation)
-- bei der call by name Auswertung berechnete Werte werden für spätere Verwendung gespeichert
-- (\x -> mult x x) (2+3)
-- > mult x x (beide x sind an (2+3) gebunden, teilen sich also diesen Ausdruck)
-- > (\y -> x * y) x (beide x sind an (2+3) gebunden, teilen sich also diesen Ausdruck)
-- > (x * y) (x und y sind an (2+3) gebunden)
-- > 5 * 5
-- > 25

-- Aufgabe 3
-- add 2 3
-- > inc (add (dec 2) 3)
-- > inc (add (2-1) 3)
-- > inc (add 1 3)
-- > inc (inc (add (dec 1) 3))
-- > inc (inc (add (1-1) 3))
-- > inc (inc (add 0 3))
-- > inc (inc 3)
-- > inc (3+1)
-- > (3+1)+1
-- > 4+1
-- > 5

-- add' 2 3
-- > add' (dec 2) (inc 3)
-- > add' (2-1) (inc 3)
-- > add' 1 (inc 3)
-- > add' (dec 1) (inc (inc 3))
-- > add' (1-1) (inc (inc 3))
-- > add' 0 (inc (inc 3))
-- > inc (inc 3)
-- > inc (3+1)
-- > (3+1)+1
-- > 4+1
-- > 5