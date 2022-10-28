module Labor5.Labor5 where

-- durch .. muss zusätzlich zur Typklasse Num auch Enum angegeben werden
f1 :: (Enum a, Num a) => [a] -> [a]
f1 = (++ [1 .. 10])

f1' :: Num a => [a] -> [a]
f1' = (++ [1, 10])

-- durch die : (cons) -Funktion ist klar, dass der Rückgabetyp eine Liste sein muss
-- ebenfalls ist x dadurch von dem Typ, von welchem die Liste ist
-- da f2 von f1 abhängt, werden die Typen davon abgeleitet
f2 x y = x : f1 y

-- "kurz" steht hier f3 = f2 1, dadurch wird in f2 x durch 1 ersetzt und der Typ wird von f2 abgeleitet
f3 :: (Num a, Enum a) => [a] -> [a]
f3 =
  let a = f2
   in a 1

-- f4 x y = x + y
f4 :: Num a => a -> a -> a
f4 = (\x -> (\y -> x + y))

f5 :: Num a => a
f5 = (\x -> (\y -> x + y)) 2 3

-- durch die Funktion div muss die Typklasse der Typen Integral sein
f6 :: Integral a => a -> a
f6 = \x -> (4 * x + 2) `div` 2

-- Aufgabe 2
f7 = f6 5

-- Aufgabe 3
-- da x mit 3 auf Gleichheit geprüft wird, muss x daher mindestens einer Typklasse angehören, welche die Funktion == zur Verfügung stellt und Zahlen repräsentiert
-- der Ausdruck ((== 3) x) wird zu einem Boolean ausgewert
-- f _ 0 = 0
-- f x y = f ((== 3) x) y

-- Aufgabe 4
notenliste = [(1, 2.3), (2, 2.5), (3, 1.0)]

notenlisteMean :: [(Integer, Double)] -> Double
notenlisteMean l = notenlisteMean' l 0 0
  where
    notenlisteMean' [] akk akkLength = akk / akkLength
    notenlisteMean' ((_, note) : xs) akk akkLength = notenlisteMean' xs (akk + note) (akkLength + 1)

gibmatnummern :: [(Integer, Double)] -> [Integer]
gibmatnummern l = gibmatnummern' l []
  where
    gibmatnummern' [] akk = akk
    gibmatnummern' ((matnr, 1.0) : xs) akk = gibmatnummern' xs (matnr : akk)
    gibmatnummern' (_ : xs) akk = gibmatnummern' xs akk

gibmatnummern' :: [(Integer, Double)] -> [Integer]
gibmatnummern' l =
  map
    fst
    ( filter
        ( \(_, note) -> case note of
            (1.0) -> True
            _ -> False
        )
        l
    )
