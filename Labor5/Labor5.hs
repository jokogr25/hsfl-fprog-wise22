module Labor5.Labor5 where

f6 = \x -> (4 * x + 2) `div` 2

-- Aufgabe 4
notenliste = [(1, 2.3), (2, 2.5), (3, 1.0)]

notenlisteMean :: [(Integer, Double)] -> Double
notenlisteMean l = notenlisteMean' l 0 0
  where
    notenlisteMean' [] akk akkLength = akk / fromIntegral akkLength
    notenlisteMean' ((_, note) : xs) akk akkLength = notenlisteMean' xs (akk + note) (akkLength + 1)

gibmatnummern :: [(Integer, Double)] -> [Integer]
gibmatnummern l = gibmatnummern' l []
    where
        gibmatnummern' [] akk = akk
        gibmatnummern' ((matnr, 1.0) : xs) akk = gibmatnummern' xs (matnr : akk)
        gibmatnummern' (_ : xs) akk = gibmatnummern' xs akk
