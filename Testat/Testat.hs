module Testat.Testat where

-- das Programm ist fehlerhaft, da durch das voranstellen von (==) die Präzedenz des "Operators" erhöht wird und dadurch die "Klammerung" geändert wird
-- f x y = (==) [(1.3, 'a')] x ++ y

g =
  let f :: Num a => a -> [a] -> [a]
      f = \x y -> 1 : x : y
   in f 3 [2.5, 4]

reverseIter :: [a] -> [a]
reverseIter l =
  let reverseIter' :: [a] -> [a] -> [a]
      reverseIter' [] reversed = reversed
      reverseIter' (x : xs) reversed = reverseIter' xs (x : reversed)
   in reverseIter' l []
