module Testat.Testat where

-- Aufgabe 1
-- das Programm ist fehlerhaft, da durch die Verwendung von (==) der Operator als Funktion "interpretiert" wird und dadurch seine Präzedenz erhöht wird. D. h. es werden zwei Argumente erwartet - [(1.3, 'a')] und x. Die Klammerung ist also folgende: ((==) [(1.3, 'a')] x) ++ y . Da ((==) [(1.3, 'a')] x) einen Bool zurückgibt, kann dieser Ausdruck nicht mit ++ y ausgeführt werden, da ++ zwei Listen als Parameter erwartet.
-- Wenn man : verwenden würde, wäre das Programm  korrekt
-- f x y = (==) [(1.3, 'a')] x ++ y
-- f x y = (==) [(1.3, 'a')] x : y

-- Aufgabe 2
g =
  let f :: Num a => a -> [a] -> [a]
      f = \x y -> 1 : x : y
   in f 3 [2.5, 4]

-- Aufgabe 3
reverseIter :: [a] -> [a]
reverseIter l =
  let reverseIter' :: [a] -> [a] -> [a]
      reverseIter' [] reversed = reversed
      reverseIter' (x : xs) reversed = reverseIter' xs (x : reversed)
   in reverseIter' l []

-- Aufgabe 4
liste :: [(Integer, [(String, Double)])]
liste =
  [ ( 1,
      [ ("sprog", 1.0),
        ("fprog", 3.0),
        ("mathe2", 1.0)
      ]
    ),
    ( 2,
      [ ("sprog", 2.0),
        ("fprog", 4.0),
        ("mathe2", 2.7)
      ]
    )
  ]

gibNoten :: Integer -> [(Integer, [(String, Double)])] -> [(String, Double)]
gibNoten _ [] = []
gibNoten matrikelNummer ((matrikelNr, liste) : xs)
  | matrikelNummer == matrikelNr = liste
  | otherwise = gibNoten matrikelNummer xs

rechneSchnitt :: [(String, Double)] -> Double
rechneSchnitt = rechneSchnitt' 0 0
  where
    rechneSchnitt' notenSumme anzahl [] = notenSumme
    rechneSchnitt' notenSumme anzahl ((_, note) : xs) = rechneSchnitt' (notenSumme + note) (anzahl + 1) xs

gibNotenSchnitt mnr nl = rechneSchnitt (gibNoten mnr nl)
