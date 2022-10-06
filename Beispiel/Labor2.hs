module Beispiel.Labor2 where

{-
der typ der Funktion ist Fractional a => a -> a
da 3.14 ein Ausdruck vom Typ Double/Float ist, muss dies als Fractional deklariert werden
-}
f :: Double -> Double
f x = 3.14 + x

f' :: Float -> Float
f' x = 3.14 + x

f'' :: Fractional a => a -> a
f'' x = 3.14 + x

{-
da x mit einem String/Char-liste verglichen wird, muss der Eingabe-Parameter x als String deklariert werden
der Rückgabewert ist 0, also ist es möglich diesen als Int zu deklarieren
-}
g :: Fractional a => String -> a
g x
  | x == "" = 0
  | otherwise = error "nein"

g' :: Num a => String -> a
g' x
  | x == "" = 0
  | otherwise = error "nein"

g'' :: String -> Int
g'' x
  | x == "" = 0
  | otherwise = error "nein"

{-
Aufgabe 2
die Typvariable a wird als fractional definiert und damit sind nur Datentypen möglich, die in dieser Typklasse sind.
Daher kann der Aufruf von aufruf :: Int nicht funktionieren
-}

func :: (Fractional a, Ord a) => a -> a -> a
func x y
  | x > y = x - y
  | otherwise = x + y

aufruf :: Double
aufruf = func 3.5 2.0

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

sum' :: Int -> Int -> Int
sum' x y
  | x > y = error "bitte x < y wählen"
  | x == y = y
  | otherwise = x + sum' (x + 1) y

anzahl :: Int -> Int -> Int
anzahl x y
  | x > y = error "bitte x < y wählen"
  | x == y = 1
  | otherwise = 1 + anzahl (x + 1) y

mittelwert :: Fractional a => Int -> Int -> a
mittelwert x y = fromIntegral (sum' x y) / fromIntegral (anzahl x y)

-- bonbon starts with the price of the first bonbon of 10c and the money (the bonbons "are counted" in the return of otherwise); see, that this is eta reduced
bonbon :: Integer -> Integer
bonbon = bonbon' 10
  where
    bonbon' :: Integer -> Integer -> Integer
    bonbon' priceOfNextBonbon money
      | money < priceOfNextBonbon || priceOfNextBonbon > 100 = 0
      | otherwise =
        1 + bonbon' (priceOfNextBonbon + 10) (money - priceOfNextBonbon)

-- bonbon' starts with the number of current bonbons of 0 and the money (the bonbons "are counted" )
bonbon' :: Integer -> Integer
bonbon' = bonbon'' 0
  where
    bonbon'' bonbons money
      | money >= ((bonbons + 1) * 10) && bonbons <= 10 = bonbon'' (bonbons + 1) (money - ((bonbons + 1) * 10))
      | otherwise = bonbons

-- bonbon'' starts with the price of the first bonbon of 10c and the money (this function doesnt stop buying bonbons but starts again buying the cheapest one up to the most expensive one)
bonbon'' :: Integer -> Integer
bonbon'' = bonbon3 10
  where
    bonbon3 priceOfNextBonbon money
      | money < priceOfNextBonbon = 0
      | otherwise =
        1 + bonbon3 ((priceOfNextBonbon `mod` 100) + 10) (money - priceOfNextBonbon)
