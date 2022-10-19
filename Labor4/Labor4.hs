{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}
module Labor4.Labor4 where

{-
Akkumulator-Technik
- zusätzlicher Parameter als Akkumulator
  für jeden rekursiven Aufruf
- der rekursive Ausfruf muss kein Teilausdruck vom Rumpf sein
- das Ergebnis wird schrittweise im Akkumulator aufgebaut
--> Der Trick ist, dass der nächste Funktionsaufruf den vorherigen komplett ersetzt; bei der Rekursion, wo der nächste Aufruf ein Teilausdruck "wird", wird dieser Aufruf auf den Stack drauf gepackt
-}

facc 0 akk = akk
facc n akk = facc (n -1) (n * akk)

facci 0 akk = akk
facci n akk =
  let akk' = n * akk'
   in seq akk' (facci (n - 1) akk')

-- Aufgabe 1
f :: (Eq a, Num a) => a -> a -> [a] -> [a]
f 0 x y = y ++ [x]
f x y z = x : y : z

-- Aufgabe 2
-- f1 0 3 ergibt 4
-- f1 2 3 ergibt (2 * (3 + 3)) = 12
f1 0 x =
  let f2 x y = x + y
   in f2 x 1
f1 x y = x * f2 y
  where
    f2 x = x + y

f_rek 1 = 1
f_rek n = (n * n) + f_rek (n -1)

f_rekGuard n
  | n == 1 = 1
  | otherwise = (n * n) + f_rek (n -1)

f_iter :: Int -> Int
f_iter =
  let f_iter' akk 1 = akk
      f_iter' akk n = f_iter' (akk + n ^ 2) (n - 1)
   in f_iter' 1

f_iter' :: Int -> Int
f_iter' =
  let f_iter'' akk m
        | m == 1 = akk
        | otherwise = f_iter'' (akk + m ^ 2) (m - 1)
   in f_iter'' 1

fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fibGuard n
  | n == 0 = 0
  | n == 1 = 1
  | otherwise = fib (n - 1) + fib (n - 2)

fibIter :: Int -> Int
fibIter n =
  let fibIter' m akk1 akk2
        | m == n = akk1
        | otherwise = fibIter' (m + 1) (akk1 + akk2) akk1
   in fibIter' 0 0 1

fibIter' :: Int -> Int
fibIter' =
  let fibIter' akk1 akk2 0 = akk1
      fibIter' akk1 akk2 m = fibIter' akk2 (akk1 + akk2) (m - 1)
   in fibIter' 0 1