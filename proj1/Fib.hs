--1.1
fibRec :: (Integral a) => a -> a
fibRec x = if x < 2 then x else fibRec (x-1) + fibRec (x-2)


--1.3

fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
fibLista n = fibs !! n

{-
--1.3
fibListaInfinita ::  (Integral a) => a -> a
fibListaInfinita x = map fibListaInfinita' [0 ..] !! x
    where
      fibListaInfinita' 0 = 0
      fibListaInfinita' 1 = 1
      fibListaInfinita' x = fibListaInfinita (x - 1) + fibListaInfinita (x - 2)
-}
