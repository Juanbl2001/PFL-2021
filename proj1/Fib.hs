--1.1
fibRec :: (Integral a) => a -> a
fibRec x = if x < 2 then x else fibRec (x-1) + fibRec (x-2)

--1.2
fibLista :: (Integral a) => a -> a
fibLista x = map fibLista' [0 ..] !! x
    where
      fibLista' 0 = 0
      fibLista' 1 = 1
      fibLista' n = fibLista (n - 1) + fibLista (n - 2)