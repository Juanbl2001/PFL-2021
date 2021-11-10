--1.1
fibRec :: (Integral a) => a -> a
fibRec x = if x < 2 then x else fibRec (x-1) + fibRec (x-2)

--1.2
