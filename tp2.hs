--2.1
{-
lista = [x^2 | x<-[1..100]]
val = sum lista
-}

--2.2
{-
--a
pi_aprox n = [ (-1)^x / fromIntegral(2*x + 1) | x <- [0..n]]

aprox c = sum (pi_aprox c) * 4

--b
pi_aproxi n = [ (-1)^x / fromIntegral( (x + 1) ^ 2 ) | x <- [0..n]]

aproxi c =  sqrt ( sum (pi_aproxi c) * 12 )
-}

--2.4
{-
divprop :: Int -> [Int]
divprop n = [x | x<-[1..(n-1)] , n`mod`x == 0]
-}

--2.6
{-
pitagoricos :: Int -> [(Int ,Int ,Int)]
pitagoricos n = [(x,y,z) | x<-[0..n], y<-[0..n], z<-[0..n], x^2+y^2 == z^2, x/=y , y/=z, x/=z]
-}

--2.7
{-
divprop :: Int -> [Int]
primo::Int -> Bool
divprop n = [x | x<-[1..(n-1)] , n`mod`x == 0]
primo i = (sum (divprop i) == 1)
-}

--recursive

--2.10
{-
--a
myand:: [Bool] -> Bool
myand [x] = x
myand (x:xs) = x && myand xs

--b
myor:: [Bool] -> Bool
myor [x] = x
myor (x:xs) = x || myor xs

--c
myconcat:: [[a]] -> [a]
myconcat [] = []
myconcat (x:xs) = [x] ++ (myconcat [xs])
-}
