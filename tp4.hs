--4.1 lista de Fatores usando Lazy Evaluation
primos::[Int]
primos = crivo [2..]
crivo::[Int] -> [Int]
crivo(p:xs) = p : crivo [x | x<-xs, x `mod` p /= 0]
factores:: Int -> [Int]
factores 1 = []
factores n = primo : factores (n `div` primo)
    where primo = head(filter(\p -> n `mod` p == 0) primos)

--4.2
--a
calcPi1, calcPi2:: Int -> Double
num1, den1 :: [Double]
num1 = concat(map(\(x,y)->[x,y])(zip [4,4..] [-4,-4..]))
den1 = [1,3..]
inf1 :: [Double]
inf1 = zipWith (/) (num1) (den1)
calcPi1 n = sum(take n inf1)

--b
--den2 = [(a,a+1,a+2) | a<-[2,4..]]
--help = map (\(x,y,z) -> x*y*z) (den2)
--inf2 = zipWith (/) (num1) (help)
calcPi2 n = 3 + sum(take n inf2) where
  den2 = [(a,a+1,a+2) | a<-[2,4..]]
  num2 = concat(map(\(x,y)->[x,y])(zip [4,4..] [-4,-4..]))
  help = map (\(x,y,z) -> x*y*z) (den2)
  inf2 = zipWith (/) (num2) (help)
