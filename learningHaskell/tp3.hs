--3.2 Usando foldl, defina uma função dec2int :: [Int] → Int que converte uma
--lista de dígitos decimais num inteiro.
dec2int :: [Int] -> Int
dec2int = foldl addDigit 0
    where addDigit num d = 10*num + d

--3.3
miZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
miZipWith f [] [] = []
miZipWith f (x:xs) [] = []
miZipWith f [] (y:ys) = []
miZipWith f (x:xs) (y:ys) = f x y: miZipWith f xs ys

--3.4
insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x (y:ys) | x < y     = x:y:ys
                | otherwise = y:(insert x ys)

isort :: Ord a => [a] -> [a]
isort [] = []
isort (x:xs) = insert x (isort xs)
