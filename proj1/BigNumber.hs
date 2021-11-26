import Data.Char

--2.1
data BigNumber = Pos [Int]
                |Neg [Int]
                deriving (Eq, Show)

--2.2
isNeg :: String -> String
isNeg xs = if head xs == '-' then drop 1 xs else xs

checkSignal :: String -> Bool
checkSignal xs = if head xs == '-' then True else False

scanner :: String -> BigNumber
scanner xs = if checkSignal xs then Neg (reverse (map digitToInt (isNeg xs))) else Pos (reverse (map digitToInt (isNeg xs)))

--2.3
output :: BigNumber -> String 
output (Pos x) = map intToDigit (reverse x)
output (Neg x) =  "-" ++ map intToDigit (reverse x)


--2.4
auxSoma :: Int -> [Int] -> [Int] -> [Int]
auxSoma 0 [] [] = []
auxSoma n [] [] = [n]
auxSoma n xs [] = auxSoma n xs [0]
auxSoma n [] xs = auxSoma n [0] xs
auxSoma n (x:xs) (y:ys) = r : auxSoma q xs ys
    where (q,r) = quotRem (x+y+n) 10

sumList :: [Int] -> [Int] -> [Int]
sumList = auxSoma 0
 
somaBN :: BigNumber -> BigNumber -> BigNumber
somaBN (Neg x) (Neg y) = Neg (sumList x y)
somaBN (Neg x) (Pos y) = Neg (sumList x y)
somaBN (Pos x) (Neg y) = Pos (sumList x y)
somaBN (Pos x) (Pos y) = Pos (sumList x y)