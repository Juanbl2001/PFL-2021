{-# LANGUAGE DataKinds #-}
import Data.Char
import Distribution.Simple.Command (commandParseArgs)
import Text.XHtml (clear)

--2.1
data BigNumber = Pos [Int]
                |Neg [Int]
                deriving (Eq, Show)

--2.2
isNeg :: String -> String
isNeg xs = if head xs == '-' then drop 1 xs else xs

checkSignal :: String -> Bool
checkSignal xs = head xs == '-'

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

-- change Num to Integral because we need to work with integers
sub :: (Integral a) => [a] -> [a] -> a -> [a]
--        we need to sub a carry now ^
-- these base cases break down if carry is non-zero
sub [] x c
    -- if carry is zero we're fine
    | c == 0    = x
    -- just sub the carry in as a digit
    | otherwise = sub [c] x 0
-- same applies here
sub x [] c
    | c == 0    = x
    | otherwise = sub x [c] 0
sub (x:xs) (y:ys) c = dig : sub xs ys rst
    where sum = if x >= y then x - y - c else x+10 - y - c --x + y + c    -- find the sum of the digits plus the carry

          -- these two lines can also be written as (rst, dig) = sum `divMod` 10
          dig = sum `mod` 10 -- get the last digit
          rst = if x >= y then 0 else 1 -- get the rest of the digits (the new carry)

sumList :: [Int] -> [Int] -> [Int]
sumList = auxSoma 0

subList :: [Int] -> [Int] -> [Int]
subList x y = sub x y 0

compareList :: [Int] -> [Int] -> Bool
compareList a b = a==b || compareList2 a b

compareList2 :: [Int] -> [Int] -> Bool
compareList2 (x:xs) (y:ys) | x == y = compareList2 xs ys
                          | x > y = True
                          |otherwise = False

clearZero :: [Int] -> [Int]
clearZero xs = if last xs == 0 && length xs /= 1 then clearZero (take (length xs - 1) xs) else xs

somaBN :: BigNumber -> BigNumber -> BigNumber
somaBN (Neg x) (Neg y) = Neg (reverse (sumList (reverse x) (reverse y)))
somaBN (Neg x) (Pos y) = if length x > length y || length x == length y && compareList x y then Neg (reverse(clearZero (subList (reverse x) (reverse y)))) else Pos (reverse(clearZero(subList (reverse y)(reverse x))))
somaBN (Pos x) (Neg y) = if length x > length y || length x == length y && compareList x y then Pos (reverse(clearZero (subList (reverse x) (reverse y)))) else Neg (reverse(clearZero(subList (reverse y) (reverse x))))
somaBN (Pos x) (Pos y) = Pos (reverse (sumList (reverse x) (reverse y)))



--2.5
subBN :: BigNumber -> BigNumber -> BigNumber
subBN (Neg x) (Neg y) = if length x > length y || length x == length y && compareList x y then Neg (reverse(clearZero (subList (reverse x) (reverse y)))) else Pos (reverse(clearZero(subList (reverse y) (reverse x))))
subBN (Neg x) (Pos y) =  Neg (reverse(sumList (reverse x) (reverse y)))
subBN (Pos x) (Neg y) = Pos (reverse(sumList (reverse x) (reverse y)))
subBN (Pos x) (Pos y) = if length x > length y || length x == length y && compareList x y then Pos (reverse(clearZero(subList (reverse x) (reverse y)))) else Neg (reverse(clearZero(subList (reverse y) (reverse x))))

--2.6

listOfN :: Int -> [Int] --creates lists with n zeros
listOfN n = replicate n 0

cartProd :: Num a => [a] -> [a] -> [a] --multiply each number by each other and store in position of sum of 10´s
cartProd xs ys = let n = listOfN (length xs + length ys)
                in [x*y | x <- xs, y <- ys] --ex: 700*10 -> store in position 2+1 (3), finally use the sumList and add to zero

auxMult :: [Int] -> [Int] -> [Int] -> [Int] -> [Int]
auxMult f x y n = if length y > length n || length n == length y && compareList y n then auxMult (reverse (sumList (reverse f) (reverse x))) x y (sumList (reverse n) [1]) else f --Sum value to itself y times, n is a counter

mulBN :: BigNumber -> BigNumber -> BigNumber
--mulBN (Pos x) (Pos y) = Pos (sumList (zipWith (*) x y) (zipWith (*) (reverse x) y))
mulBN (Neg x) (Neg y) = Pos (auxMult (listOfN 1) y x (listOfN 1))
mulBN (Pos x) (Pos y) = if length y > length x || length x == length y && compareList y x then Pos (auxMult (listOfN 1) y x (listOfN 1)) else Pos (auxMult (listOfN 1) x y (listOfN 1))
mulBN (Pos x) (Neg y) = Neg (auxMult (listOfN 1) y x (listOfN 1))
mulBN (Neg x) (Pos y) = Neg (auxMult (listOfN 1) y x (listOfN 1))



auxDiv :: [Int] -> [Int] -> [Int] -> [[Int]]
auxDiv x y n = if length x > length y || length x == length y && compareList x y then auxDiv (reverse(clearZero(subList (reverse x) (reverse y)))) y (sumList (reverse n) [1])  else [n,x] --Sum value to itself y times, n is a counter

divBN :: BigNumber -> BigNumber -> (BigNumber, BigNumber)
divBN (Pos x) (Pos y) = (Pos (head t), Pos (t !! 1)) where t = auxDiv x y (listOfN 1)

main :: IO ()
main = do
print"This is div:"
print(divBN (Pos [1,2,3]) (Pos [1,2]))
print(divBN (Pos [2,4,3]) (Pos [1,6]))
print(divBN (Pos [1,2,1]) (Pos [1,1]))
print(divBN (Pos [1,2,0]) (Pos [2,4]))
print"This is mul:"
print(mulBN (Pos [9,1,1]) (Pos [1,1,1]))
print(mulBN (Pos [6,2]) (Pos [7,4]))