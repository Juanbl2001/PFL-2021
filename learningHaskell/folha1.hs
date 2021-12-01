import Control.Exception (evaluate)
--IN
--3
in3a :: Integer
in3a = (3 - (-2))+1

in3b :: Double
in3b = (4 / (-2))-(3*6)

--4
-- em papel

--5
double:: Num a => a -> a
double x = 2*x

nand :: Bool -> Bool -> Bool
nand a b = not (a && b)

funcX :: Floating a => a -> a -> a -> a -> a
funcX x a b c = (a*(t^2))+(b*t)+c where t = cos x+sin x

--6
half :: Fractional a => a -> a
half x = x/2

xor :: Bool -> Bool -> Bool
xor x y = (x && not y) || (not x && y)

cbrt :: Floating a => a -> a
cbrt x = x**(1/3)

--13
--a
--Function f computes the sign of a number x, and returns 1, -1 or
--0, respectively, if x is positive, negative or null.
--b
f :: (Ord a, Num a, Integral b) => a -> b
f 0 = 0
f x = if x > 0 then 1 else -1

--FT
--3
myswap :: (Int,Int) -> (Int,Int)
myswap (a,b) = (b,a)

--4

distance2:: Floating a => (a,a) -> (a,a) -> a
distance2 (ax,ay) (bx,by) = sqrt ((ax-bx)**2+(ay-by)**2)

distanceInf :: (Ord a, Num a) => (a, a) -> (a, a) -> a
distanceInf (x1,y1) (x2,y2) = max (abs(x1-x2)) (abs(y1-y2))

--9
{-
a) []
b) [[1,2],[],[3],[4,5]]
c) [[1,2],[],[3],[4,5]]
d) 3
e) error
f) [[1,2],[3,4,5]]
g) [[],[],[]]

-}

 --10
 --a
 --Function f returns a pair with the third element of the input list l
--and the sublist of l starting at the fourth element.

--b
f' :: [a]-> (a,[a])
f' x = (x !! 2, drop 3 x)

--11
--a
evaluateLength :: [a] -> String
evaluateLength a
  | length a < 2 = "short"
  | length a < 4 = "medium-sized"
  | otherwise = "long"

evaluateLength' :: [a] -> String 
evaluateLength' [] = "short"
evaluateLength' [_] = "short"
evaluateLength' [_,_] = "medium-sized"
evaluateLength' [_,_,_] = "medium-sized"
evaluateLength' _ = "long"

--21
{-
i) zip [1,2] "abc":: [(Integer, Char)]
ii) [[1],[2]] :: [[Int]]
iii) [succ 'a'] :: [Char]
iv) [1,2,3,4,5.5] :: [Float]
v) [1,2] == [1,2] :: Bool
vi) zip (zip "abc""abc")"abc":: [((Char,Char),Char)]

b)
b)
i) zip [1,2] "abc":: Num a =>[(a, Char)]
ii) [[1],[2]] :: Num a =>[[a]]
iii) [succ 'a'] :: [Char]
iv) [1,2,3,4,5.5] :: Fractional a =>[a]
v) [1,2] == [1,2] :: Bool
vi) zip (zip "abc""abc")"abc":: [((Char,Char),Char)]
-}

--24
func24 :: (a,b,a)-> (b,a,b)
func24 (x,y,z) = (y,x,y)