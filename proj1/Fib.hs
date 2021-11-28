import BigNumber
import Control.Monad.State
    ( forM_, evalState, MonadState(get, put) )


--1.1
fibRec :: (Integral a) => a -> a
fibRec x = if x < 2 then x else fibRec (x-1) + fibRec (x-2)


--1.2

fibLista :: Integer -> Integer
fibLista n = flip evalState (0,1) $ do
  forM_ [0..(n-1)] $ \_ -> do
    (a,b) <- get
    put (b,a+b)
  (a,b) <- get
  return a

--1.3

fibListaInfinita :: (Num a) => Int -> a
fibListaInfinita n = fibs !! n where fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

--maybe (works)

fibLista2 :: (Integral a) => a -> a
fibLista2 x = last (map fibLista' [0 ..x])
    where
      fibLista' 0 = 0
      fibLista' 1 = 1
      fibLista' n = fibLista2 (n - 1) + fibLista2 (n - 2)

-- fibRecBN :: BigNumber -> BigNumber 
fibRecBN :: BigNumber -> BigNumber
fibRecBN (Pos x) = if checkBNSignal (subBN (Pos [1]) (Pos x)) then Pos x else somaBN (fibRecBN (subBN(Pos x)(Pos [2]))) (fibRecBN (subBN(Pos x)(Pos [1])))


fibListaBN :: BigNumber -> BigNumber 
fibListaBN (Pos x) = last (map fibListaBN'(zipWith (somaBN)
  where 
      fibListaBN' [0] = Pos [0]
      fibListaBN' [1] = Pos [1]
      fibListaBN' [n] = somaBN (fibListaBN (subBN(Pos n)(Pos [2]))) (fibListaBN (subBN(Pos n)(Pos [1])))


fibListaInfinitaBN :: BigNumber -> BigNumber 
fibListaInfinitaBN (Pos x) = fibs !! x where fibs = (Pos [0]) : (Pos [1]) : zipWith (somaBN ([a]) ([b])
