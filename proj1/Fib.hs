
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
