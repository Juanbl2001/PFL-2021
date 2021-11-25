import Control.Monad.State
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

fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
fibListaInfinita :: Int -> Integer
fibListaInfinita n = fibs !! n
