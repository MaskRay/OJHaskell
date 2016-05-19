import Control.Monad
import qualified Data.Vector.Unboxed.Mutable as U

n = 100

nim = do
  a <- U.new $ n+1
  forM_ [0..n] $ \i -> do
    t2 <- i >= 2 & U.read a (i-2)
    t3 <- i >= 3 & U.read a (i-3)
    t5 <- i >= 5 & U.read a (i-5)
    U.write a i $ t2 || t3 || t5
  return a
  where
    x & y =
      if x
      then fmap not y
      else return False
    infixr 3 &

main = do
  cases <- readLn
  a <- nim
  replicateM cases $ do
    n <- readLn
    t <- U.read a n
    putStrLn $ if t then "First" else "Second"
