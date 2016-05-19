import Control.Monad
import Data.Functor
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV

n = 23

nim = do
  a <- MV.new $ n*n
  let
    f x y
      | x < 0 || y < 0 = return False
      | otherwise = not <$> MV.unsafeRead a (n*x+y)
  forM_ [0..n-1+n-1] $ \i ->
    forM_ [0..n-1] $ \j ->
      when (0 <= i-j && i-j < n) $ do
        t0 <- f (i-j-2) (j-1)
        t1 <- f (i-j-1) (j-2)
        t2 <- f (i-j-2) (j+1)
        t3 <- f (i-j+1) (j-2)
        MV.unsafeWrite a (n*(i-j)+j) $ t0 || t1 || t2 || t3
  V.unsafeFreeze a

main = do
  a <- nim
  cases <- readLn
  replicateM cases $ do
    [x, y] <- fmap (map (pred . read) . words) getLine :: IO [Int]
    putStrLn $ if a V.! (n*x+y) then "First" else "Second"
