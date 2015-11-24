import Control.Monad
import Data.Array.IO
import Data.IORef

main = do
  [n,k,hi] <- (map read . words) `fmap` getLine :: IO [Int]
  [near,res] <- replicateM 2 $ newIORef 0
  [c1,c2,pos] <- replicateM 3 $ newArray (0,k-1) 0 :: IO [IOArray Int Int]
  forM_ [1..n] $ \i -> do
    [c,v] <- (map read . words) `fmap` getLine :: IO [Int]
    when (v <= hi) $ writeIORef near i
    near' <- readIORef near
    pos' <- readArray pos c
    when (pos' <= near') $ writeArray c2 c =<< readArray c1 c
    readArray c2 c >>= \x -> modifyIORef res (+x)
    readArray c1 c >>= \x -> writeArray c1 c (x+1)
    writeArray pos c i
  readIORef res >>= print