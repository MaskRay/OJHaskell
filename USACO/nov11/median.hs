{-# LANGUAGE BangPatterns #-}
import Control.Applicative
import Control.Monad
import Control.Monad.State
import Data.Array.IO
import Data.Bits
import qualified Data.ByteString.Char8 as B
import Data.Char
import Data.Maybe

parseInput = do
  n <- readInt
  m <- readInt
  replicateM n $ (\ !x -> if x >= m then 1 else (-1)) <$> readInt
  where
    readInt = state $ fromJust . B.readInt . B.dropWhile isSpace

add :: (Ix i, Bits i, Num e, MArray a e m) => a i e -> i -> e -> m ()
add !fenwick !x !delta = getBounds fenwick >>= \ !bnds -> forM_ (takeWhile (inRange bnds) (iterate (\ !i -> i .|. (i+1)) x)) $ \ !i -> readArray fenwick i >>= \ !fi -> writeArray fenwick i (fi+delta)

getSum :: (Ix i, Bits i, Num e, MArray a e m) => a i e -> i -> m e
getSum !fenwick !x = sum `liftM` mapM (readArray fenwick . (subtract 1)) (takeWhile (>0) $ iterate (\ !i -> i .&. (i-1)) x)

main = do
  as <- evalState parseInput <$> B.getContents
  let n = length as
  fenwick <- newArray (0,2*n) 0 :: IO (IOUArray Int Int)
  add fenwick (0+n) 1
  (_,res) <- foldM (\(s,res) a ->
          let !s' = s+a
          in getSum fenwick (s'+1) >>= \delta -> add fenwick s' 1 >> return (s',res+delta)
        ) (0+n,0) as
  print res