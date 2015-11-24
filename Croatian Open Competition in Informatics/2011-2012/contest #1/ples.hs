{-# LANGUAGE BangPatterns, ScopedTypeVariables #-}
import Control.Applicative
import Control.Monad
import Control.Monad.State
import Data.Char
import Data.Int
import Data.List
import Data.Maybe
import qualified Data.ByteString.Char8 as B

parse = do
  n <- readInt
  xs <- replicateM n readInt
  ys <- replicateM n readInt
  let !xs0 = map negate $ filter (<0) xs
      !xs1 = filter (>0) xs
      !ys0 = map negate $ filter (<0) ys
      !ys1 = filter (>0) ys
  return (sort xs0,sort xs1,sort ys0,sort ys1)
  where
    readInt = state $ fromJust . B.readInt . B.dropWhile isSpace

f xs ys = go xs ys 0
  where
    go _ [] s = s
    go [] _ s = s
    go (x:xs) (y:ys) s
      | x >= y = go (x:xs) ys s
      | otherwise = go xs ys (s+1)

main = do
  !(xs0,xs1,ys0,ys1) <- evalState parse <$> B.getContents
  print $ f xs1 ys0 + f ys1 xs0
