{-# LANGUAGE BangPatterns #-}
import Control.Applicative
import Control.Monad
import Control.Monad.State
import Data.Array
import Data.Char
import Data.Maybe
import Data.List
import qualified Data.ByteString.Char8 as B

parse = do
  n <- readInt
  !a <- listArray ((0,0),(n-1,n-1)) <$> replicateM (n*n) readInt
  return (n,a)
  where
    readInt = state $ fromJust . B.readInt . B.dropWhile isSpace

onCell n a = maximum [go x y 1 0 0 | (x,y) <- range $ bounds a]
  where
    go !x !y !i !s !opt | x-i < 0 || x+i >= n || y-i < 0 || y+i >= n = opt
                      | otherwise = go x y (i+1) s' (max opt s')
		   where
		     s' = s + a!(x-i,y-i) + a!(x+i,y+i) - a!(x-i,y+i) - a!(x+i,y-i)

onCorner n a = maximum [go x y 0 0 0 | x <- [0..n-2], y <- [0..n-2]]
  where
    go !x !y !i !s !opt | x-i' < 0 || x+i'+1 >= n || y-i' < 0 || y+i'+1 >= n = opt'
                      | otherwise = go x y i' s' opt'
		   where
		     i' = i+1
		     s' = s + a!(x-i,y-i) + a!(x+i+1,y+i+1) - a!(x-i,y+i+1) - a!(x+i+1,y-i)
		     opt' = max opt s'

      
main = do
  (n,a) <- evalState parse <$> B.getContents
  print $ max (onCell n a) (onCorner n a)
