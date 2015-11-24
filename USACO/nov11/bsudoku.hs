{-# LANGUAGE BangPatterns #-}
import Control.Applicative
import Control.Monad
import Control.Monad.State
import Data.Array
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

solve :: Array (Int,Int) Bool -> Int
solve g = a!(0,0,0,0,False)
  where
    a = listArray ((0,0,0,0,False),(8,8,255,255,True))
        [ go r c mc mb mr
        | r <- [0..8], c <- [0..8]
        , mc <- [0..255], mb <- [0..255], mr <- [False,True]
        ] :: Array (Int,Int,Int,Int,Bool) Int
    go 9 _ mc _ _
      | mc == 0 = 0
      | otherwise = 999
    go r 9 mc mb mr
      | mr || r `rem` 3 == 2 && mb > 0 = 999
      | otherwise = a!(r+1,0,mc,mb,False)
    go r c mc mb mr = min ((a!(r,c+1,mc,mb,mr))+f) ((a!(r,c+1,mc.|.1`shiftL`c,mb.|.1`shiftL`(c`div`3),not mr))+1-f)
      where
        f = fromEnum $ g!(r,c)
  
main = do
  as <- B.lines <$> B.getContents
  let g = listArray ((0,0),(8,8)) [ B.index (as!!i) j == '1' | i <- [0..8], j <- [0..8] ] :: Array (Int,Int) Bool
  print as
  print $ solve g