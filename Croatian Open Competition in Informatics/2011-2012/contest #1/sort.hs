{-# LANGUAGE BangPatterns #-}
import Control.Applicative
import Control.Monad
import Control.Monad.State
import Data.Char
import Data.Int
import Data.List
import Data.Maybe
import Data.Monoid
import qualified Data.ByteString.Char8 as B

data T = T [Int] !Int deriving Show

instance Monoid T where
  mempty = T [] 0
  T xs sl `mappend` T ys sr = T (xs ++ ys) (sl+sr)

parse = readInt >>= \n -> replicateM n readInt
  where
    readInt = state $ fromJust . B.readInt . B.dropWhile isSpace

firstPass [] = mempty
firstPass (x:xs) = T ys (if length ys > 1 then 1 else 0) <> firstPass zs
  where
    (ys,zs) = go [] x xs
    go zs x (y:ys) | x > y = go (x:zs) y ys
    go zs x ys = (x:zs, ys)

advance (T xs s) ss = T xs (s+ss)

merge [] = mempty
merge [x] = T [x] 0
merge xs = go ys' zs' 0 0 [] `advance` (l+r)
  where
    n = length xs
    (ys,zs) = splitAt (n `div` 2) xs
    T ys' l = merge ys
    T zs' r = merge zs
    go ys [] !r !s rs = T (reverse rs ++ ys) (s + length ys * r)
    go [] zs _ !s rs = T (reverse rs ++ zs) s
    go (y:ys) (z:zs) !r !s rs
      | y > z = go (y:ys) zs (r+1) s (z:rs)
      | otherwise = go ys (z:zs) r (s+r) (y:rs)

main = do
  xs <- evalState parse <$> B.getContents
  let T ys s = firstPass xs
      T _ ss = merge ys
  print $ s+ss
