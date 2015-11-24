{-# LANGUAGE BangPatterns, NoMonomorphismRestriction #-}
import Control.Applicative
import Control.Monad
import Data.Array
import Data.Int

bsearch :: [(Int,Int64)] -> [(Int,Int)] -> Int64 -> Int64
bsearch !wvs !is !std = go 0 (maximum (map fst wvs)+1)
  where
    !n = length wvs
    go !lo !hi
        | lo == hi-1 = min (abs $ inspect lo-std) (abs $ inspect hi-std)
        | otherwise = if inspect mid >= std then go mid hi else go lo mid
      where
        !mid = (lo+hi) `div` 2
    inspect !para = sum $ map (\(l,r) -> fromIntegral (cnta!r-cnta!(l-1)) * (suma!r-suma!(l-1))) is
      where
        !cnta = listArray (0,n) $ scanl (\acc (w,_) -> acc + fromEnum (w >= para)) 0 wvs :: Array Int Int
        !suma = listArray (0,n) $ scanl (\acc (w,v) -> acc + (if w >= para then v else 0)) 0 wvs :: Array Int Int64

main = do
    let s2tuple = (\[a,b] -> (read a,read b)) . words
    [n,m,std] <- (map read . words) <$> getLine
    wvs <- replicateM (fromIntegral n) $ s2tuple <$> getLine
    is <- replicateM (fromIntegral m) $ s2tuple <$> getLine
    print $ bsearch wvs is std
