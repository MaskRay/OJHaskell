{-# LANGUAGE GADTs, MultiWayIf #-}
import Control.Monad.Primitive
import Control.Monad.State.Strict
import Data.Bits
import Data.Char
import Data.Function
import Data.Functor
import Data.List
import Data.Maybe
import qualified Data.ByteString.Char8 as B
import qualified Data.Vector.Unboxed as VI
import qualified Data.Vector.Unboxed.Mutable as V

int = fst . fromJust . B.readInt
ints = map int . B.words

chunksOf i [] = []
chunksOf i xs = take i xs : chunksOf i (drop i xs)

-- | Segment tree

rmqBuild xs = VI.fromListN (2*nn-1) $ concat l
  where
    n = length xs
    nn = until (>=n) (*2) 1
    l = until (null . tail . head) (\(y:ys) -> map maximum (chunksOf 2 y):y:ys)
        [xs ++ replicate (nn-n) minBound]

rmqQuery tree ll rr = go 0 0 nn
  where
    nn = (VI.length tree+1) `quot` 2
    go i l r =
      if | rr <= l || r <= ll -> minBound
         | ll <= l && r <= rr -> tree VI.! i
         | otherwise -> go (2*i+1) l m `max` go (2*i+2) m r
      where
        m = (l+r) `quot` 2

-- | Fenwick tree

add fenwick i delta = go i
  where
    n = V.length fenwick
    go i | i >= n = return ()
         | otherwise = do
           v <- V.unsafeRead fenwick i
           V.unsafeWrite fenwick i $ v+delta
           go (i .|. (i+1))

prefixSum fenwick i = do
  let go i acc | i == 0 = return acc
               | otherwise = do
                 v <- V.unsafeRead fenwick (i-1)
                 go (i .&. (i-1)) $ acc + v
  go i 0

data Range m = Range (V.MVector (PrimState m) Int) (V.MVector (PrimState m) Int)

newRange :: Int -> IO (Range IO)
newRange n = liftM2 Range a a
  where
    a = V.replicate n 0

rangeAdd (Range fenwick1 fenwick2) l r delta = suffix l delta >> suffix r (-delta)
  where
    suffix i delta = do
      add fenwick1 i delta
      add fenwick2 i $ i * delta

rangeSum (Range fenwick1 fenwick2) l r = liftM2 (-) (prefix r) (prefix l)
  where
    prefix i = do
      x <- prefixSum fenwick1 i
      y <- prefixSum fenwick2 i
      return $ i*x-y

buildRects a = sort $ go 0 (length a) []
  where
    tree = rmqBuild $ zip a [0..]
    go l r acc
      | l >= r = acc
      | otherwise = go l i . go (i+1) r $ (l,i,r,x):(i+1,i,r,-x):acc
      where
        (x, i) = rmqQuery tree l r

readEvents m = go 0 []
  where
    go i acc
      | i == m = return acc
      | otherwise = do
        line <- B.getLine
        let [l, r] = map (subtract 1. int) (B.words line)
        go (i+1) $ (l,i,l,r+1,-1):(r+1,i,l,r+1,1):acc

solve :: [(Int,Int,Int,Int)] -> [(Int,Int,Int,Int,Int)] -> Range IO -> Range IO -> V.MVector (PrimState IO) Int -> IO ()
solve _ [] _ _ _ = return ()
solve ((y,yl,yr,yv):rects) es@((x,_,_,_,_):_) d1 d2 ans | y < x = do
  rangeAdd d1 yl yr yv
  rangeAdd d2 yl yr (y * yv)
  solve rects es d1 d2 ans
solve rects ((x,i,xl,xr,xv):es) d1 d2 ans = do
  d1s <- rangeSum d1 xl xr
  d2s <- rangeSum d2 xl xr
  let sum = x * d1s - d2s
  V.modify ans (+ (xv * sum)) i
  solve rects es d1 d2 ans

main = do
  [n, m] <- ints <$> B.getLine
  rects <- (buildRects . ints) <$> B.getLine
  es <- sort <$> readEvents m
  d1 <- newRange n
  d2 <- newRange n
  ans <- V.replicate m 0
  solve rects es d1 d2 ans
  forM_ [0..m-1] $ (>>= print) . V.unsafeRead ans
