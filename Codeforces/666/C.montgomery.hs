{- l == length of the string
- dp[l][i] = dp[l][i]*26 + 25**(i-l)*binom(i-1,l-1)
-}
{-# LANGUAGE BangPatterns, CPP, FlexibleContexts, MultiWayIf, NoMonomorphismRestriction, OverloadedStrings, RankNTypes, Safe #-}
import Control.Monad
#if __GLASGOW_HASKELL__ >= 710
import Control.Monad.ST
#else
import Control.Monad.ST.Safe
import Data.Functor
#endif
import Data.Array.IO.Safe
import Data.Array.ST.Safe
import Data.Array.Unboxed
import Data.Bits
import Data.Int
import Data.Maybe
import qualified Data.ByteString.Char8 as B
import Text.Printf

maxN = 100000
modulus = 1000000007 :: Int64
antiModulus = 2226617417 :: Int64
r3 = 873523211 :: Int64
threshold = 500

{-# INLINE inverse #-}
inverse :: Int64 -> Int64
inverse a = go a 1
  where
    go a s | a == 1 = s
           | otherwise = go (modulus `rem` a) $ (modulus - modulus `quot` a) * s `rem` modulus

factorial :: (UArray Int64 Int64, UArray Int64 Int64)
factorial = runST $ do
  a <- newArray_ (0, maxN) :: ST s (STUArray s Int64 Int64)
  b <- newArray_ (0, maxN) :: ST s (STUArray s Int64 Int64)
  writeArray a 0 $ montgomery 1
  writeArray b 0 $ montgomery 1
  let f i | i > maxN = return ()
          | otherwise = do
              x <- readArray a (i-1)
              let y = (x*i) `mod` modulus
              writeArray a i y
              writeArray b i . mult r3 $ inverse y
              f (i+1)
  f 1
  liftM2 (,) (freeze a) (freeze b)

{-# INLINE binom #-}
binom :: Int64 -> Int64 -> Int64
binom n m = mult (mult (fact!n) (inv!m)) (inv!(n-m))
  where
    (fact, inv) = factorial

powMod :: Int64 -> Int -> Int64
powMod a n = go a n 1
  where
    go _ 0 s = s
    go a n s = go (a*a `rem` modulus) (n `div` 2) (if even n then s else s*a `rem` modulus)

compute :: IOUArray Int64 Int64 -> B.ByteString -> IO ()
compute a s = f 0 (montgomery 1)
  where
    l = fromIntegral $ B.length s
    f i pw | i < l = writeArray a i 0 >> f (i+1) pw
           | i <= maxN = do
              t <- readArray a (i-1)
              writeArray a i . norm $ mult t m26 + mult (binom (i-1) (l-1)) pw
              f (i+1) (mult pw m25)
           | otherwise = return ()

montgomery :: Int64 -> Int64
montgomery a = a `shiftL` 32 `mod` modulus

m25 = montgomery 25
m26 = montgomery 26

unMontgomery :: Int64 -> Int64
unMontgomery a = a * 518424770 `mod` modulus

{-# INLINE norm #-}
norm :: Int64 -> Int64
norm a = if a >= modulus then a - modulus else a

redc :: Int64 -> Int64
redc t = if x >= modulus then x - modulus else x
  where
    x = (t + (t.&.(2^32-1) * antiModulus .&. (2^32-1) * modulus)) `shiftR` 32

{-# INLINE mult #-}
mult :: Int64 -> Int64 -> Int64
mult a b = redc (a*b)

main = do
  m <- readLn :: IO Int
  a <- newArray_ (0, maxN) :: IO (IOUArray Int64 Int64)
  [s] <- B.words <$> B.getLine
  when (B.length s > threshold) $ compute a s
  let
    go 0 _ = return ()
    go m s = do
      [op, b] <- B.words <$> B.getLine
      if op == "1"
         then do
           when (B.length b > threshold) $ compute a b
           go (m-1) b
         else do
           let n = fromIntegral . fst . fromJust $ B.readInt b
               f i pw acc | i < 0 = acc
                          | otherwise =
                            let !acc' = acc - mult (binom n i) pw in
                            f (i-1) (mult pw m25) $ if acc' < 0 then acc'+modulus else acc'
           if | l > threshold -> readArray a n >>= print . unMontgomery
              | n < l -> print 0
              | otherwise -> print . unMontgomery $ f (l-1) (montgomery $ powMod 25 (n-l+1)) (montgomery $ powMod 26 n)
           go (m-1) s
      where
        l = fromIntegral $ B.length s
  go m s
