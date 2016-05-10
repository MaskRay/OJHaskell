{- l == length of the string
- dp[l][i] = dp[l][i]*26 + 25**(i-l)*binom(i-1,l-1)
-}
{-# LANGUAGE CPP, FlexibleContexts, MultiWayIf, NoMonomorphismRestriction, OverloadedStrings, RankNTypes, Safe #-}
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
import Data.Int
import Data.Maybe
import qualified Data.ByteString.Char8 as B
import Text.Printf

maxN = 100000
modulus = 1000000007 :: Int64
threshold = 500

inverse :: Int64 -> Int64
inverse 1 = 1
inverse n = (modulus - modulus `quot` n) * inverse (modulus `rem` n) `rem` modulus

factorial :: (UArray Int64 Int64, UArray Int64 Int64)
factorial = runST $ do
  a <- newArray_ (0, maxN) :: ST s (STUArray s Int64 Int64)
  b <- newArray_ (0, maxN) :: ST s (STUArray s Int64 Int64)
  writeArray a 0 1
  writeArray b 0 1
  let f i | i > maxN = return ()
          | otherwise = do
              x <- readArray a (i-1)
              let y = x*i `rem` modulus
              writeArray a i y
              writeArray b i $ inverse y
              f (i+1)
  f 1
  liftM2 (,) (freeze a) (freeze b)

binom :: Int64 -> Int64 -> Int64
binom n m = fact!n * inv!m `rem` modulus * inv!(n-m) `rem` modulus
  where
    (fact, inv) = factorial

powMod :: Int64 -> Int -> Int64
powMod a n = go a n 1
  where
    go _ 0 s = s
    go a n s = go (a*a `rem` modulus) (n `div` 2) (if even n then s else s*a `rem` modulus)

compute :: IOUArray Int64 Int64 -> B.ByteString -> IO ()
compute a s = f 0 1
  where
    l = fromIntegral $ B.length s
    f i pw | i < l = writeArray a i 0 >> f (i+1) pw
           | i <= maxN = do
              t <- readArray a (i-1)
              writeArray a i $ (t * 26 + binom (i-1) (l-1) * pw) `rem` modulus
              f (i+1) (pw*25 `rem` modulus)
           | otherwise = return ()

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
                          | otherwise = f (i-1) (pw*25 `rem` modulus) $ (acc - binom n i * pw) `rem` modulus
           if | l > threshold -> readArray a n >>= print
              | n < l -> print 0
              | otherwise -> print $ f (l-1) (powMod 25 (n-l+1)) (powMod 26 n) `mod` modulus
           go (m-1) s
      where
        l = fromIntegral $ B.length s
  go m s
