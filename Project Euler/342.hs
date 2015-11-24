{-# LANGUAGE BangPatterns #-}

import Data.Array.Base
import Data.Array.ST
import Data.Array.Unboxed
import Data.List
import Control.Monad
import Control.Monad.ST

maxx = 10^6 :: Int

buildFactor :: Int -> UArray Int Int
buildFactor n = runSTUArray $ do
    a <- newListArray (2, n) [2..n] :: ST s (STUArray s Int Int)
    forM_ (takeWhile (\x -> x*x <= n) $ 2:[3,5..]) $ \i -> do
        ai <- readArray a i
        when (ai == i) $ forM_ [i*i,i*i+i..n] $ \j -> writeArray a j i
    return a
factors = buildFactor maxx

factorize :: Int -> [Int]
factorize !n
    | fn == n = [fn]
    | otherwise = fn : factorize (n `div` fn)
  where
    fn = factors ! n

-- numbers of n s.t. phi(n^2) = x^3
solve :: Int -> [Integer]
solve x = [ n2
          | f' <- tail $ subsequences f
          , let n2 = x3 `div` fromIntegral (product (map pred f')) * fromIntegral (product f')
          , checkSqr n2 $ map ((^2) . fromIntegral) f'
          ]
  where
    x3 = toInteger x ^ 3
    f = nub $ sort $ factorize x
    checkSqr n [] = n == 1
    checkSqr n (x:xs)
        | r == 0 = checkSqr n' (x:xs)
        | otherwise = checkSqr n xs
      where
        (n', r) = n `quotRem` x

-- n must be x^2
isqrt :: Integer -> Integer
isqrt n = fst . head . dropWhile (\(a,b) -> a /= b) $ zip xs (tail xs)
  where
    xs = iterate (\x -> (x + n `div` x) `div` 2) n

p342 = sum [ isqrt n2
           | i <- [2..maxx]
           , n2 <- solve i
           , inRange (2^2, (10^10-1)^2) n2
           ]

main = print p342
