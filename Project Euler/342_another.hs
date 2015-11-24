{-# LANGUAGE BangPatterns #-}
import Control.Arrow
import Data.Int
import Data.List

maxx = 10^5
maxn = 10^10

isPrime n = and . map ((/=0) . (n `mod`)) $ takeWhile (\p -> p*p <= n) primes
primes = 2 : filter isPrime [3,5..] :: [Int]

getExp :: Int -> Integer -> (Int, Integer)
getExp !p !x
    | r /= 0 = (0, x)
    | otherwise = first succ $ getExp p x'
  where
    !(x', r) = x `quotRem` fromIntegral p

solve :: [Int] -> Integer -> Integer -> Integer
solve ![] !n _ = fromIntegral n
solve !(p:ps) !n !q = (if e' `rem` 3 == 0 then (+solve ps n q') else id) . sum
    $ map (\(e,n') -> solve ps n' $ q'*(fromIntegral p-1))
    $ zip [ii,ii+3..] $ takeWhile (<fromIntegral maxn) ns
  where
    !i = (2-2*e') `mod` 3
    !ii = if i == 0 then 3 else i
    !(e', q') = getExp p q
    !ns = iterate (* fromIntegral p ^ 3) (n * fromIntegral p ^ ii)

p342 = solve (reverse $ takeWhile (<maxx) primes) 1 1 - 1

main = print p342
