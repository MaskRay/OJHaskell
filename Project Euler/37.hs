import Control.Monad
import Data.List

primeFactors = loop primes
             where loop ps@(p:ps') n
                        | p*p > n = [n]
                        | n `mod` p == 0 = p : loop ps (n `div` p)
                        | otherwise = loop ps' n
                   primes = 2 : filter ((== 1) . length . primeFactors) [3,5..]

isPrime n = case primeFactors n of
                 [1] -> False
                 [_] -> True
                 otherwise -> False

isTrunc k n
        | n < 10 = isPrime n
        | otherwise = isPrime n && isTrunc (k `div` 10) (n `mod` k)

f n
  | not $ isPrime n = []
  | otherwise = n : concat [f $ n * 10 + x | x <- [1..9]]

main = print $ sum $ filter (\x -> x >= 10 && isTrunc (10 ^ length (show x) `div` 10) x) $ concat [f x | x <- [1..9]]