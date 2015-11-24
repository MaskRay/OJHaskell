import Data.Array
import Data.List

n = 28124

abundant = listArray (1,n) $ map (\x -> sumOfProperFactors x > x) [1..n]

sumOfProperFactors 1 = 0
sumOfProperFactors n = product [(p * product g - 1) `div` (p - 1) | g <- group $ factors n, let p = head g] - n

factors = loop primes
        where loop ps@(p:ps') n
                   | p*p > n = [n]
                   | n `mod` p == 0 = p : loop ps (n `div` p)
                   | otherwise = loop ps' n
              primes = 2 : filter (\x -> length (factors x) == 1) [3,5..]

isSum n = any (abundant !) $ map (n -) $ takeWhile (<= n `div` 2) $ filter (abundant !) [1..n]

main = print . sum . filter (not . isSum) $ [1..n]
