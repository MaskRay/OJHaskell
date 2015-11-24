import Data.List

main = print $ sum [x+y | x <- [2..9999], let y = d x, x < y && y < 10000, x == d y]

d n = product [(p * product g - 1) `div` (p - 1) |
              g <- group $ factors n, let p = head g] - n

factors = loop primes
        where loop ps@(p:ps') n
                   | p * p > n = [n]
                   | r == 0 = p : loop ps (n `div` p)
                   | otherwise = loop ps' n
                   where (q, r) = n `divMod` p
              primes = 2 : filter (\x -> length (factors x) == 1) [3,5..]