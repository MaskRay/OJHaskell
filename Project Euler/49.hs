import Data.Set
import Data.List

primeFactors = loop primes
             where loop ps@(p:ps') n
                        | p*p > n = [n]
                        | n `mod` p == 0 = p : loop ps (n `div` p)
                        | otherwise = loop ps' n
                   primes = 2 : Prelude.filter ((== 1) . length . primeFactors) [3,5..]

isPrime n = case primeFactors n of
                 [1] -> False
                 [_] -> True
                 otherwise -> False

primes4 = Prelude.filter isPrime [1000..9999]
primes4set = fromAscList primes4

main = putStrLn . last $ [show a++show b++show c |
     a <- primes4, b <- dropWhile (<= a) primes4,
     sort (show a) == sort (show b),
     let c = 2*b-a, c `member` primes4set,
     sort (show b) == sort (show c)
     ]