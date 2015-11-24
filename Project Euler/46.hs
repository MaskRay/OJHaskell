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

goldbach n = (not $ isPrime n) && not (any isPrime $ takeWhile (> 0) $ map (\x -> n - 2*x*x) [1..n])

prob46 = head . filter goldbach $ [9,11..]

main = print $ prob46