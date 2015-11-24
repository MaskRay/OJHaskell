import Data.List

primeFactors n = loop primes n []
        where loop ps@(p:ps') n s
                   | p*p > n = n:s
                   | n `mod` p == 0 = loop ps (n `div` p) (p:s)
                   | otherwise = loop ps' n s

primes = 2 : filter ((==1) . length . primeFactors) [3,5..]

count = (`div` 2) . succ . product . fmap (\s -> 2*length s+1). group . primeFactors

main = print . head . filter (\x -> count x > 1000) $ [2..]
