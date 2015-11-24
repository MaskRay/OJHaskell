import Data.List

primeFactors n = loop primes n []
        where loop ps@(p:ps') n s
                   | p*p > n = n:s
                   | n `mod` p == 0 = loop ps (n `div` p) (p:s)
                   | otherwise = loop ps' n s
primes = 2 : filter ((==1) . length . group . primeFactors) [3,5..]

main = print $ head [a | a <- [2..], let p = map (\x -> group $ primeFactors (x+a)) [0..3], all ((==4) . length) p, length (concat p) == 16]