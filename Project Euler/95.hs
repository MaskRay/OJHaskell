import Data.List

primeFactors 1 = []
primeFactors n = loop primes n
        where loop ps@(p:ps') n
                   | p*p > n = [n]
                   | n `mod` p == 0 = p : loop ps (n `div` p)
                   | otherwise = loop ps' n
primes = 2 : filter ((==1) . length . primeFactors) [3,5..]

sumOfProperFactors n = (subtract n) $ foldl (\x (ps@(p:_)) -> (p^(length ps+1)-1) `div` (p-1) * x) 1 $ group $ primeFactors n

chain n n' s
      | n' > 1000000 = []
      | n' `elem` s = s
      | otherwise = chain n (sumOfProperFactors n') (n':s)

main = print $ negate $ snd $ maximum $ [(l, (negate n)) | n <- [12496..15000], let l = chain n n [], n `elem` l]