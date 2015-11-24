import Math.Sieve.ONeill

main = print $ fst $ head $ filter (\(n,p) -> (if n `mod` 2 == 0 then 2 else 2*n `mod` p) * p >= 10^10) $ zip [1..] primes