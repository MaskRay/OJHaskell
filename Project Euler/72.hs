import Math.Sieve.Phi

main = print $ sum $ fmap (phi sie) $ [2..1000000]
     where sie = sieve 1000000