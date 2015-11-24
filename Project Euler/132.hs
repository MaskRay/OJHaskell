import Math.Sieve.ONeill

powMod a n p
  | n == 0 = 1
  | n `mod` 2 == 0 = powMod (a*a `mod` p) (n `div` 2) p
  | otherwise = powMod a (n-1) p * a `mod` p

f p = (==1) $ powMod 10 (10^9) p

main = print $ (3 `subtract`) $ sum $ take 41 [p | p <- primes, f p]