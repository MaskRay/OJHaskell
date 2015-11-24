powMod a 0 m = 1
powMod a n m
       | n `mod` 2 == 0 = ret * ret `mod` m
       where ret = powMod a (n `div` 2) m
powMod a n m = powMod a (n-1) m * a `mod` m

main = print $ (`mod` (10^10)) $ 28433 * powMod 2 7830457 (10^10) + 1