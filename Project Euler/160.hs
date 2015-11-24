powMod a n p
       | n == 0 = 1
       | even n = powMod (a*a `mod` p) (n `div` 2) p
       | otherwise = powMod a (n-1) p * a `mod` p

gcd' a 0 = (1, 0)
gcd' a b = let (x, y) = gcd' b (a `mod` b)
           in  (y, x-a `div` b*y)

zero n = sum . takeWhile (>0) . map (div n) . iterate (*5) $ 5

solve5_5 0 _ = 1
solve5_5 n k = powMod (f (5^k-1)) (n `div` 5^k) (5^k) * f (n `mod` 5^k) * res'
         where res' = solve5_5 (n `div` 5) k
               f 0 = 1
               f n | n `mod` 5 == 0 = f (n-1)
                   | otherwise = f (n-1) * n `mod` 5^k

solve10_5 n k = let phi = 4*5^(k-1)
                    a = solve5_5 n k
                in  powMod 2 (phi-zero n `mod` phi) (5^k) * a `mod` (5^k)

solve10_10 n k = let (x, _) = gcd' (2^k) (5^k)
                     a = solve10_5 n k
                 in  x*2^k*a `mod` (10^k)

main = print $ solve10_10 (10^12) 5