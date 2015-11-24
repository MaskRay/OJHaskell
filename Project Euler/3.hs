main = print $ largest 600851475143 2 1
    where largest n p ans
            | p*p > n = max ans n
            | n `mod` p == 0 = largest (n `div` p) p p
            | otherwise = largest n (p + 1) ans