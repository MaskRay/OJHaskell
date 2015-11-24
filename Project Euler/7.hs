main = print $ (0:primes) !! 10001
     where primes = 2 : filter isPrime [3,5..]

isPrime n = loop n 2
        where loop n i
                   | i*i > n = True
                   | n `mod` i == 0 = False
                   | otherwise = loop n (i+1)
