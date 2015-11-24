isPrime n = n > 1 && loop primes n
        where loop ps@(p:ps') n
                   | p*p > n = True
                   | n `mod` p == 0 = False
                   | otherwise = loop ps' n
primes = 2 : filter isPrime [3,5..]

prob58 len nume deno
       | nume*10 < deno = len
       | otherwise = prob58 len'
                   (nume+sum [if isPrime n then 1 else 0 | n <- take 4 [len'*len',len'*len'-len'+1..]]) (deno+4)
       where len' = len+2

main = print $ prob58 3 3 5