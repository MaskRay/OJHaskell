import Math.Sieve.Factor

main = print $ length $ filter (isPrime sie . f) $ takeWhile ((<=1000000) . f) [1..]
       where sie = sieve 1000000
             f x = 3*x*x+3*x+1