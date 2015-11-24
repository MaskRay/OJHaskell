import Data.Array.Unboxed

n = 50000000

isPrime n = n > 1 && loop primes n
        where loop (ps@p:ps') n
                   | p*p > n = True
                   | n `mod` p == 0 = False
                   | otherwise = loop ps' n
primes = 2 : filter isPrime [3,5..]

takePrimes b f = takeWhile (<b) . map f $ primes

f = accumArray (||) False (1,n) [(a+b+c, True) | a <- takePrimes n (^2), b <- takeWhile (<n-a) $ takePrimes n (^3), c <- takeWhile (<n-a-b) $ takePrimes n (^4)] :: UArray Int Bool

main = print . length . filter id . elems $ f