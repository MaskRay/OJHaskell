import Data.Array

isPrime n = n > 1 && loop primes n
        where loop (ps@p:ps') n
                   | p*p > n = True
                   | n `mod` p == 0 = False
                   | otherwise = loop ps' n
primes = 2 : filter isPrime [3,5..]
primesA = listArray (0,99) primes

a = listArray ((0,0),(99,99)) [f i j | i <- [0..99], j <- [0..99]]
  where f 0 _ = 1
        f i 0 = if i `mod` 2 == 0 then 1 else 0
        f i j = a!(i,j-1) + (if i-primesA!j < 0 then 0 else a!(i-primesA!j,j))

main = print $ head $ filter (\x -> a!(x,x) >= 5000) [0..99]