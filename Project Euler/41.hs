import Data.List

isPrime n = n > 1 && loop primes n
        where loop (ps@p:ps') n
                   | p*p > n = True
                   | n `mod` p == 0 = False
                   | otherwise = loop ps' n
primes = 2 : filter isPrime [3,5..]

perm [] = [0]
perm l = concat [(+a*10^(length l-1)) `fmap` perm (delete a l)| a <- l]

main = print $ head $ filter isPrime $ concat [perm [n,n-1..1] | n <- [9,8..1]]