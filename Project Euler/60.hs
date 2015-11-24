import Control.Monad

isPrime n = n > 1 && loop primes n
        where loop (ps@p:ps') n
                   | p*p > n = True
                   | n `mod` p == 0 = False
                   | otherwise = loop ps' n
primes = 2 : filter isPrime [3,5..]
primes10000 = takeWhile (<10000) primes

prob60 = do
     a <- primes10000
     b <- dropWhile (<=a) primes10000
     guard $ f a b
     c <- dropWhile (<=b) primes10000
     guard $ f a c && f b c
     d <- dropWhile (<=c) primes10000
     guard $ f a d && f b d && f c d
     e <- dropWhile (<=d) primes10000
     guard $ f a e && f b e && f c e && f d e
     return $ a+b+c+d+e
     where f x y = isPrime (read $ shows x $ show y) && isPrime (read $ shows y $ show x)

main = print $ head $ prob60