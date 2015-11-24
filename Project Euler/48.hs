exgcd a 0 = (a, 1, 0)
exgcd a b = let (d, y, x) = exgcd b $ a `mod` b
            in (d, x, y-a `div` b*x)
powMod a n m
       | n == 0 = 1
       | n `mod` 2 == 0 = let ret = powMod a (n `div` 2) m in ret * ret `mod` m
       | otherwise = let ret = powMod a (n `div` 2) m in ret * ret * a `mod` m

powSum a 0 m = (0, 1)
powSum a n m
       | n `mod` 2 == 0 = (sum * (1 + a) `mod` m, last * last `mod` m)
       where (sum, last) = powSum a (n `div` 2) m
powSum a n m
       | otherwise = ((sum + last) `mod` m, last * a `mod` m)
       where (sum, last) = powSum a (n - 1) m

prob48 n m = sum [f i n | i <- [1..min (m-1) n]] `mod` m
       where f lo hi = -- lo^lo+lo^(lo+m)+...+lo^hi
               let lolo = powMod lo lo m
                   items = (hi-lo) `div` m + 1
               in  fst (powSum lo items m) * lolo `mod` m

main = let (_, x, y) = exgcd (2^10) (5^10)
       in print $ (y * (5^10) * prob48 n (2^10) + x * (2^10) * prob48 n (5^10)) `mod` (10^10)
       where n = 1000