import Data.Set

exgcd a 0 = (a, 1, 0)
exgcd a b = let (d, y, x) = exgcd b $ a `mod` b
            in (d, x, y-a `div` b*x)
inverse a n = let (_, x, _) = exgcd a n
              in x
powMod a n m
       | n == 0 = 1
       | n `mod` 2 == 0 = let ret = powMod a (n `div` 2) m in ret * ret `mod` m
       | otherwise = let ret = powMod a (n `div` 2) m in ret * ret * a `mod` m

prob48 n m = sum [f i upper n m | i <- [1..min (m-1) n],
       let maxMultiple = n `div` m * m
           upper = if maxMultiple+i <= n then maxMultiple+i else maxMultiple+i-m
       ] `mod` m
f lo hi n m = -- lo^lo+lo^(lo+m)+...+lo^hi
               let lolo = powMod lo lo m
                   items = (hi-lo) `div` lo + 1
               in  if lo == 1
                   then items
                   else (powMod lolo items m - 1) * inverse (lolo - 1) m * lolo `mod` m

main = let (_, x, y) = exgcd (2^10) (5^10)
       in print $ (x * prob48 n (2^10) - y * prob48 n (5^10)) `mod` (10^10)
       where n = 1000