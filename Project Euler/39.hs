import Data.Array
import Data.List
import Data.Ord

f x = [a+b+c | m <- [2..ceiling . sqrt . fromIntegral $ x], n <- [1..m-1],
      let a = m^2-n^2; b = 2*m*n; c = m^2+n^2, a+b+c <= x, even a || even b, gcd a b == 1]

g n = [x*p | x <- f n, p <- [1..n `div` x]]


main = print . head . maximumBy (comparing length) . group . sort $ g 1000
--main = print . g$ 1000


