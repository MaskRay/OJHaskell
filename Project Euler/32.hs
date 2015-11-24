import Control.Monad
import Data.List

perms 0 xs = [([], xs)]
perms n xs = [(x:ys, rest) | x <- xs, (ys, rest) <- perms (n-1) (delete x xs)]

l2n = foldl1 (\x y -> 10 * x + y)

pandigiticals =
              nub $ do (bgn, end) <- perms 5 [1..9]
                       n <- [1,2]
                       let (a, b) = splitAt n bgn
                           res = l2n a * l2n b
                       guard $ sort (unfoldr (\x -> if x == 0 then Nothing else Just . swap $ divMod x 10) res) == end
                       return res
              where swap (x, y) = (y, x)

main = print $ sum pandigiticals