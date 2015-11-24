import Control.Monad
import Data.List

primeFactors = loop primes
             where loop ps@(p:ps') n
                        | p*p > n = [n]
                        | n `mod` p == 0 = p : loop ps (n `div` p)
                        | otherwise = loop ps' n
                   primes = 2 : filter ((== 1) . length . primeFactors) [3,5..]

isPrime n = case primeFactors n of
                 [1] -> False
                 [_] -> True
                 otherwise -> False

rots l = map (\i -> let (x,y) = splitAt i l in y ++ x) [0..length l - 1]

isCircular = all (isPrime . l2n) . rots
           where l2n = foldl1 (\x y -> x * 10 + y)

circular 1 = [[2], [3], [5], [7]]
circular n = filter isCircular . replicateM n $ [1,3,7,9]

main = print . length . concatMap circular $ [1..6]