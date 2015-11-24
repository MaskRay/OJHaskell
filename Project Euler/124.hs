import Data.Monoid
import Math.Sieve.Factor
import Data.List

radical n = product $ map fst $ factor sie n
  where sie = sieve 100000

main = print $ snd $ (!!9999) $ sort [(radical n, n) | n <- [1..100000]]