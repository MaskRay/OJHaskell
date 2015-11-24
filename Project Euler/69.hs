import Math.Sieve.Phi
import Data.Ord
import Data.Ratio
import Data.List

main = print . fst . maximumBy (comparing snd) $ [(n, n % phi sie n) | n <- [2..1000000]]
     where sie = sieve 1000000