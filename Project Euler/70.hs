import Math.Sieve.Phi
import Data.List
import Data.Ratio
import Data.Ord

main = do
     let sie = sieve $ 10^7
     print . fst . minimumBy (comparing $ snd) $ [(n, n % phi sie n) | n <- [2..10^7-1], sort (show n) == sort (show $ phi sie n)]
