import Data.List
import Data.Ord

main = print . fst . maximumBy (comparing snd) $ [(n, f n) | n <- [2..999]]
     where f n = g 1 n []
           g r 0 xs = 0
           g r n xs = let r' = r `mod` n
                      in case elemIndex r' xs of
                         Just i  -> i+1
                         Nothing -> g (r' * 10) n (r':xs)
