import Data.Ord
import Data.List

pell n = loop 1 0 a0 1  0 1 a0
     where a0 = floor . sqrt . fromIntegral $ n
           loop p' q' p q g h a
                | p^2-n*q^2 == 1 = p
                | otherwise = loop p q p1 q1 g1 h1 a1
                where p1 = a1*p+p'
                      q1 = a1*q+q'
                      g1 = a*h-g
                      h1 = (n-g1^2) `div` h
                      a1 = (g1+a0) `div` h1

main = print $ fst $ maximumBy (comparing snd) $ [(n,pell n) | n <- [2..1000], n /= (floor . sqrt . fromIntegral $ n) ^ 2]
