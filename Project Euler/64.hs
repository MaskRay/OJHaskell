import Data.List

continuedFraction n = loop 0 1 a0
     where a0 = floor . sqrt . fromIntegral $ n
           loop g h a
                | g1 == a0 && h1 == 1 = 1
                | otherwise = 1 + loop g1 h1 a1
                where g1 = a*h-g
                      h1 = (n-g1^2) `div` h
                      a1 = (g1+a0) `div` h1

main = print . sum $ [continuedFraction n `mod` 2 | n <- [1..10000] \\ fmap (^2) [1..100]]