import Data.Ratio

main = print $ length $ filter (\x -> len (numerator x) > len (denominator x)) $ take 1000 $ iterate (\x -> 1+1/(1+x)) (3%2)
     where len = length . show