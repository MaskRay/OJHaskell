import Data.List

factors n = foldr (\x y ->
        if n `mod` x == 0
        then if x * x == n then x : y else x : n `div` x : y
        else y) [] [1..floor $ (+1e-5) $ sqrt $ fromIntegral n]

main = print . head . filter ((> 500) . length . factors) $ tri
     where tri = scanl1 (+) [1..]