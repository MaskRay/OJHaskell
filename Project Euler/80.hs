import Data.Array

partitions = listArray (0,60000) $ 1 : 
           [sum [s*partitions ! p | (s, p) <- zip (cycle [1,1,(-1),(-1)]) $ parts n] |
           n <- [1..]]
           where pentagonal n = n*(3*n-1) `div` 2
                 parts n = takeWhile (>=0) [n-pentagonal x | x <- concat [[n,(-n)] | n <- [1..]]]

main = print $ head $ filter (\x -> (partitions!x) `mod` 1000000 == 0) [1..]