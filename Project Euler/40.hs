import Data.Char

main = print $ d 1*d 10*d 100*d 1000*d 10000*d 100000*d 1000000
     where d n = digitToInt $ (concat $ map show [1..]) !! (n-1)