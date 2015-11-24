import Data.List

l = 0:0:1:1:zipWith4 (\a b c d -> a+b+c+d) l l1 l2 l3
  where l1 = tail l
        l2 = tail l1
        l3 = tail l2

main = print $ (l!!) $ (+2) 50