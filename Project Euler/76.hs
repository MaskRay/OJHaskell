import Data.Array

n = 100

a = listArray ((0,0),(n,n)) [f i j | i <- [0..n], j <- [0..n]]
  where f 0 _ = 1
        f _ 0 = 0
        f i j
          | i >= j = a!(i,j-1) + a!(i-j,min (i-j) j)
          | otherwise = 0

main = print $ a!(n,n-1)