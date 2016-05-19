import Data.Array

n = 60

a = listArray ((0,0,1,1,1),(n,n,6,6,6))
  [ g i j t f r | i <- [0..n], j <- [0..n],
    t <- [1..6], f <- [1..6], r <- [1..6] ]

g 0 _ _ _ _ = 0
g _ 0 _ _ _ = 0
g 1 1 1 2 4 = 1
g i j t f r = max (if x > 0 then x+t else 0) (if y > 0 then y+t else 0)
  where
    x = a ! (i,j-1,r,f,7-t)
    y = a ! (i-1,j,f,7-t,r)

solve :: [Int] -> Int
solve [m, n] = maximum [a ! (m,n,i,j,k) | i <- [1..6], j <- [1..6], k <- [1..6]]

main = interact $ unlines . map (show . solve . map read . words) . tail . lines
