import Data.Array

m = 1000000
f = concat [takeWhile ((<m).fst) [(n*(4*k-n), 1)| k <- [1+(n`div`4)..n-1]] | n <- [1..m]]
main = print $ length $ filter ((==10).snd) $ assocs $ accumArray (+) 0 (1,m) f
