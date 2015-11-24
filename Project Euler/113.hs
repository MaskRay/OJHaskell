import Data.Array

digits = 100

dp = listArray ((1,0), (digits,9)) $ map f $ range ((1,0), (digits,9))

f (1, _) = 1
f (i, j) = sum [dp ! (i-1, j') | j' <- [0..j]]

main = print $ sum [dp!i | i <- range ((digits,0), (digits,9))]
     + sum [dp!i | i <- range ((1,1), (digits,9))]
     - digits*9 - 1