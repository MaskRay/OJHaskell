binomial n m = product [n-m+1..n] `div` product [1..m]

f n x = sum [binomial (i+j) i | i <- [1..n `div` x], let j = n-i*x]

main = print $ sum $ fmap (f 50) [2..4]