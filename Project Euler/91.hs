n = 50
main = print $ 3*n^2 + 2 * sum [min ((n-y) `div` x') (x `div` y') | x <- [1..n], y <- [1..n], let x' = x `div` gcd x y; y' = y `div` gcd x y ]