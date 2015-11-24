modulus = 10007

powMod a n
    | n == 0 = 1
    | even n = powMod (a*a `mod` modulus) (n `div` 2)
    | otherwise = powMod a (n-1) * a `mod` modulus

c n m = foldl (((`mod` modulus).) . (*)) 1 $ [1..n] ++ map inverse [1..m] ++ map inverse [1..n-m]
  where
    inverse = flip powMod (modulus-2)

main = do
    [a,b,k,n,m] <- (map read . words) `fmap` getLine :: IO [Int]
    print $ powMod a n * powMod b m `mod` modulus * c k n `mod` modulus
