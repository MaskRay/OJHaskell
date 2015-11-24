isPentagonal n = b == 0 && a `mod` 6 == 5
             where (a, b) = properFraction . sqrt . fromIntegral $ 24*n+1

main = print . head . filter (> 40755) . filter isPentagonal . scanl1 (+) $ [1,5..]