prob53 n = loop 0 [1] 0
       where loop m l ans
                  | m > n = ans
                  | otherwise = loop (m+1) (zipWith (+) (0:l) $ reverse (0:l)) (ans + length (filter (> 1000000) l))

main = print . prob53 $ 100
