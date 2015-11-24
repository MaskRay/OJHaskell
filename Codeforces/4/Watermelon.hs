main = do
     input <- getLine
     let n = read input
     putStrLn $ if n > 2 && n `mod` 2 == 0 then "YES" else "NO"