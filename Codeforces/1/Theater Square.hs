main = do
     [m,n,a] <- return . map read . words =<< getLine
     print $ ((m+a-1) `div` a) * ((n+a-1) `div` a)