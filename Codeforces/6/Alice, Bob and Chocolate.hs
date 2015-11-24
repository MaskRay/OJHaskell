main = do
     dummy <- getLine
     a <- fmap (fmap read . words) getLine
     let b = scanl (+) 0 a
         alice = bsearch b 0 $ length a
         bob = length a - alice
     putStrLn $ show alice ++ " " ++ show bob
     where bsearch b l r
                   | l >= r = l
                   | otherwise = let m = (l+r) `div` 2
                                 in  if b!!m <= last b-b!!(m+1) then bsearch b (m+1) r else bsearch b l m