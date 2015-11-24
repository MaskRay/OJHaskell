main = print . head . foldr1 f . map (map read . words) . lines =<< getContents
     where f x y = zipWith3 (\x y z -> x + max y z) x y $ tail y