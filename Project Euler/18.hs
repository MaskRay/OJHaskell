main = print . head . foldr1 g . map (map read . words) . lines =<< getContents
     where g xs ys = zipWith3 (\x y z -> x + max y z) xs ys $ tail ys