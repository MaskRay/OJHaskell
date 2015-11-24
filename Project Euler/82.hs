import Data.List

main = do
     s <- readFile "matrix.txt"
     let a = transpose . map (\x -> read ("["++x++"]")) . lines $ s
     print $ minimum $ foldl1 (\u v ->
           let l1 = (head u + head v) : zipWith3 (\x y z -> x + min y z) (tail v) l1 (tail u)
               v' = reverse v
               l1' = reverse l1
               l2 = head l1' : zipWith3 (\x y z -> min x (y+z)) (tail l1') l2 (tail v')
           in  reverse l2) a
