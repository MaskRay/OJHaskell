import Data.List

main = do
     s <- readFile "matrix.txt"
     print . last . foldl (\u v -> f u v) (0:repeat (10^100)) . fmap (\s -> read $ "["++s++"]") . lines $ s
     where f u v = let l = (u!!0+v!!0) : zipWith3 (\x y z -> x+min y z) (tail v) l (tail u)
                   in  l