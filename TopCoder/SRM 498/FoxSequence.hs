isValid :: [Int] -> String
isValid l = let a = foldr (\x a -> if null a || head a /= x then x:a else a) [] $ zipWith subtract l (tail l)
            in  if length a == 4 && a!!0 > 0 && a!!1 < 0 && a!!2 > 0 && a!!3 < 0 ||
                   length a == 5 && a!!0 > 0 && a!!1 < 0 && a!!2 == 0 && a!!3 > 0 && a!!4 < 0
                   then "YES"
                   else "NO"

main = do
     n <- fmap read getLine
     l <- fmap (fmap read . take n . words) getContents
     putStrLn $ isValid l