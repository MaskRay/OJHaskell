import Data.List

permute [] = [[]]
permute l = concatMap (\(x:xs) -> map (x:) $ permute xs) $ take (length l) $ unfoldr (\l@(x:xs) -> Just (l, xs++[x])) l

main = putStrLn $ maximum $ map (concatMap show) solutions
     where
     solutions = [concat handles |
               external <- map (6:) $ permute [6+1..10],
               internal <- map (\(x:xs) -> zipWith (\x y -> [x,y]) (x:xs) (xs++[x])) $ permute [1..5],
               let handles = zipWith (:) external internal,
               length (nub $ map sum handles) == 1]