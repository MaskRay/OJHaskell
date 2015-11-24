import Control.Monad

main = do
     d:sumt:[] <- getLine >>= return . (map read :: [String] -> [Int]) . words
     a <- getContents >>= return . map ((map read :: [String] -> [Int]) . words) . lines
     let sum_min = sum $ map (\(x:xs) -> x) a
     let sum_max = sum $ map (\(_:x:xs) -> x) a
     if sumt < sum_min || sum_max < sumt
        then putStrLn "NO"
        else putStrLn "YES" >> foldM_ (\rest [x,y] -> print (x+min rest (y-x)) >> return (rest-min rest (y-x))) (sumt - sum_min) a