import Data.List

main = do
     contents <- readFile "base_exp.txt"
     let a = map (\x -> read ("["++x++"]") :: [Double] ) $ lines contents
     let Just d = (`elemIndex` a) (maximumBy (\ [x1,y1] [x2,y2] -> (y1*log x1) `compare` (y2*log x2)) a)
     print (d+1)