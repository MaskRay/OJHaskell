import Control.Monad

main = do
  n <- fmap read getLine
  a <- replicateM n $ (map read . words) `fmap` getLine
  [sx,sy] <- (map read . words) `fmap` getLine :: IO [Int]
  print $ foldl (\acc (i,c) -> if c!!0<=sx&&sx<=c!!0+c!!2&&c!!1<=sy&&sy<=c!!1+c!!3 then i else acc) (-1) $ zip [1..] a
