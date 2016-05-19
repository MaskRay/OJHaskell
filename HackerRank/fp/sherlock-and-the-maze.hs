import Control.Monad
import Data.Functor
import Data.List

main = do
  t <- readLn
  replicateM_ t $ do
    [n, m, k] <- (map read . words) <$> getLine
    let g n m = sum . map (last . last) . take (k+1) . iterate (map (init . scanl (+) 0) . transpose) $ (0:replicate (m-1) 1):replicate (n-1) (replicate m 0)
    print $ if n * m == 1 then 1 else (g n m + g m n) `mod` 1000000007
