import Control.Monad
import Data.Functor
import Data.List

main = do
  n <- readLn
  xs <- replicateM n $ ((\[x,y] -> (read x, read y)) . words) <$> getLine :: IO [(Double, Double)]
  print $ abs $ (/2) $ sum $ zipWith (\(x0,y0) (x1,y1) -> x0*y1-y0*x1) (tail $ cycle xs) xs
