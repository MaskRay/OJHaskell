import Control.Monad
import Data.Functor
import Data.List

main = do
  n <- readLn
  xs <- replicateM n $ ((\[x,y] -> (read x, read y)) . words) <$> getLine :: IO [(Double, Double)]
  print $ sum $ zipWith (\(x0,y0) (x1,y1) -> sqrt $ (x0-x1)^2+(y0-y1)^2) (tail $ cycle xs) xs
