import Data.Array
import Data.Functor
import Data.List

main = do
  [n, m] <- (map read . words) <$> getLine :: IO [Int]
  as <- (map (pred . read) . words) <$> getLine :: IO [Int]
  let a = listArray (0, n-1) as
      go i x | i < 0 = x
             | y == n-1 = y+i
             | otherwise = go (i-1) y
        where
          y = a ! (x+1)
  print $ go (m-1) (-1) + 1
