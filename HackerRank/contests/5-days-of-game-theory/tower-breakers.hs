import Control.Monad

main = do
  cases <- readLn
  replicateM cases $ do
    [n, m] <- fmap (map read . words) getLine :: IO [Int]
    print $ if odd n && m > 1 then 1 else 2
