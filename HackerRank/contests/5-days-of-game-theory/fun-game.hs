import Control.Monad
import Data.Function
import Data.Functor
import Data.List

main = do
  cases <- readLn
  replicateM cases $ do
    getLine
    xs <- (map read . words) <$> getLine :: IO [Int]
    ys <- (map read . words) <$> getLine :: IO [Int]
    let go [] _ acc = acc
        go ((xy,x):xys) True acc = go xys False (acc+x)
        go ((xy,x):xys) False acc = go xys True (acc-(xy-x))
        r = go (sortBy (flip compare) $ zipWith (\x y -> (x+y,x)) xs ys) True 0
    putStrLn $ case signum r of
      1 -> "First"
      0 -> "Tie"
      -1 -> "Second"
