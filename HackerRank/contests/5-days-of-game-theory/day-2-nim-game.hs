import Control.Monad
import Data.Bits
import Data.Functor
import Data.List

main = do
  cases <- readLn
  replicateM cases $ do
    getLine
    t <- (map read . words) <$> getLine :: IO [Int]
    putStrLn $ if foldl1' xor t /= 0 then "First" else "Second"
