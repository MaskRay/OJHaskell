import Control.Monad
import Data.Bits
import Data.Functor
import Data.List

main = do
  cases <- readLn
  replicateM cases $ do
    getLine
    t <- (map read . words) <$> getLine :: IO [Int]
    let g = all (==1) t
        s = foldl1' xor t
    putStrLn $ if g && s /= 0 || not g && s == 0 then "Second" else "First"
