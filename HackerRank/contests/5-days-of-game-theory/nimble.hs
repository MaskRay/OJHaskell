import Control.Monad
import Data.Bits
import Data.Functor
import Data.List

main = do
  cases <- readLn
  replicateM cases $ do
    getLine
    t <- (zip [0..] . map read . words) <$> getLine :: IO [(Int, Int)]
    putStrLn $ if foldl' (\s (x,y) -> if odd y then s `xor` x else s) 0 t == 0 then "Second" else "First"
