import Control.Arrow
import Control.Monad
import Data.List
import Text.Printf

main = do
  cases <- readLn :: IO Int
  forM_ [1..cases] $ \cc -> do
    n <- readLn
    a <- concatMap (map read . words) <$> replicateM (2*n-1) getLine :: IO [Int]
    printf "Case #%d: " cc
    putStrLn . unwords . map (show . fst) . filter (odd . snd) . map (head &&& length) . group $ sort a
