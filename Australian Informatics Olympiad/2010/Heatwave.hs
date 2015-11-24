import Data.List
import System.IO

main = do
  file <- openFile "heatin.txt" ReadMode
  [_, h] <- fmap words (hGetLine file)
  t <- fmap (map read . words) (hGetContents file)
  writeFile "heatout.txt" . (++"\n") . show . maximum . (0:) . map length . filter ((==1) . head) . group . map (\x -> if (x :: Int) >= read h then 1 else 0) $ t
