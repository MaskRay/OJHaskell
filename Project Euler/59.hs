import Control.Monad
import Data.Char
import Data.Bits
import Data.List
import Data.Ord

main = do
     s <- readFile "cipher1.txt"
     let ss = (read $ "["++s++"]") :: [Int]
         ss' = filter possible [map chr $ zipWith xor (cycle key) ss | key <- replicateM 3 [97..122]]
         message = maximumBy (comparing $ length . (filter (== ' '))) ss'
     print . sum . map ord $ message

     where possible = all (\c -> 32 <= ord c && ord c <= 122)