import Data.Graph
import Data.Char
import Data.List

main = do
     log <- readFile "keylog.txt"
     let usedDigits = intersect ['0'..'9'] log
         edges = concat $ map ((\ [a,b,c] -> [(a,b),(b,c)]) . map digitToInt) $ words log
     putStrLn $ (`intersect` usedDigits) $ map intToDigit $ topSort $ buildG (0,9) edges
