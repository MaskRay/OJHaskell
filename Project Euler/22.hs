import Data.Char
import Data.List

main = do
     input <- readFile "names.txt"
     let names = sort $ read $ "["++input++"]"
     print . sum . zipWith f names $ [1..]
     where f x i = (* i) . sum . map (\c -> ord c - ord 'A' + 1) $ x
