import Data.Char
import Data.Set

s = fromList [x*(x+1) `div` 2 | x <- [1..100]]

main = do
     content <- readFile "words.txt"
     print . length . Prelude.filter (\w -> foldl (\x c -> x + ord c - ord 'A' + 1) 0 w `member` s) . read $ "["++content++"]"


