import Data.List
import Data.Char
import Data.Array

gen 1 m = fmap (:[]) [m..9]
gen n m = [a:b | a <- [m..9], b <- gen (n-1) a]

iter = until (\x -> x <= 1 || x == 89) next
     where next = sum . map (squares !) . show
           squares = listArray ('0','9') $ map (^2) [0..9]

count s = product $ map ((\len -> product [1..len]) . length) $ group s

main = print $ sum [factorial7 `div` count n | n <- gen 7 0 ,
     (==89) $ iter $ sum $ map (^2) n]
     where factorial7 = product [1..7]