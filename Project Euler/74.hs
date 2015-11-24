import Data.Array
import Data.Char
import Data.Set (empty, member, insert)

limit = 2177280

next :: Int -> Int
next = (!) $ listArray (1,limit) $ map next' [1..limit]
  where
    next' = sum . map (factorial . digitToInt) . show
    factorial = (!) $ listArray (0,9) $ scanl (*) 1 [1..9]

check steps n = go steps n empty
  where
    go 0 n set = member n set
    go _ n set | member n set = False
    go steps n set = go (steps-1) (next n) (insert n set)

main = print . length . filter (check 60) $ [1..1000000]
