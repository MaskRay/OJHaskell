import Data.Char

factorial = scanl (*) 1 [1..9]

main = print . sum $ [x | x <- [10..999999], (sum . map (product . enumFromTo 1 . digitToInt) . show $ x) == x]