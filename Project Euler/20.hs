import Data.Char
main = print . sum . map digitToInt . show . product $ [1..100]