import Data.List

main = interact $ take 10 . show . sum . map read . lines