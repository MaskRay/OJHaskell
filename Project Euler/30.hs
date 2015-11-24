import Data.Char

main = print . sum $ [x | x <- [10..999999], sum (map ((^ 5) . digitToInt) (show x)) == x]