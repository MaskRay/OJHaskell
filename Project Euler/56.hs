import Data.Char

main = print . maximum $ [sum . map digitToInt . show $ a^b | a <- [1..99], b <- [1..99]]