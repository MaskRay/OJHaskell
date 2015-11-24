import Data.List

sameDigits a b = (show a) \\ (show b) == [] && (show b) \\ (show a) == []

main = print . head . filter (\n -> all (sameDigits n) $ map (n*) [2..6]) $ [1..]