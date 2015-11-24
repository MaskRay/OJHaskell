import Data.Set

pentagonals = [n*(3*n-1) `div` 2 | n <- [1..3000]]

main = print . head $ [a-b | a <- pentagonals,
     b <- takeWhile (< a) pentagonals,
     isPentagonal $ a+b,
     isPentagonal $ a-b]
     where s = fromAscList pentagonals
           isPentagonal n = n `member` s