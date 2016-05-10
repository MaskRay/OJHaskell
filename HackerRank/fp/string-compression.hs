import Data.List
main = interact $ concatMap (\x -> if null $ tail x then x else head x:show (length x)) . group
