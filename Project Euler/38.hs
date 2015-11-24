import Data.List
import Data.Char

f x i s
  | length s > 9 = 0
  | length s == 9 = if sort s == ['1'..'9'] then foldl1 (\x y -> x * 10 + y) $ map digitToInt s else 0
  | otherwise = f x (i+1) (s ++ show (x*i))

main = print . maximum $ [f x 1 "" | x <- [1..9999]]