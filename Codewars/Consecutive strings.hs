module Codewars.G964.Longestconsec where
import Data.Function
import Data.List

longestConsec :: [String] -> Int -> String
longestConsec ss k =
  if length ss >= k
  then concat . take k . snd $ minimumBy (compare `on` fst) cs
  else ""
  where
    ls = scanl (+) 0 $ map length ss
    cs = zipWith3 (\x y s -> (x-y,s)) ls (drop k ls) (tails ss)
    cmp a b = (a++b) `compare` (b++a)
