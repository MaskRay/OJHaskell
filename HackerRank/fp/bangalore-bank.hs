{-# LANGUAGE ViewPatterns #-}
import Control.Monad
import Data.List

tr x = if x == 0 then 9 else x-1

solve :: [Int] -> Int
solve (map tr -> x:xs) = ((length xs+1)+) . minimum . snd . foldl' (\(y, xs) z ->
  let t = map (abs (y-z) +) xs in
  (z, snd . mapAccumR (\r x -> join (,) $ min (r+1) x) 99999 . snd . mapAccumL (\l x -> join (,) $ min (l+1) x) 99999 $ take y t ++ (xs!!z) : drop (y+1) t)
  ) (x, zero) $ xs
  where
    zero = replicate 10 0

main = interact $ (++"\n") . show . solve . map read . words . last . lines
