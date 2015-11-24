import Control.Monad
import Data.Bits
import System.IO.Unsafe

feasible :: [Int] -> [Int] -> Int -> Bool
feasible timeJ timeB mask =
  (/= Nothing) $ foldM (\t x -> if shiftL 1 x .&. mask /= 0 then
                   return t
                 else
                   if t < timeB!!x then Nothing
                   else return (t+timeJ!!x-timeB!!x)
        ) totJ [0..length timeJ-1]
  where totJ = foldl (\s x -> if shiftL 1 x .&. mask /= 0 then s+timeJ!!x else s
                         ) 0 [0..length timeJ-1]

find :: [Int] -> [Int] -> Int
find timeJ timeB = length $ filter (
  \mask -> feasible timeJ timeB mask &&
           feasible timeB timeJ (complement mask)) [0..shiftL 1 (length timeJ) - 1]

main = do
  (n:ls) <- fmap lines getContents
  let (timeJ,ls') = splitAt (read n) ls
      (_:timeB) = ls'
  print $ find (map read timeJ) (map read timeB)