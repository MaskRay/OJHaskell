module NIM where
import Data.Bits

-- | Returns the index and the number picked from a board
chooseMove :: [Int] -> (Int,Int)
chooseMove xs = f 0 xs
  where
    s = foldl1 xor xs
    f i (x:xs) = if d > 0 then (i,d) else f (i+1) xs
      where
        d = x - (x `xor` s)
