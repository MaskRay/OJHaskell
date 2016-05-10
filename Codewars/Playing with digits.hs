module Codewars.Kata.DigPow where
import Data.List

digpow :: Integer -> Integer -> Integer
digpow n p =
  let ds = unfoldr (\x -> if x < 0 then Nothing else Just (x`rem`10, if x < 10 then -1 else x`quot`10)) n
      x = sum $ zipWith (^) (reverse ds) [p..] in
  if x `rem` n == 0 then x `quot` n else -1
