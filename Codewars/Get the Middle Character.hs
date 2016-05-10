module Codewars.G964.Getmiddle where

getMiddle :: String -> String
getMiddle s = f s (tail s)
  where
    f x "" = take 1 x
    f x [_] = take 2 x
    f (_:xs) (_:_:ys) = f xs ys
