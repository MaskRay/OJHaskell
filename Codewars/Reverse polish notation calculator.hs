module RPN where

calc :: String -> Double
calc line = f [] $ words line
  where
    f [] [] = 0
    f (x:_) [] = x
    f xs'@ ~(y:x:xs) (o:os) =
      case o of
           "+" -> f ((x+y):xs) os
           "-" -> f ((x-y):xs) os
           "*" -> f ((x*y):xs) os
           "/" -> f ((x/y):xs) os
           _ -> f (read o:xs') os
