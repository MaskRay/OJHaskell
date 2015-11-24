f x y
  | n >= 10^12 = (x+2*y+1) `div` 2
  | otherwise = f (3*x+8*y) (3*y+x)
  where n = (x+1) `div` 2 + 2 * y

main = print $ f 3 1