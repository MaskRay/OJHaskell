{-# LANGUAGE NoMonomorphismRestriction #-}

calc2 :: Integer -> Integer -> Integer -> Integer -> Integer
-- calc2 n a b m = sum $ map (\i -> (a+b*i)`div`m) [0..n-1]
calc2 n a b m
  | n == 0 = 0
  | b == 0 = a`quot`m*n
  | b < 0 = calc2 n (a+(n-1)*b) (-b) m
  | a >= m = calc2 n (a `rem` m) b m + a`quot`m*n
  | b >= m = calc2 n a (b `rem` m) m + b`quot`m*n*(n-1)`quot`2
  | otherwise = calc2 ((a+n*b)`quot`m) ((a+n*b)`rem`m) m b

calc :: Integer -> Integer -> Integer -> Integer -> Integer -> Integer
calc x1 y1 x2 y2 m
  | x1 == x2 = 0
  | x1 > x2 = -calc x2 y2 x1 y1 m
  | otherwise = let l = (x1+m-1)`div`m*m
                    r = (x2-1)`div`m*m
                    n = (r-l)`div`m+1
                in calc2 n (((y2-y1)*l+y1*x2-x1*y2) `div` m) (y2-y1) (x2-x1)

main = do
  let readLine = fmap read getLine
  n <- readLine
  xs <- mapM (const readLine) [1..n] :: IO [Integer]
  _ <- getLine
  ys <- mapM (const readLine) [1..n] :: IO [Integer]
  denom <- readLine

  let ps' = zip xs ys
      ps = last ps':ps'
  print $ abs $ sum $ zipWith (\(x1,y1) (x2,y2) -> calc x1 y1 x2 y2 denom) ps (tail ps)