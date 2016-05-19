import Data.Function
import Data.Functor
import Data.List

f [] = return ()
f ((cx,x):xs) = do
  putChar x
  f $ if cx == 1 then xs else (cx-1,x):xs


g _ [] = return ()

-- The first two and the last characters are the same, thus we can not use 'x'
g 0 [x] = g 1 [x]
g 0 ((cx,x):(cy,y):xs) = do
  putChar y
  if cy == 1
     then g 1 $ (cx,x):xs
     else g 1 $ (cx,x):(cy-1,y):xs

-- The first character is different to the last, and we can use 'x'
g 1 ((cx,x):xs) = do
  putChar x
  if cx == 1
     then g 2 xs
     else g 0 $ (cx-1,x):xs

-- The first character has been used up
g 2 ((cx,x):xs) = do
  putChar x
  if cx == 1
     then g 2 xs
     else g 2 $ (cx-1,x):xs

main = do
  a <- (map read . words) <$> getLine
  let (xc,x):xs = sort . filter ((0/=) . fst) $ zip a ['a'..]
  putChar x
  let ys = sortBy (compare `on` snd) $ if xc == 1 then xs else (xc-1,x):xs
  case ys of
    (_,y):_ | x == y -> g 1 ys
    _ -> f ys
