import Data.Array.IArray

main = do
  [n, m] <- fmap (fmap read . words) getLine
  forbidden <- mapM (const
                     (fmap ((\ [x,y] -> [((read x,read y),True),((read y,read x),True)]) . words) getLine)
                    ) [1..m]
  let a = accumArray (||) False ((1,1),(n,n)) (concat forbidden) :: Array (Int,Int) Bool
  print $ length [() | x <- [1..n], y <- [x+1..n], z <- [y+1..n], not (a!(x,y) || a!(y,z) || a!(z,x))]