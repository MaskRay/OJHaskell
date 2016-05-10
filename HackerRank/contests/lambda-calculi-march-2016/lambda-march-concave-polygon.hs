import Control.Monad
import Data.Functor
import Data.List

sub (x0, y0) (x1, y1) = (x0-x1, y0-y1)

cross (x0, y0) (x1, y1) = x0*y1-y0*x1

monotoneChain xs' =
  let xs = sort xs'
      ys = reverse xs
      go [] _ acc = acc
      go (z:zs) n acc@ ~(y:x:xs) =
        if n > 1 && cross (sub y x) (sub z x) < 0
        then go (z:zs) (n-1) (x:xs)
        else go zs (n+1) (z:acc)
  in
  tail (go xs 0 []) ++ tail (go (tail ys) 1 [head ys])

main = do
  n <- readLn
  xs <- replicateM n $ ((\[x,y] -> (read x, read y)) . words) <$> getLine :: IO [(Int, Int)]
  let ys = monotoneChain xs
  putStrLn $ if length xs == length ys then "NO" else "YES"
