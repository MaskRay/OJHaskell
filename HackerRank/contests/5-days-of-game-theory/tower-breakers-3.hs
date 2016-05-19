import Control.Monad
import Data.Bits
import Data.Functor
import Data.List

primes = go [2..400]
  where
    go [] = []
    go (x:xs) = x : go (xs \\ [x,x+x..400])

f n = go n primes 0 0
  where
    go x (p:ps) t acc
      | x `rem` p == 0 = go (x `quot` p) (p:ps) (t+1) acc
      | otherwise =
        let g = if p == 2 then acc+min t 1 else acc+t in
        if p*p > x
        then if x == 1 then g else g+1
        else go x ps 0 g

main = do
  cases <- readLn
  replicateM cases $ do
    getLine
    t <- (map (f . read) . words) <$> getLine :: IO [Int]
    print $ if foldl1' xor t == 0 then 2 else 1
