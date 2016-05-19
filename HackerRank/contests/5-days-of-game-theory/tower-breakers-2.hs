import Control.Monad
import Data.Bits
import Data.Functor
import Data.List

primes = go [2..1000]
  where
    go [] = []
    go (x:xs) = x : go (xs \\ [x,x+x..1000])

f n = go n primes 0
  where
    go 1 _ acc = acc
    go _ [] acc = acc+1
    go x (p:ps) acc
      | p*p > x = acc+1
      | x `rem` p == 0 = go (x `quot` p) (p:ps) (acc+1)
      | otherwise = go x ps acc

main = do
  cases <- readLn
  replicateM cases $ do
    getLine
    t <- (map (f . read) . words) <$> getLine :: IO [Int]
    print $ if foldl1' xor t == 0 then 2 else 1
