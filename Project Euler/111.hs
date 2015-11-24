import Control.Monad
import Data.Function
import Data.List
import Data.Ord

isPrime n = and [n `mod` p /= 0 | p <- takeWhile (\x -> x*x <= n) primes]
primes = 2 : filter isPrime [3,5..]

gen n d = [ (length $ filter (==[d]) s, s'')
          | s <- replicateM n [delete d [0..9], [d]]
          , let s' = delete 0 (head s) : tail s
          , let s'' = map (foldl1 $ \x y -> x*10+y) $ sequence s'
          ]

p111 n d = sum . head . dropWhile null . map (filter isPrime . concatMap snd) . reverse . groupBy ((==) `on` fst) . sortBy (comparing fst) $ gen n d

main = print . sum $ map (p111 10) [0..9]
