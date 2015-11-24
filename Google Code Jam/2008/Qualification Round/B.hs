import Control.Arrow
import Control.Monad
import Data.List
import Text.Printf

readTime s = hour * 60 + min
  where
    hour = read $ take 2 s
    min = read $ drop 3 s

solve as bs = loop (sort as) (sort bs)
  where
    loop [] _ = 0
    loop as [] = length as
    loop (a:as) (b:bs)
      | a >= b = loop as bs
      | True = loop as (b:bs) + 1

main = do
  cases <- liftM read getLine :: IO Int
  forM_ [1..cases] $ \cas -> do
    turnaround <- liftM read getLine
    [na,nb] <- liftM (map read . words) getLine
    a <- replicateM na $ liftM (map readTime . words) getLine
    b <- replicateM nb $ liftM (map readTime . words) getLine
    let ansa = solve (map head a) (map ((+turnaround).last) b)
        ansb = solve (map head b) (map ((+turnaround).last) a)
    printf "Case #%d: %d %d\n" cas ansa ansb