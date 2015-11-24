import Control.Applicative
import Control.Monad
import Data.List
import Data.Array

count :: Array (Int, Int) Char -> Char -> String
count a c = show . length . group . sort $ do
      let b = bounds a
      (x, y) <- range b
      guard $ a ! (x, y) == c
      idx' <- [(x+1, y), (x-1, y), (x, y-1), (x, y+1)]
      guard $ inRange b idx'
      let c' = a ! idx'
      guard $ c' /= '.' && c' /= c
      return c'

solve :: [String] -> [String]
solve = fmap return $ do
      [nn, mm, cc] <- words . head
      let n = read nn; m = read mm; c = head cc
      xs <- concat . take n . tail
      let a = listArray ((1,1), (n,m)) xs
      return $ count a c

main = interact $ unlines . solve . lines