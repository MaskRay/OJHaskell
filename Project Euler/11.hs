import Data.Array
import Control.Arrow

dirs = [succ *** id, succ *** succ, id *** succ, succ *** pred]

input = listArray ((1,1), (20,20)) . map read . words

inArray a i = inRange (bounds a) i

prog11 :: Array (Int, Int) Int -> Int
prog11 a = maximum [product $ map (a!) xs |
                            i <- range $ bounds a,
                            d <- dirs,
                            let xs = take 4 $ iterate d i,
                            inArray a $ last xs]

main = print . prog11 . input =<< getContents