import Data.Ratio

prob71 f limit = traverse 0 1 1 0
       where traverse a b c d
                      | d > 0 && c%d == f = let count = min ((limit-a) `div` c) ((limit-b) `div` d)
                                            in  a+c*count
                      | m%n >= f = traverse a b m n
                      | otherwise = traverse m n c d
                      where m = a+c
                            n = b+d

main = print $ prob71 (3%7) 1000000