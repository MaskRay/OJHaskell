import Data.List

isBouncy n = sort s /= s && reverse (sort s) /= s
         where s = show n

findProportion prop = snd . head . filter (\(a,b) -> a >= prop * fromIntegral b) . zip [1..]

main = print $ findProportion 0.99 $ filter isBouncy [1..]