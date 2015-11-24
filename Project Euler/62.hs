import Data.List

a = map (sort . show) $ map (^3) [0..10000]
b = filter ((==5) . length) . group . sort $ a
Just c = head (head b) `elemIndex` a
main = print (toInteger c ^ 3)