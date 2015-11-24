monday = 1
sunday = 0

since1990 = scanl (\x y -> (x + y) `mod` 7) monday . concat $ replicate 4 nonLeap ++ cycle (leap : replicate 3 nonLeap)

nonLeap = [31,28,31,30,31,30,31,31,30,31,30,31]
leap = 31 : 29 : drop 2 nonLeap

main = print . length . filter (== sunday) . take 1200 . drop 12 $ since1990