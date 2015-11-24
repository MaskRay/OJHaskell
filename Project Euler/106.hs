binomial n m = product [n-m+1..n] `div` product [1..m]

prob106 n = loop 3 1 [0,1,3]
        where loop m mm l
                   | m > n = 0
                   | otherwise = let l' = 0 : map (\i -> l!!(i-1)+l!!i) [1..div (m+1) 2]
                                     ans = if mm*2 < m then binomial n (m+1) * (l !! mm) else 0
                                     (l'', mm') = if mm*2 == m
                                           then (l'++[binomial (m+1) (mm+1)], mm)
                                           else (l', mm+1)
                                 in ans + loop (m+1) mm' l''
