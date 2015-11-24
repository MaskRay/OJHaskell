calc a v l d w
     | v <= w = calc2 l 0
     | otherwise = let tw = w/a
                       dw = dist 0 tw
                   in  if dw >= d then
                          calc2 l 0
                       else tw+2*calc2 ((d-dw)/2) w+calc2 (l-d) w
     where calc2 len v0 = let t1 = (v-v0)/a
                              t2 = ((sqrt $ v0*v0+2*a*len) - v0) / a
                          in  if t1 > t2 then t2
                              else t1+(len-dist v0 t1)/v
           dist v0 t = v0*t+a*t*t/2

main = do
     [a, v] <- return . (map read :: [String] -> [Double]) . words =<< getLine
     [l, d, w] <- return . (map read :: [String] -> [Double]) . words =<< getLine
     print $ calc a v l d w