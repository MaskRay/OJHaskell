currency = [1,2,5,10,20,50,100,200]

f = foldl (\without c -> let (a, b) = splitAt c without
                             with = a ++ zipWith (+) with b
                         in  with)
          (1 : repeat 0)

main = print . (!! 200) . f $ currency