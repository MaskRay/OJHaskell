calc :: [Int] -> [Int] -> Double
calc low high = sum $ zipWith
       (\(a,b) (c,d) ->
         sum [
           (let l = c; r = min i d
            in if l <= r then (i-(l+r)/2.0)*(r-l+1)/(d-c+1)
               else 0.0) -
           (let r = d; l = max i c
            in if l <= r then (i-(l+r)/2.0)*(r-l+1)/(d-c+1)
               else 0.0)
           | i <- [a..b]] / (b-a+1)) f $ tail f
  where f = zip (fmap fromIntegral low :: [Double]) (fmap fromIntegral high :: [Double])

main = do
  ([nn], c) <- fmap (splitAt 1 . lines) getContents
  let n = read nn
      (low, dummy:high) = splitAt n c
  print $ calc (fmap read low) (fmap read high)