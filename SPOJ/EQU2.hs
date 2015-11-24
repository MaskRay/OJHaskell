import Text.Printf

pell :: Integer -> String
pell n = loop 1 0 a0 1  0 1 a0
     where a0 = floor . sqrt . fromIntegral $ n
           loop p' q' p q g h a
                | p^2-n*q^2 == 1 = printf "%d %d" p q
                | otherwise = loop p q p1 q1 g1 h1 a1
                where p1 = a1*p+p'
                      q1 = a1*q+q'
                      g1 = a*h-g
                      h1 = (n-g1^2) `div` h
                      a1 = (g1+a0) `div` h1

main = interact $ unlines . map (pell . read) . tail . lines