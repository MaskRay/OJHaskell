prob9 l = head [a * b * c | m <- [2..limit], n <- [1..limit-1], let a = m^2-n^2; b = 2*m*n; c = m^2+n^2, a+b+c == l]
      where limit = floor $ sqrt $ fromIntegral l

main = print $ prob9 1000