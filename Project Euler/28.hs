main = print $ sum [topright n + topleft n | n <- [-500..500]] - 1
     where topright n = (2*n-1)^2+2*n
           topleft n | n >= 0 = (2*n+1)^2
                     | otherwise = 4*n^2+1
