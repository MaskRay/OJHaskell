main = print $ prob6 100
     where prob6 n = a - b
                 where a = (sum [1..n]) ^ 2
                       b = sum $ map (^2) [1..n]
