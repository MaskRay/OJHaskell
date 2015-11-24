main = print $ snd $ minimum [(abs (a*(a+1)*b*(b+1)-8000000), a*b) | a <- [1..100], b <- [1..100]]
