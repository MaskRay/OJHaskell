main = do
  cases <- fmap read getLine
  mapM_ (\_ -> fmap read getLine >>= putStrLn . (["Akash","Aayush"]!!) . foo) [1..cases]

foo 0 = 0
foo n
  | even n = 1 - foo (div n 2)
  | otherwise = 1 - foo (n-1)