p = 1000000007

f a b c a' b' c' n
  | n == 0 = c
  | n `mod` 2 == 1 = f ((a*a'+b*b') `mod` p) ((a*b'+b*c') `mod` p) ((b*b'+c*c') `mod` p) a'' b'' c'' (n `div` 2)
  | otherwise = f a b c a'' b'' c'' (n `div` 2)
  where a'' = (a'*a'+b'*b') `mod` p
        b'' = (a'*b'+b'*c') `mod` p
        c'' = (b'*b'+c'*c') `mod` p

calc 0 = 0
calc 1 = 2
calc n = f 0 1 1 0 1 1 (n+1)
main = fmap (tail . lines) getContents >>= mapM_ (\n -> print $ calc $ read n)