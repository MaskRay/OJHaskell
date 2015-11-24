main = print . snd . foldl (\(n,ans) (s:ss) ->
     if s == '+' then (n+1,ans)
     else if s == '-' then (n-1,ans)
     else (n, n * (pred $ length $ dropWhile (/=':') ss) + ans)
     ) (0,0) =<< return . lines =<< getContents