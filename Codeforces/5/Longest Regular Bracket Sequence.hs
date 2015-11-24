import Text.Printf

main = do
     line <- getLine
     putStrLn . (\(opt,cnt,_,_,_) -> printf "%d %d" (opt::Int) (cnt::Int))
              . foldl (\(opt,cnt,b,s,i) c ->
              if c == '(' then
                 (opt,cnt,b,i:s,i+1)
              else if null s then
                   (opt,cnt,i,[],i+1)
              else let s' = tail s
                       opt' = i - (if null s' then b else head s')
                   in  if opt' > opt then
                          (opt',1,b,s',i+1)
                       else
                          (opt,cnt+(if opt' == opt then 1 else 0),b,s',i+1)
              ) (0,1,-1,[],0) $ line