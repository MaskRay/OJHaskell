ast n = replicate n '*'
spc n = replicate n ' '

f w [] _ = []
f w (l:ls) left =
  let n = (w-length l) `div` 2
  in if (w-length l) `mod` 2 == 0 then
        ("*"++spc n++l++spc n++"*") : f w ls left
     else
        ("*"++spc (n+left)++l++spc (n+1-left)++"*") : f w ls (1-left)

main = do
     contents <- return . lines =<< getContents
     let w = maximum $ map length contents
     mapM_ putStrLn $ [ast (w+2)]++f w contents 0++[ast (w+2)]