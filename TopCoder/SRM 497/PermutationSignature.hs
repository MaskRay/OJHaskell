reconstruct :: String -> [Int]
reconstruct =
  foldr (\x l ->
          if x == 'I' then
            1 : fmap succ l
          else
            head l + 1 : fmap (\x -> if x <= head l then x else x+1) l
        ) [1]

main = do
  s <- getLine
  let l = reconstruct s
  print $ length l
  putStr $ unlines $ fmap show l