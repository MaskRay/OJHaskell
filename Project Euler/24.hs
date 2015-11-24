import Data.List

perm "" _ = ""
perm xs k = x : perm (delete x xs) (k `mod` m)
     where m = product [1..length xs - 1]
           x = xs !! (k `div` m)

main = print $ perm "0123456789" 999999