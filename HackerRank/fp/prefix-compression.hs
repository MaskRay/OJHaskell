import Data.List
import Text.Printf

main = do
  a <- getLine
  b <- getLine
  let c = takeWhile (uncurry (==)) $ zip a b
      l = length c
      a' = drop l a
      b' = drop l b
  printf "%d %s\n" l $ map fst c
  printf "%d %s\n" (length a') a'
  printf "%d %s\n" (length b') b'
