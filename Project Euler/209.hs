import Control.Monad
import Data.Bits
import Data.List

loops :: [[[Int]]]
loops = f $ mapM (const [0,1]) [1..6]
        where f [] = []
              f (x:xs) = (getAll x [x]) : (f $ xs \\ getAll x [x])

getAll x xs = let x' = transform x
              in if x' == last xs
                 then xs
                 else getAll x' (x':xs)

transform [a,b,c,d,e,f] = [b,c,d,e,f,a`xor`(b.&.c)]

lucas = 2 : scanl (+) 1 lucas

main = print $ product $ map ((lucas!!) . length) loops
