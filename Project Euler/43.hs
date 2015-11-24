import Control.Monad
import Control.Monad.State
import qualified Data.IntSet as S

type Select = StateT S.IntSet []

primes :: [Int]
primes = [2,3,5,7,11,13,17]

select :: [Int] -> Select Int
select xs = do
    set <- get
    x <- lift xs
    guard $ not $ S.member x set
    put $ S.insert x set
    return x

p43 :: Int -> [Int] -> Select [Int]
p43 (-1) xs = return xs
p43 k xs = do
    d <- select [0..9]
    guard $ k == 0 || k > 7 || (d*100+(head xs)*10+(xs!!1)) `mod` (primes!!(k-1)) == 0
    p43 (k-1) (d:xs)

main = print . sum . map (foldl (\x y -> x*10+y) 0 . fst) $ runStateT (p43 9 []) S.empty
