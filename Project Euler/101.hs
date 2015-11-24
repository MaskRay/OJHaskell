import Control.Monad
import Control.Monad.Instances
import Data.List

f t n = sum $ zipWith (*) t $ iterate (*n) 1
t = 1 : concat (replicate 5 [-1,1])

induce = sum . map last . takeWhile (not . null) . iterate (tail >>= zipWith (-))
p101 = sum . map (induce . map (f t)) . tail $ inits [1..length t-1]
