import Control.Monad
import Data.Function
import Data.Functor
import Data.List
import Data.Maybe
import qualified Data.Map as M

f m [x,y] =
  if M.member x m
  then M.insertWith (\y yy -> if y == yy then y else (-1)) x y m
  else M.insert x y m

run = do
  n <- readLn
  xs <- replicateM n $ (map (read :: String -> Int) . words) <$> getLine
  putStrLn $ if isNothing . findIndex (<0) . M.elems $ foldl f M.empty xs then "YES" else "NO"

main = readLn >>= flip replicateM_ run
