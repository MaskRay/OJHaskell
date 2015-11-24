import Control.Applicative
import Control.Monad
import Data.List
import qualified Data.Map as M

main = do
  n <- read <$> getLine
  a <- replicateM 5 . replicateM n $ read <$> getLine :: IO [[Int]]
  let b = map (M.fromList . flip zip [0..]) a
  let cmp x y = if length [ m | m <- b, M.lookup x m < M.lookup y m] > 2 then LT else GT
  mapM_ print $ sortBy cmp $ head a