import Control.Monad
import Control.Monad.State
import Data.Char
import Data.Int
import Data.List
import Data.Maybe
import qualified Data.ByteString.Char8 as B

parse = do
  n <- readInt
  ds <- replicateM n (liftM2 (-) readInt readInt)
  return ds
  where
    readInt = state $ fromJust . B.readInt . B.dropWhile isSpace

main = do
  ds <- evalState parse `fmap` B.getContents
  let ss = scanl1 (+) ds
      m = sort ss !! ((length ss) `div` 2)
      res = sum $ map (fromIntegral.abs.(m-)) ss :: Int64
  print res
