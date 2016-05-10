import Control.Monad.State
import Data.Char
import Data.Functor
import Data.Maybe
import Text.Printf
import qualified Data.ByteString.Char8 as B

int = state $ fromJust . B.readInt . B.dropWhile isSpace

parse = do
  n <- int
  a <- replicateM n int
  return (n, a)

main = do
  (n, a) <- evalState parse <$> B.getContents
  let s = sum a
  putStr $ if even s then "Yes" else "No"
  printf " %d\n" s
