import Control.Monad.State
import Data.Char
import Data.Functor
import Data.Maybe
import qualified Data.ByteString.Char8 as B

int = state $ fromJust . B.readInt . B.dropWhile isSpace

parse = do
  cases <- int
  replicateM cases int

main = do
  ns <- evalState parse <$> B.getContents
  forM_ ns $ \n ->
    putStrLn $ if n `rem` 8 == 0 then "Second" else "First"
