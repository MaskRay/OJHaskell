import Control.Applicative
import Control.Monad
import Control.Monad.State
import Data.Char
import Data.Maybe
import Data.List
import qualified Data.ByteString.Char8 as B

data T x y = T !x !y

parse = do
  _ <- readInt
  m <- readInt
  j <- readInt
  xs <- replicateM j readInt
  return (m,xs)
  where
    readInt = state $ fromJust . B.readInt . B.dropWhile isSpace

go m = t . foldl' f (T 1 0)
  where
    t (T _ s) = s
    f (T l s) x
      | x < l = T x (s+l-x)
      | l+m <= x = T (x-m+1) (s+x-l-m+1)
      | otherwise = T l s

main = do
  (m,xs) <- evalState parse <$> B.getContents
  print $ go m xs
