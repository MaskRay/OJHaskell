import Data.Functor
import Data.List
import Data.Maybe
import qualified Data.ByteString.Lazy.Char8 as L

main = do
  (n:m:ts) <- (map (fst . fromJust . L.readInt) . L.words) <$> L.getContents
  let (a, b) = splitAt n ts
      f l h | l == h-1 = l
            | otherwise =
                if sum ys <= m
                then f x h
                else f l x
        where
          x = (l+h) `div` 2
          ys = take x . sort $ zipWith (\a b -> a+(x-1)*b) a b
  print $ f 0 (n+1)
