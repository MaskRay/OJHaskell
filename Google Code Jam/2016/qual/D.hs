import Control.Monad
import Data.Foldable
import Data.Maybe
import Text.Printf
import qualified Data.ByteString.Char8 as B

int = fst . fromJust . B.readInt
ints = map int . B.words

main = do
  cases <- int <$> B.getLine
  forM_ [1..cases] $ \cc -> do
    printf "Case #%d:" cc
    [k, c, s] <- ints <$> B.getLine
    if c*s < k
       then putStrLn " IMPOSSIBLE"
       else do
         forM_ [0,c..k-1] $ \i ->
           printf " %d" . (+1) $ foldl' (\x j -> x * k + min j (k-1)) 0 [i..i+c-1]
         putStrLn ""
