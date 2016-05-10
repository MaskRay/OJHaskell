import Control.Monad
import Data.Maybe
import Text.Printf
import qualified Data.ByteString.Char8 as B

main = do
  cases <- (fst . fromJust . B.readInt) <$> B.getLine
  forM_ [1..cases] $ \cc -> do
    [a] <- B.words <$> B.getLine
    let n = fst $ B.foldl' (\(l,x) c -> (l + fromEnum (x /= c), c)) (0, ' ') a
    printf "Case #%d: %d\n" cc $ if B.last a == '+' then n-1 else n
