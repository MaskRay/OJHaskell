import Control.Monad.Cont
import Control.Monad.State
import Data.Maybe
import Text.Printf
import qualified Data.ByteString.Char8 as B

f n m =
  flip evalStateT m . ($ return) . runContT . callCC $ \shift ->
    forM_ [0..n-10] $ \i ->
      forM_ [0..n-10-i] $ \j ->
        forM_ [0..n-10-i-j] $ \k -> do
          let l = n-10-i-j-k
          lift . lift . putStr $ "11"++replicate i '0'++
            "11"++replicate j '0'++
            "11"++replicate k '0'++
            "11"++replicate l '0'++
            "11"
          lift . lift $ putStrLn " 3 4 5 6 7 8 9 10 11"
          m <- get
          if m == 1
             then shift ()
             else put $ m-1

main = do
  cases <- readLn :: IO Int
  forM_ [1..cases] $ \cc -> do
    [n, m] <- (map (fst . fromJust . B.readInt) . B.words) <$> B.getLine
    printf "Case #%d:\n" cc
    f n m
