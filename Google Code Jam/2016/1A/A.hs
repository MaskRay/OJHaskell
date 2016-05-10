import Control.Monad
import Text.Printf
import qualified Data.ByteString.Char8 as B

main = do
  n <- readLn :: IO Int
  forM_ [1..n] $ \cc -> do
    a <- B.getLine
    printf "Case #%d: " cc
    B.putStrLn $ B.foldl (\s c -> if B.null s || c >= B.head s then B.cons c s else B.snoc s c) B.empty a
