import System.IO
import Data.Array.IO
import Data.Char
import Data.Array.Base (unsafeRead, unsafeWrite)
import qualified Data.ByteString.Lazy.Char8 as B

main = do
     a <- newArray (0,1000000) 0 :: IO (IOArray Int Int)
     hSetBuffering stdin $ BlockBuffering Nothing
     hSetBuffering stdout $ BlockBuffering Nothing
     ss <- B.getContents
     mapM_ (\x -> do
                        let idx = readInt x
                        d <- unsafeRead a idx
                        unsafeWrite a idx (d+1))
           (tail . B.lines $ ss)
     mapM_ (\x -> do
                        d <- unsafeRead a x
                        B.putStr $ B.concat $ replicate d $ B.pack (show x ++ "\n")
               ) [0..1000000]

readInt :: B.ByteString -> Int
readInt = B.foldl' (\x c -> 10 * x + ord c - 48) 0
