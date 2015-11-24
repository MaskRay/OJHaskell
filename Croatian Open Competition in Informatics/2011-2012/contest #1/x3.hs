{-# LANGUAGE ScopedTypeVariables #-}
import Control.Applicative
import Control.Monad
import Control.Monad.State
import Data.Array.Base
import Data.Array.IO
import Data.Bits
import Data.Char
import Data.Int
import Data.Maybe
import qualified Data.ByteString.Char8 as B

k = 20

parse = do
  n <- readInt
  xs <- replicateM n readInt
  return (n,xs)
  where
    readInt = state $ fromJust . B.readInt . B.dropWhile isSpace

main = do
  (n,xs) <- evalState parse <$> B.getContents
  a <- newArray (0,k-1) 0 :: IO (IOUArray Int Int)
  forM_ xs $ \x ->
    forM_ [0..k-1] $ \i ->
      when ((x `unsafeShiftR` i) .&. 1 == 1) $ do
        o <- unsafeRead a i
        unsafeWrite a i (o+1)
  vs :: [Int64] <- forM [0..k-1] $ \i -> do
    let x = fromIntegral
    o <- unsafeRead a i
    return $ x o * x (n-o) * x (1 `unsafeShiftL` i)
  print $ sum vs
