{-# LANGUAGE ForeignFunctionInterface, MultiWayIf, OverloadedStrings, Safe #-}
import Control.Arrow
import Control.Monad.State
import Data.Char
import Data.Functor
import Data.Maybe
import Foreign.C.String
import Foreign.C.Types
import Text.Printf
import qualified Data.ByteString.Char8 as B

int = state $ fromJust . B.readInt . B.dropWhile isSpace

double :: State B.ByteString Double
double = state $ first (f 0 0 Nothing) . B.span (not . isSpace) . B.dropWhile isSpace
  where
    f i acc dot s =
      if | i == B.length s -> maybe acc ((acc/) . (10^)) dot
         | B.index s i == '-' -> f (i+1) (-acc) (Just 0) s
         | B.index s i == '.' -> f (i+1) acc (Just 0) s
         | otherwise -> f (i+1) (acc*10 + fromIntegral (fromEnum $ B.index s i) - 48) ((1+) <$> dot) s

parse = do
  n <- int
  maxs <- replicateM n double
  mins <- replicateM n double
  return (n, maxs, mins)

foreign import ccall "stdio.h printf" c_printf :: CString -> Double -> IO ()
foreign import ccall "stdio.h putchar" c_putchar :: Int -> IO ()

writeDouble x = withCString "%.9f" $ flip c_printf x

writeChar :: Char -> IO ()
writeChar = c_putchar . fromEnum

f [] [] _ _ _ _ = []
f (mx:maxs) (mi:mins) mxs mis xs ys = (xs'-xs, ys'-ys) : f maxs mins mxs' mis' xs' ys'
  where
    mxs' = mxs + mx
    mis' = mis + mi
    b = mxs'+mis'
    d = sqrt . max 0 $ b^2 - 4 * mxs'
    xs' = (b+d)/2
    ys' = (b-d)/2

main = do
  (n, maxs, mins) <- evalState parse <$> B.getContents
  let (xs, ys) = unzip $ f maxs mins 0 0 0 0
  forM_ xs ((>> writeChar ' ') . writeDouble)
  writeChar '\n'
  forM_ xs ((>> writeChar ' ') . writeDouble)
  writeChar '\n'
