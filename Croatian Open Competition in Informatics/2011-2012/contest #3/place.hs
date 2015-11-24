{-# LANGUAGE BangPatterns, OverloadedStrings #-}
import Control.Applicative
import Control.Monad
import Control.Monad.State
import Control.Monad.ST
import Data.Array
import Data.Array.IO
import Data.Bits
import Data.Char
import Data.Int
import Data.List
import qualified Data.Map as M
import Data.Maybe
import qualified Data.ByteString.Char8 as B

data Op = Modify !Int !Int | Query !Int
type Vertex = Int
type Graph = M.Map Vertex [Vertex]

dfs :: Graph -> IOUArray Int Int -> IOUArray Int Int -> IO ()
dfs g bgn end = evalStateT (go 1) 0
  where
    go !u = do
      !v <- get
      put $ v+1
      lift $ writeArray bgn u v
      mapM_ go (fromMaybe [] $ M.lookup u g)
      get >>= lift . writeArray end u

add :: (Ix i, Bits i, Num e, MArray a e m) => a i e -> i -> e -> m ()
add !fenwick !x !delta = getBounds fenwick >>= \ !bnds -> forM_ (takeWhile (inRange bnds) (iterate (\ !i -> i .|. (i+1)) x)) $ \ !i -> readArray fenwick i >>= \ !fi -> writeArray fenwick i (fi+delta)

getSum :: (Ix i, Bits i, Num e, MArray a e m) => a i e -> i -> m e
getSum !fenwick !x = sum `liftM` mapM (readArray fenwick . (subtract 1)) (takeWhile (>0) $ iterate (\ !i -> i .&. (i-1)) x)

parse = do
  n <- readInt
  m <- readInt
  wage1 <- readInt
  (wages, superiors) <- unzip <$> replicateM (n-1) (liftM2 (,) readInt readInt)
  ops <- replicateM m readOp
  let g = foldl' (\g (v,sup) -> M.insertWith' (++) sup [v] g) M.empty $ zip [2..] superiors
  return (n, listArray (1,n) (wage1:wages), g, ops)
  where
    readInt = state $ fromJust . B.readInt . B.dropWhile isSpace
    readWord = state $ B.span (not.isSpace) . B.dropWhile isSpace
    readOp = do
      op <- readWord
      if op == "p"
         then Modify <$> readInt <*> readInt
         else Query <$> readInt

main = do
  (n, wages, g, ops) <- evalState parse <$> B.getContents
  bgn <- newArray_ (1,n) :: IO (IOUArray Int Int)
  end <- newArray_ (1,n) :: IO (IOUArray Int Int)
  fenwick <- newArray (0,n) 0 :: IO (IOUArray Int Int)
  dfs g bgn end
  forM_ ops $ \op -> case op of
    Query v -> do
      b <- readArray bgn v
      getSum fenwick (b+1) >>= print . (+(wages!v))
    Modify v x -> do
      b <- readArray bgn v
      e <- readArray end v
      add fenwick (b+1) x
      add fenwick e (-x)
