{-# LANGUAGE BangPatterns, FlexibleInstances, FunctionalDependencies, MultiParamTypeClasses, OverloadedStrings, TupleSections #-}
{-# OPTIONS_GHC -O2 #-}

import Control.Applicative
import Control.Monad
import Data.Char
import Data.Array.Base
import Data.Array.Unboxed
import Data.Array.IO
import Data.Int
import Data.List
import Data.Maybe
import qualified Data.ByteString.Char8 as B

data Op = Add Int | Del Int | Sum

parse = do
  n <- readInt
  (n,) <$> replicateM n readOp
  where
    readInt = state $ fromJust . B.readInt . B.dropWhile isSpace
    readStr = state $ B.span (not . isSpace) . B.dropWhile isSpace
    readOp = do
      op <- readStr
      case op of
        "add" -> Add <$> readInt
        "del" -> Del <$> readInt
        "sum" -> return Sum

lowerBound a x = go 0 (snd $ bounds a)
  where
    go !l !r | l == r = l
             | True = let !m = (l+r) `div` 2
                      in if a!m < x then go (m+1) r else go l m

treeInsert :: IOUArray (Int,Int) Int64 -> UArray Int Int -> Int -> Int -> Int -> Int -> Int -> IO ()
treeInsert tree da !root !l !r !x !d = do
  s <- unsafeRead tree (root*6+5)
  let m = (l+r) `div` 2
  unsafeWrite tree (root*6+5) (s + fromIntegral d)
  if l == r-1
    then
      unsafeWrite tree (root*6) . fromIntegral $ (d+1) `div` 2 * da!l
    else do
      if x < m then treeInsert tree da (root*2+1) l m x d else treeInsert tree da (root*2+2) m r x d
      lsum <- unsafeRead tree ((root*2+1)*6+5)
      forM_ [0..4] $ \i -> do
        la <- unsafeRead tree ((root*2+1)*6+i)
        ra <- unsafeRead tree ((root*2+2)*6+(i-fromIntegral lsum)`mod`5)
        unsafeWrite tree (root*6+i) (la+ra)

main = do
  (n,ops) <- evalState parse `fmap` B.getContents
  let discrete = map head . group . sort $ [x | Add x <- ops] ++ [x | Del x <- ops]
      m = length discrete
      da = listArray (0,m-1) discrete
  tree <- newArray ((0,0),(min (4*m) 262144 - 1,5)) 0 :: IO (IOUArray (Int,Int) Int64)
  forM_ ops $ \op ->
    case op of
      Add x -> treeInsert tree da 0 0 m (lowerBound da x) 1
      Del x -> treeInsert tree da 0 0 m (lowerBound da x) (-1)
      Sum -> unsafeRead tree 2 >>= print


class (Monad m) => MonadState s m | m -> s where
  get :: m s
  put :: s -> m ()

modify :: (MonadState s m) => (s -> s) -> m ()
modify f = do
	s <- get
	put (f s)

gets :: (MonadState s m) => (s -> a) -> m a
gets f = do
	s <- get
	return (f s)

newtype State s a = State { runState :: s -> (a, s) }

instance Functor (State s) where
  fmap f m = State $ \s -> let (a, s') = runState m s in (f a, s')

instance Applicative (State s) where
  pure = return
  (<*>) = ap

instance Monad (State s) where
  return a = State $ \s -> (a, s)
  m >>= k  = State $ \s -> let (a, s') = runState m s in runState (k a) s'

instance MonadState s (State s) where
  get = State $ \s -> (s, s)
  put s = State $ \_ -> ((), s)

evalState :: State s a -> s -> a
evalState m s = fst (runState m s)

state = State
