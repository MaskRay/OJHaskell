{-# LANGUAGE FlexibleInstances, FunctionalDependencies, MultiParamTypeClasses #-}
import Control.Applicative
import Control.Monad
import qualified Data.ByteString.Char8 as B
import Data.Char
import Data.List
import qualified Data.Map as M
import Data.Maybe

data T = T !Int !Int deriving (Show)

parse = do
  n <- readI
  _ <- readI
  replicateM (n-1) $ liftM2 (,) readI readI
  where
    readI = state $ fromJust . B.readInt . B.dropWhile isSpace

inf = maxBound `div` 2 :: Int

go :: Int -> Int -> M.Map Int [Int] -> T
go u p g = T (v1+1) v0
  where
    children = M.findWithDefault [] u g \\ [p]
    f (T a b) ch = T (maximum [a+c,b+d,-inf]) (maximum [a+d,b+c,-inf])
      where
        T c d = go ch u g
    T v0 v1 = foldl' f (T 0 (-inf)) children

main = do
  es <- evalState parse <$> B.getContents
  let g = foldl' (\g (u,v) -> M.insertWith (++) u [v] . M.insertWith (++) v [u] $ g) M.empty es
      T res res1 = go 1 0 g
  print (res-1)

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
