{-# LANGUAGE FlexibleContexts #-}
import Control.Applicative
import Control.Monad.Identity hiding (forM_)
import Control.Monad.State.Strict hiding (forM_)
import Data.Array.Base
import Data.Array.IO
import Data.Bits
import Data.Foldable
import Data.Functor
import Data.Maybe
import qualified Data.ByteString.Char8 as B

int = fst . fromJust . B.readInt

-- | Backport

a & f = f a
infixl 1 &

-- | Lens

type ASetter s t a b = (a -> Identity b) -> s -> Identity t
_1 f (a, b, c) = (\a' -> (a', b, c)) <$> f a
_2 f (a, b, c) = (\b' -> (a, b', c)) <$> f b
_3 f (a, b, c) = (\c' -> (a, b, c')) <$> f c

(%~) :: ASetter s t a b -> (a -> b) -> s -> t
l %~ f = runIdentity . l (Identity . f)
infixr 4 %~

(.~) :: ASetter s t a b -> b -> s -> t
l .~ b = l %~ const b
infixr 4 .~

type Getting r s a = (a -> Const r a) -> s -> Const r s
(^.) :: s -> Getting a s a -> a
x ^. l = getConst $ l Const x
infixl 8 ^.

lowerBound :: (MArray a e m, Ord e) => a Int e -> Int -> Int -> e -> m Int
lowerBound a l h x = go l h
  where
    go l h | l == h = return l
           | otherwise = do
               let m = (l+h) `quot` 2
               t <- unsafeRead a m
               if t < x then go (m+1) h else go l m

{-# INLINABLE sort3ByIndex #-}
sort3ByIndex :: (MArray a e m, Ord e) => a Int e -> Int -> Int -> Int -> m ()
sort3ByIndex a i j k = do
  a0 <- unsafeRead a i
  a1 <- unsafeRead a j
  a2 <- unsafeRead a k
  case compare a0 a1 of
    GT -> case compare a0 a2 of
            GT -> case compare a2 a1 of
                    LT -> do unsafeWrite a i a2
                             unsafeWrite a k a0
                    _  -> do unsafeWrite a i a1
                             unsafeWrite a j a2
                             unsafeWrite a k a0
            _  -> do unsafeWrite a i a1
                     unsafeWrite a j a0
    _  -> case compare a1 a2 of
            GT -> case compare a0 a2 of
                    GT -> do unsafeWrite a i a2
                             unsafeWrite a j a0
                             unsafeWrite a k a1
                    _  -> do unsafeWrite a j a2
                             unsafeWrite a k a1
            _  -> return ()

{-# INLINABLE swap #-}
swap :: (MArray a Int IO) => a Int Int -> Int -> Int -> IO ()
swap a i j = do
  a0 <- unsafeRead a i
  a1 <- unsafeRead a j
  unsafeWrite a i a1
  unsafeWrite a j a0

{-# SPECIALIZE qsort :: IOUArray Int Int -> Int -> Int -> IO () #-}
qsort :: (MArray a Int IO) => a Int Int -> Int -> Int -> IO ()
qsort a l h = go l h
  where
    go l h | h-l < 2 = return ()
           | otherwise = do
             sort3ByIndex a c l (h-1)
             p <- unsafeRead a l
             mid <- partition p (l+1) h
             swap a l (mid-1)
             go l (mid-1)
             go mid h
      where
        c = (l+h) `quot` 2
    partition = up
      where
        up p l h
          | l < h = do
            e <- unsafeRead a l
            if e < p
               then up p (l+1) h
               else down p l (h-1)
          | otherwise = return l
        down p l h
          | l < h = do
            e <- unsafeRead a h
            if p < e
               then down p l (h-1)
               else swap a l h >> up p (l+1) h
          | otherwise = return l

{-# INLINABLE add #-}
add :: (MArray a e m, Num e) => a Int e -> Int -> Int -> Int -> e -> m ()
add fenwick base n' i delta = do
  let n = n' - base
      go i | i >= n = return ()
           | otherwise = do
             v <- unsafeRead fenwick (base+i)
             unsafeWrite fenwick (base+i) $ v+delta
             go $ i .|. (i+1)
  go $ i-base

{-# INLINABLE getSum #-}
getSum :: (MArray a e m, Num e) => a Int e -> Int -> Int -> m e
getSum fenwick base i = do
  let go i acc | i == 0 = return acc
               | otherwise = do
                 v <- unsafeRead fenwick (base+i-1)
                 go (i .&. (i-1)) $ acc + v
  go (i-base) 0

meow :: Int -> IOArray Int (Int,Int,Int) -> IO ()
meow n a = do
  posa <- newListArray (0, n-1) [0..n-1] :: IO (IOUArray Int Int)
  za <- newArray_ (0, n-1) :: IO (IOUArray Int Int)
  forM_ [0..n-1] $ \i -> do
    t <- unsafeRead a i
    unsafeWrite za i $ t^._3
  qsort za 0 n
  ya <- newArray_ (0, n-1) :: IO (IOUArray Int Int)
  let go i | i == n = return ()
           | otherwise = do
             t <- unsafeRead a i
             base <- lowerBound za 0 n $ t^._3
             pos <- unsafeRead posa base
             unsafeWrite posa base $ pos+1
             unsafeWrite ya pos $ t ^. _2
             unsafeWrite a i $ t & _3 .~ base
             go $ i+1
  go 0
  let go i | i == n = return ()
           | otherwise = do
             j <- unsafeRead posa i
             when (i < j) $ qsort ya i j
             go $ i+1
  go 0
  fenwick <- newArray (0, n-1) 0 :: IO (IOUArray Int Int)
  let go i | i == n = return ()
           | otherwise = do
             t <- unsafeRead a i
             let base = t^._3
             ub <- unsafeRead posa base
             j <- lowerBound ya base ub (t^._2)
             case t^._1 of
                  1 ->
                    add fenwick base ub j 1
                  2 ->
                    add fenwick base ub j (-1)
                  3 ->
                    getSum fenwick base (j+1) >>= print
             go $ i+1

  go 0

main = do
  n <- readLn
  a <- replicateM n (((\[x,y,z] -> (x,y,z)) . map int . B.words) <$> B.getLine) >>= newListArray (0, n-1) :: IO (IOArray Int (Int,Int,Int))
  meow n a
