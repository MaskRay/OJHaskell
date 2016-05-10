{-# LANGUAGE FlexibleContexts, ScopedTypeVariables #-}
import Control.Monad
import Data.Array.IArray
import Data.Foldable
import Data.Functor
import Data.List hiding (foldl')
import Data.Maybe
import qualified Data.Set as S
import qualified Data.ByteString.Char8 as B

int = fst . fromJust . B.readInt
ints = map int . B.words

has :: (Integral i, Ix i, Ord t, IArray a (t, t)) => a i (t, t) -> (t, t) -> Bool
has es (u, v) = not $ i < h && es!i == x
  where
    x = if u < v then (u, v) else (v, u)
    go l h =
      if l == h
      then l
      else
        let m = (l+h) `div` 2 in
        if es!m < x
        then go (m+1) h
        else go l m
    (l, h) = succ <$> bounds es
    i = go l h

dfs es un u =
  foldl' (dfs es) (S.difference un vs) vs
  where
    vs = S.filter (\v -> has es (u, v)) un

main = do
  [n, m, k] <- ints <$> B.getLine
  es :: Array Int (Int,Int) <- ((listArray (0, m-1) . sort) <$>) . replicateM m $ do
    [u, v] <- ints <$> B.getLine
    return $ if u < v then (u-1, v-1) else (v-1, u-1)
  let deg0 = foldl' (\c (u,v) -> c + fromEnum (u == 0 || v == 0)) 0 es
  let (un, ncomp) = foldl' (\(un, ncomp) u ->
       if has es (0,u) && S.member u un
       then (dfs es un u, ncomp+1)
       else (un, ncomp)
       ) (S.fromList [1..n-1], 0) [1..n-1]
  if S.null un && ncomp <= k && k <= n-1-deg0
     then putStrLn "possible"
     else putStrLn "impossible"
