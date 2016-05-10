{-# LANGUAGE TupleSections #-}
import Control.Applicative
import Control.Monad.State
import Data.Array.IO
import Data.Char
import Data.Functor
import Data.Maybe
import qualified Data.ByteString.Char8 as B

data Op = Top !Int | Pop !Int | Push !Int !Int | Merge !Int !Int deriving (Show)

parse = do
  n <- int
  q <- int
  (n,q,) <$> replicateM q readOp
  where
    int = state $ fromJust . B.readInt . B.dropWhile isSpace
    readOp = do
      op <- int
      case op of
        1 -> Top <$> int
        2 -> Pop <$> int
        3 -> Push <$> int <*> int
        4 -> Merge <$> int <*> int

data PairingHeap a = Empty | Node !a ![PairingHeap a] ![PairingHeap a] deriving (Show)
merge l Empty = l
merge Empty r = r
merge l@(Node x ls lc) r@(Node y rs rc)
  | x > y = Node x (r:ls) lc
  | otherwise = Node y (l:rs) rc

mergeList [] = Empty
mergeList [x] = x
mergeList (x:y:xs) = merge (merge x y) (mergeList xs)

deleteMax (Node _ s c)= mergeList $ s ++ c

findMax (Node x _ _) = x

insert y = merge (Node y [] [])

main = do
  (n, q, ops) <- evalState parse <$> B.getContents
  a <- newArray (1,n) Empty :: IO (IOArray Int (PairingHeap Int))
  forM_ ops $ \op ->
    case op of
      Top i -> readArray a i >>= print . findMax
      Pop i -> readArray a i >>= writeArray a i . deleteMax
      Push i x -> readArray a i >>= writeArray a i . insert x
      Merge i j -> do
        hi <- readArray a i
        hj <- readArray a j
        writeArray a i (merge hi hj)
