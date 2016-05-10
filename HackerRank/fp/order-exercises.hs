{-# LANGUAGE MultiWayIf, RecordWildCards #-}
import Control.Monad
import Data.Array.IArray
import Data.Functor
import Data.Maybe
import Data.Monoid
import qualified Data.ByteString.Char8 as B

ints = map (fst . fromJust . B.readInt) . B.words <$> B.getLine

-- = Pairing heap

data PairingHeap a = Empty | Node !a ![PairingHeap a] ![PairingHeap a] deriving (Show)

merge l Empty = l
merge Empty r = r
merge l@(Node x ls lc) r@(Node y rs rc)
  | x < y = Node x (r:ls) lc
  | otherwise = Node y (l:rs) rc

mergeList [] = Empty
mergeList [x] = x
mergeList (x:y:xs) = merge (merge x y) (mergeList xs)

-- = Subarray

data Sub = Sub {-# UNPACK #-} !Int !Int !Int deriving (Eq, Show)

value (Sub _ _ v) = v

instance Monoid Sub where
  mempty = Sub 0 0 0
  mappend (Sub l _ x) (Sub _ r y) = Sub l r (x+y)

instance Ord Sub where
  Sub l0 r0 v0 `compare` Sub l1 r1 v1 = compare v1 v0 <> compare l0 l1 <> compare r0 r1

-- = Segment tree

data Segment = Segment {left, right :: !Int, whole, prefix, postfix, infi :: !Sub, lseg, rseg :: Segment}

isNull Segment{whole = Sub l r v} = l == r

instance Show Segment where
  showsPrec d Segment{..}
    | left >= right = id
    | otherwise =
      (replicate (2*d) ' ' ++) .
      shows left . (',':) . shows right .
      (" whole "++) . shows (value whole) .
      (" prefix "++) . shows (value prefix) .
      (" postfix "++) . shows (value postfix) .
      (" infi "++) . shows (value infi) .
      ('\n':) .
      (if left < right-1
       then showsPrec (d+1) lseg .  showsPrec (d+1) rseg
       else id)

instance Monoid Segment where
  mempty = Segment{left = 0, right = 0, whole = mempty, prefix = mempty, infi = mempty, postfix = mempty, lseg = undefined, rseg = undefined}
  mappend x y = Segment{left = left x, right = right y,
                        whole = if | isNull x -> whole y
                                   | isNull y -> whole x
                                   | otherwise -> whole x <> whole y,
                        prefix = if | isNull x -> prefix y
                                    | isNull y -> prefix x
                                    | otherwise -> min (prefix x) (whole x <> prefix y),
                        postfix = if | isNull y -> postfix x
                                     | isNull x -> postfix y
                                     | otherwise -> min (postfix y) (postfix x <> whole y),
                        infi = if | isNull x -> infi y
                                  | isNull y -> infi x
                                  | otherwise -> min (min (infi x) (infi y)) (postfix x <> prefix y),
                        lseg = undefined,
                        rseg = undefined
                       }

buildSegment :: Array Int Int -> Int -> Int -> Segment
buildSegment a l r
  | l == r-1 = let x = Sub l r (a!l) in
               Segment{left = l, right = r, whole = x, prefix = x, postfix = x, infi = x, lseg = undefined, rseg = undefined}
  | otherwise = (lseg <> rseg) {lseg = lseg, rseg = rseg}
  where
    m = (l+r) `div` 2
    lseg = buildSegment a l m
    rseg = buildSegment a m r

querySegment :: Segment -> Int -> Int -> Segment
querySegment seg@Segment{..} l r
  | l <= left && right <= r = seg
  | r <= m = lq
  | m <= l = rq
  | otherwise = lq <> rq
  where
    m = (left+right) `div` 2
    lq = querySegment lseg l r
    rq = querySegment rseg l r

-- | Slice with max subarray

data Slice = Slice {-# UNPACK #-} !Int !Int !Sub deriving (Eq, Show)

instance Ord Slice where
  Slice _ _ x `compare` Slice _ _ y = compare x y

singleton :: Segment -> Int -> Int -> PairingHeap Slice
singleton seg l r = Node (Slice l r . infi $ querySegment seg l r) [] []

main = do
  [n, k] <- ints
  a <- listArray (0, n-1) <$> ints
  let seg = buildSegment a 0 n
      f 0 _ = return ()
      f k Empty = return ()
      f k g@(Node (Slice l r (Sub ll rr v)) sib ch)
        | v <= 0 = return ()
        | otherwise = do
          print v
          f (k-1) . (if rr < r then merge $ singleton seg rr r else id) . (if l < ll then merge $ singleton seg l ll else id) . mergeList $ sib ++ ch
  f k $ singleton seg 0 n
