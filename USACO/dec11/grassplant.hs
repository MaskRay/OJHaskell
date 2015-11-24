{-# LANGUAGE BangPatterns #-}
import Control.Applicative
import Control.Arrow
import Control.Monad
import Control.Monad.State
import Data.Array
import Data.Array.IO
import Data.Bits
import Data.Bits.Extras
import qualified Data.ByteString.Char8 as B
import Data.Char
import qualified Data.Map as M
import Data.Maybe
import Data.Tuple

type Vertex = Int
type Edge = (Vertex,Vertex)
type Graph = M.Map Int [Int]

dfs :: Graph -> ([Int],[Vertex])
dfs g = go 0 (-1) 1 ([],[])
  where
    go !d !p !u = (((d:)***(u:)).) . foldl (\f g -> g.((d:)***(u:)).f) id . map (go (d+1) u) . filter (/=p) . fromJust $ M.lookup u g

buildRMQ :: Int -> [Int] -> [Vertex] -> Array (Int,Int) (Int,Vertex)
buildRMQ n deps vs =
  let nn = n*2-1
      maxK = floor $ logBase 2 (fromIntegral nn)
      ret = array ((0,0),(maxK,nn-1)) $ [ ((0,i),(x,y)) | (i,x,y) <- zip3 [0..] deps vs ] ++
           [ ((k,i),min (ret!(k-1,i)) (ret!(k-1,j)))
           | k <- [1..maxK], i <- [0..nn-(1`shiftL`k)]
           , let j = i + 1 `shiftL` (k-1)
           ]
  in ret

getLCA :: Array (Int,Int) (Int,Vertex) -> Array Vertex Int -> Vertex -> Vertex -> Vertex
getLCA !t !pre !u' !v' =
  let !uu = pre!u'
      !vv = pre!v'
      !u = min uu vv
      !v = max uu vv
      !k = bitSize (undefined::Int)-1-fromIntegral(leadingZeros (abs (v-u)+1))
  in snd $ min (t!(k,u)) (t!(k,v-1`shiftL`k+1))

getSum :: (Ix i, Bits i, Num e, MArray a e m) => a i e -> i -> m e
getSum !fenwick !x = sum `liftM` mapM (readArray fenwick . subtract 1) (takeWhile (>0) $ iterate (\i -> i .&. (i-1)) x)

add :: (Ix i, Bits i, Num e, MArray a e m) => a i e -> i -> e -> m ()
add !fenwick !x !delta = do
  !bnds <- getBounds fenwick
  forM_ (takeWhile (inRange bnds) $ iterate (\i -> i .|. (i+1)) x) $ \i -> do
    fi <- readArray fenwick i
    writeArray fenwick i (fi + delta)

parseOp = do
  n <- readInt
  m <- readInt
  !es <- replicateM (n-1) $ do
    !u <- readInt
    !v <- readInt
    return (u,v)
  !ops <- replicateM m $ do
    !op <- readString
    !a <- readInt
    !b <- readInt
    return (head $ B.unpack op, a, b)
  return (n, m, es, ops)
  where
    readInt = state $ fromJust . B.readInt . B.dropWhile isSpace
    readInteger = state $ fromJust . B.readInteger . B.dropWhile isSpace
    readString = state $ B.span (not . isSpace) . B.dropWhile isSpace
    readLine = state $ B.span (not . isEoln) . B.dropWhile isEoln
    isEoln ch = ch == '\r' || ch == '\n'
    
main = do
  (n,m,es,ops) <- evalState parseOp <$> B.getContents
  let g = M.fromListWith (++) . map (second (:[])) $ es++map swap es
  let (deps,vs) = dfs g
  let dep = array (1,n) $ zip vs deps
  let pre = array (1,n) . reverse $ zip vs [0..] :: Array Vertex Int
  let post = array (1,n) $ zip vs [0..]
  let t = buildRMQ n deps vs

  fenwick <- newArray (0,2*n-2) 0 :: IO (IOArray Int Int)
  forM_ ops $ \ !(op,a,b) ->
    if op == 'P'
    then add fenwick (pre!a) 1 >> add fenwick (pre!b) 1 >> add fenwick (pre!getLCA t pre a b) (-2)
    else let v = if dep!b > dep!a then b else a
         in liftM2 (-) (getSum fenwick (post!v+1)) (getSum fenwick (pre!v)) >>= print