{-# LANGUAGE BangPatterns #-}
import Control.Applicative
import Control.Arrow
import Control.Monad
import Control.Monad.ST
import Data.Array.ST
import Data.Function
import Data.List
import Data.Maybe
import Data.Int
import Data.Ord

modulus = 1000000007 :: Int64
  
type UnionFind s = STUArray s Int Int

newUF :: (Int,Int) -> ST s (UnionFind s)
newUF !bnds = newArray bnds (-1)
  
findUF :: UnionFind s -> Int -> ST s Int
findUF !uf !x = do
  !fx <- readArray uf x
  if fx == -1
    then return x
    else findUF uf fx >>= \r -> writeArray uf x r >> return r

mergeUF :: UnionFind s -> Int -> Int -> ST s Bool
mergeUF !uf !x !y = do
  !fx <- findUF uf x
  !fy <- findUF uf y
  if fx == fy
    then return False
    else writeArray uf fx fy >> return True

pairFindUF :: UnionFind s -> Int -> Int -> ST s (Maybe (Int,Int))
pairFindUF !uf !x !y = do
  !fx <- findUF uf x
  !fy <- findUF uf y
  if fx == fy
    then return Nothing
    else return . Just $ if fx < fy then (fx,fy) else (fy,fx)

getU !(u,v,w) = u
getV !(u,v,w) = v
getW !(u,v,w) = w

solve :: Int -> Int -> [(Int,Int,Int)] -> ST s (Int64,Int64)
solve n m es = do
  uf <- newUF (1,n)
  foldr (\(f,g) (a,b) -> (f a,g b)) (0,1) <$> (forM (groupBy ((==) `on` getW) $ sortBy (comparing getW) es) $ \es -> do
    !valids <- catMaybes <$> zipWithM (pairFindUF uf) (map getU es) (map getV es)
    let !diffs = length . nub $ sort valids
    !merges <- genericLength <$> filterM (uncurry $ mergeUF uf) (zip (map getU es) (map getV es))
    let !mstCnt = case length valids of
          3 -> if merges == 1 || merges == 2 && diffs == 3
               then (*3)
               else if merges == 2 && diffs == 2 then (*2)
                    else id
          2 -> if merges == 1 then (*2) else id
          _ -> id
    return ((+) $ merges * fromIntegral (getW (head es)), (`rem`modulus) . mstCnt))

main = do
  [n,m] <- (map read . words) <$> getLine
  es <- replicateM m $ ((\[u,v,w] -> (u,v,w)) . map read . words) <$> getLine :: IO [(Int,Int,Int)]
  putStrLn $ (\(cost,cnt) -> shows cost.(' ':).shows cnt $ "") $ runST $ solve n m es
