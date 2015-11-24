import Control.Arrow
import Control.Monad
import Control.Monad.ST
import Data.Array
import Data.Array.ST
import Data.List
import Data.Ord
import Data.STRef
import qualified Data.Set as S

data Queue = Q {visited :: S.Set (Int,Int), distance :: Array (Int,Int) Int} deriving (Show)

dirs = [id *** succ, id *** pred, succ *** id, pred *** id]

while :: (Monad m) => m Bool -> m () -> m ()
while predicate action = do
      p <- predicate
      when p $ action >> while predicate action

dijkstra a = runST $ do
         let n = fst $ snd $ bounds a
         flag <- newArray ((1,1),(n,n)) False :: ST s (STUArray s (Int,Int) Bool)
         dist <- newArray ((1,1),(n,n)) (10^8) :: ST s (STUArray s (Int,Int) Int)
         writeArray dist (1,1) $ a ! (1,1)

         while ((==10^8) `liftM` readArray dist (n,n)) $ do
               u <- newSTRef (1,1)
               mind <- newSTRef (10^8)
               mapM_ (\i -> do
                     mind' <- readSTRef mind
                     d <- readArray dist i
                     visited <- readArray flag i
                     when (not visited && d < mind') $ writeSTRef mind d >> writeSTRef u i
                     ) $ range ((1,1),(n,n))
               u' <- readSTRef u
               mind' <- readSTRef mind
               writeArray flag u' True
               mapM_ (\d -> do
                     let v = d u'
                     when (inRange ((1,1),(n,n)) v) $ do
                          old <- readArray dist v
                          when (mind'+a!v < old) $ writeArray dist v (mind'+a!v)
                     ) dirs

         readArray dist (n,n)

prob83 a' = dijkstra a
       where n = length a'
             a = listArray ((1,1),(n,n)) $ concat a'

main = do
     contents <- readFile "matrix.txt"
     print $ prob83 $ map (\x -> read ("["++x++"]") :: [Int]) $ lines contents
