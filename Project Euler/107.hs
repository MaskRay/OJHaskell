import Control.Monad
import Control.Monad.ST
import Data.Array.ST
import Data.STRef
import Text.Regex

makeLine s = map f (splitRegex (mkRegex ",") s)
         where f x
                 | '-' `elem` x = 0
                 | otherwise = read x :: Int

prim :: [[Int]] -> Int
prim a' = runST $ do
     let n = length a'
     a <- newListArray ((0,0),(n-1,n-1)) (concat a') :: ST s (STUArray s (Int,Int) Int)
     flag <- newArray (0,n-1) False :: ST s (STUArray s Int Bool)
     dist <- newListArray (0,n-1) (0:repeat (10^8)) :: ST s (STUArray s Int Int)
     res <- newSTRef 0
     mapM_ (\_ -> do
           u <- newSTRef 0
           w <- newSTRef (10^8)
           mapM_ (\i -> do
                 w' <- readSTRef w
                 ww <- readArray dist i
                 flag' <- readArray flag i
                 when (not flag' && ww < w') $ writeSTRef w ww >> writeSTRef u i
                 ) [0..n-1]
           u' <- readSTRef u
           w' <- readSTRef w
           modifySTRef res (+w')
           writeArray flag u' True
           mapM (\v -> do
                old <- readArray dist v
                edge <- readArray a (u',v)
                when (edge /= 0 && edge < old) $ writeArray dist v edge
               ) [0..n-1]
          ) [0..n-1]
     readSTRef res

main = do
     content <- readFile "network.txt"
     let a = map makeLine $ lines content
         oldCost = (`div` 2) $ sum $ fmap sum a
         newCost = prim a
     print $ oldCost-newCost
