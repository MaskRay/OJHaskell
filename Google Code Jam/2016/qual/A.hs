{-# LANGUAGE MultiWayIf #-}
import Control.Monad
import Data.List
import Data.Maybe
import Text.Printf
import qualified Data.ByteString.Char8 as B
import qualified Data.Set as S

int = (fst . fromJust . B.readInt) <$> B.getLine

f n s m
  | S.size m' == 10 = s
  | otherwise = f n (s+n) m'
  where
    m' = foldr S.insert m $ unfoldr (\x ->
      if | x < 0 -> Nothing
         | x < 10 -> Just (x, -1)
         | otherwise -> Just (x `mod` 10, x `div` 10)) s

main = do
  n <- int
  forM_ [1..n] $ \cc -> do
    x <- int
    printf "Case #%d: " cc
    if x == 0
       then putStrLn "INSOMNIA"
       else print $ f x x S.empty
