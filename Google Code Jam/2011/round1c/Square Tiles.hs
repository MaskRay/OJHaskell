import Control.Monad
import Control.Monad.State
import Data.Array.IArray

type Picture = Array (Int,Int) Char

calc :: Int -> Int -> State Picture Bool
calc r c = do
  a <- get
  let (maxRow,maxCol) = snd $ bounds a
      indices = [(r,c), (r,c+1), (r+1,c), (r+1,c+1)]
  if c > maxCol
    then calc (r+1) 0
    else if r > maxRow
         then return True
         else if a!(r,c) /= '#'
              then calc r (c+1)
              else if r < maxRow && c < maxCol &&
                      all ((=='#') . (a!)) indices
                   then put (a // zip indices "/\\\\/") >> calc r (c+1)
                   else return False

main = do
  cases <- liftM read getLine
  forM_ [1..cases] $ \cc -> do
    [r, c] <- liftM (map read . words) getLine
    lines <- forM [0..r-1] $ const getLine
    let a = listArray ((0,0),(r-1,c-1)) $ concat lines :: Picture
    putStrLn $ "Case #" ++ show cc ++ ":"
    let (res, a') = runState (calc 0 0) a
    if res
      then forM_ [0..r-1] $ \i -> forM_ [0..c-1] (putChar.(a'!).(,)i) >> putStrLn ""
      else putStrLn "Impossible"