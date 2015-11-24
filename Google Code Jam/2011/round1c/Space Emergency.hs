import Control.Monad
import Data.List

main = do
  cases <- liftM read getLine
  forM_ [1..cases] $ \cc -> do
    l:t:n:c:a <- liftM (map read . words) getLine
    let dist = take n . concat $ repeat a
        (res, tt, idx) = foldl (\x@(res,tt,idx) (i,d) ->
                             if idx /= Nothing then x
                             else if 2*d > tt then (res,tt,Just i) else (res+2*d,tt-2*d,Nothing)
                              ) (0,t,Nothing) $ zip [0..] dist
    putStr $ "Case #" ++ show cc ++ ": "
    case idx of
      Nothing -> print res
      Just idx -> let dist' = reverse . sort $ (dist!!idx - div tt 2) : drop (idx+1) dist
                  in (print $ res + tt + sum (take l dist') + sum (drop l dist') * 2)
