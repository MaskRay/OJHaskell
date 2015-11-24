import Data.Array.Base
import Data.Array.IO
import Data.Function
import Data.List
import Data.Monoid
import Text.Printf
import System.IO.Unsafe

tau = pi*2

data Circle = Circle { x::Double, y::Double, r::Double }

distance a b = sqrt $ (x b-x a)^2+(y b-y a)^2

contain a b = r a >= r b + distance a b

intersects a b = r a+r b > d && d > abs (r a-r b)
  where d = distance a b

intersection a b = if r a+r b > d && d > abs (r a-r b)
                   then
                     if end > tau
                     then [(bgn,1),(tau,-1),(0.0,1),(end-tau,-1)]
                     else [(bgn,1),(end,-1)]
                   else []
  where alpha = atan2 (y b-y a) (x b-x a)
        theta = acos $ (r a^2+d*d-r b^2) / (2*r a*d)
        d = distance a b
        (t1, t2) = (alpha-theta, alpha+theta)
        (bgn, end) = if t1 < 0 then (t1+tau, t2+tau) else (t1, t2)
        
solve ([], _) res = return ()
solve (c:cs, cs2) res = do
  go 0.0 (succ . length $ filter (\d -> contain d c) cs2) (events ++ [(tau, 0)])
  solve (cs, c:cs2) res
  where
    events = sort $ concatMap (intersection c) $ cs2++cs
    arch c bgn end = (/2) $ r c^2*(end-bgn-sin (end-bgn)) + (fst pbgn*snd pend-snd pbgn*fst pend)
      where pbgn = (x c+r c*cos bgn, y c+r c*sin bgn)
            pend = (x c+r c*cos end, y c+r c*sin end)
    go _ _ [] = return ()
    go l st (e:es) = do
      old <- unsafeRead res st
      unsafeWrite res st (old + arch c l (fst e))
      go (fst e) (st + snd e) es

main = do
  _ <- getLine
  lines <- fmap lines getContents
  let a = map ((\ [xx,yy,rr] -> Circle {x=xx,y=yy,r=rr}) . map read . words) lines
      a' = reverse $ sortBy (compare `on` r) a
      n = length a'
  res <- newArray (0,n+1) 0 :: IO (IOArray Int Double)
  solve (a', []) res
  mapM_ (\i -> do
            x <- unsafeRead res i
            y <- unsafeRead res (i+1)
            printf "[%d] = %.3f\n" i (x-y)) [1..n]