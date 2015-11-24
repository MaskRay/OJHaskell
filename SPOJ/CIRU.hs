import Data.Function
import Data.List
import Text.Printf

tau = pi*2

data Circle = Circle { x::Double, y::Double, r::Double }

distance a b = sqrt $ (x b-x a)^2+(y b-y a)^2

contain a b = r a >= r b + distance a b

intersects a b = r a+r b > d && d > r a-r b
  where d = distance a b

intersection a b = if t1 < 0 then (t1+tau, t2+tau) else (t1, t2)
  where alpha = atan2 (y b-y a) (x b-x a)
        theta = acos $ (r a^2+d*d-r b^2) / (2*r a*d)
        d = distance a b
        (t1, t2) = (alpha-theta, alpha+theta)

calc (c:cs, cs2) = (ret, cs')
  where
    cs' = filter (not . contain c) cs
    events = sort $
             concatMap (\d -> let (t1, t2) = intersection c d
                              in if t2 > tau
                                 then [(t1,1),(tau,-1),(0.0,1),(t2-tau,-1)]
                                 else [(t1,1),(t2,-1)]) $ filter (intersects c) $ cs2++cs'
    ret = if null events
          then pi*r c^2
          else (\ (_,x,_) -> x/2) $
               foldl (\ (l,s,st) e ->
                       let st' = st+snd e
                       in if st == 0
                          then (fst e, s + arch c l (fst e), st')
                          else (fst e, s, st')) (0,0,0) $ events ++ [(tau,0)]
    arch c bgn end = r c^2*(end-bgn-sin (end-bgn)) + (fst pbgn*snd pend-snd pbgn*fst pend)
      where pbgn = (x c+r c*cos bgn, y c+r c*sin bgn)
            pend = (x c+r c*cos end, y c+r c*sin end)

solve ([], _) = 0
solve (cs, cs2) = let (area, cs') = calc (cs, cs2)
               in area+solve (cs', head cs:cs2)

main = do
  _ <- getLine
  lines <- fmap lines getContents
  let a = map ((\ [xx,yy,rr] -> Circle {x=xx,y=yy,r=rr}) . map read . words) lines
      a' = reverse $ sortBy (compare `on` r) a
  printf "%.3f\n" $ solve (a', [])