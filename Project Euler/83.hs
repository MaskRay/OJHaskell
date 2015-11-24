import Control.Arrow
import Control.Monad.State
import Data.Array
import Data.List
import Data.Ord
import qualified Data.Set as S

data Queue = Q {visited :: S.Set (Int,Int), distance :: Array (Int,Int) Int} deriving (Show)

dirs = [id *** succ, id *** pred, succ *** id, pred *** id]

dijkstra :: Array (Int,Int) Int -> State Queue ()
dijkstra a = do
         Q vis dist <- get
         let (u, mind) = minimumBy (comparing snd) $ [(v, dist!v) | v <- range $ bounds a, not $ S.member v vis]
         put $ Q (S.insert u vis) dist
         mapM (\d -> do Q vis dist <- get
                        let v = d u
                        when (inRange (bounds a) v && mind+a!v < dist!v) $ put (Q vis (dist//[(v,mind+a!v)]))
                        return ()) dirs
         Q vis' dist' <- get
         let n = fst $ snd $ bounds $ a
         unless (S.member (n,n) vis') $ dijkstra a

prob83 a' = (!(n,n)) . distance $ execState (dijkstra a) (Q S.empty $ listArray ((1,1),(n,n)) (a!(1,1) : repeat (10^8)))
       where n = length a'
             a = listArray ((1,1),(n,n)) $ concat a'

main = do
     contents <- readFile "matrix.txt"
     print $ prob83 $ map (\x -> read ("["++x++"]") :: [Int]) $ lines contents
