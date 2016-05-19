import Control.Monad
import Data.Bits
import Data.IORef
import Data.List
import Data.Functor
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV
import Text.Printf

calc :: Int -> V.Vector Int -> IO Int
calc n a = do
  s <- MV.new $ (n+1)^2
  forM_ [0..n] $ \i ->
    forM_ [0..n] $ \j -> do
      MV.unsafeWrite s ((n+1)*i+j) 0
      when (i > 0) $ do
        t <- MV.unsafeRead s ((n+1)*(i-1)+j)
        MV.unsafeModify s (+t) ((n+1)*i+j)
      when (j > 0) $ do
        t <- MV.unsafeRead s ((n+1)*i+j-1)
        MV.unsafeModify s (+t) ((n+1)*i+j)
      when (i > 0 && j > 0) $ do
        t0 <- MV.unsafeRead s ((n+1)*(i-1)+j-1)
        let t1 = fromEnum $ elem (a V.! (n*(i-1)+j-1)) [0,1,4,6,8,9]
        MV.unsafeModify s (+(t1-t0)) ((n+1)*i+j)

  --V.freeze s >>= print
  nim <- MV.new $ (n+1)^4
  mex <- MV.replicate ((n+1)*4) 0
  cookie <- newIORef 0 :: IO (IORef Int)
  let
    f i j ii jj = (((n+1)*i+j)*(n+1)+ii)*(n+1)+jj
    g i j ii jj = do
      t0 <- MV.unsafeRead s ((n+1)*ii+jj)
      t1 <- MV.unsafeRead s ((n+1)*i+jj)
      t2 <- MV.unsafeRead s ((n+1)*ii+j)
      t3 <- MV.unsafeRead s ((n+1)*i+j)
      return $ t0-t1-t2+t3

  forM_ [n-1,n-2..0] $ \i ->
    forM_ [i+1,i+2..n] $ \ii ->
      forM_ [n-1,n-2..0] $ \j ->
        forM_ [j+1,j+2..n] $ \jj -> do
          modifyIORef cookie (+1)
          stamp <- readIORef cookie
          cnt <- g i j ii jj
          if cnt == 0
             then
               MV.unsafeWrite nim (f i j ii jj) 0
             else do
               forM_ [i+1,i+2..ii-1] $ \si -> do
                 t0 <- MV.unsafeRead nim $ f i j si jj
                 t1 <- MV.unsafeRead nim $ f si j ii jj
                 MV.unsafeWrite mex (xor t0 t1) stamp
               forM_ [j+1,j+2..jj-1] $ \sj -> do
                 t0 <- MV.unsafeRead nim $ f i j ii sj
                 t1 <- MV.unsafeRead nim $ f i sj ii jj
                 MV.unsafeWrite mex (xor t0 t1) stamp
          let go t = do
                v <- MV.unsafeRead mex t
                if v /= stamp
                   then return t
                   else go $ t+1
          go 0 >>= MV.unsafeWrite nim (f i j ii jj)
          --go 0 >>= printf "%d %d %d %d -- %d\n" i j ii jj
  MV.unsafeRead nim $ f 0 0 n n

main = do
  cases <- readLn
  replicateM cases $ do
    n <- readLn
    ts <- concat <$> replicateM n (fmap (map read . words) getLine) :: IO [Int]
    nim <- calc n (V.fromList ts)
    putStrLn $ if nim == 0 then "Second" else "First"
