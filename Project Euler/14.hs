import Data.List
import Data.Array.IO
import Data.Array.Base
import Data.Ord

maxn = 1000000

collatz :: (IOUArray Int Int) -> Integer -> IO Int
collatz a 1 = return 1
collatz a n = do
        ans <- if n <= maxn
            then unsafeRead a (fromIntegral n) :: IO Int
            else return 0
--        print $ show n ++ " " ++ show ans

        if (ans > 0)
           then return ans
           else do
                ans <- collatz a (if (even n) then n `div` 2 else n * 3 + 1)
                if n <= maxn
                   then unsafeWrite a (fromIntegral n) (ans+1)
                   else return ()
                return (ans+1)

main = do
     a <- newArray (1,(fromIntegral maxn)+1) 0 :: IO (IOUArray Int Int)
     print . snd . maximumBy (comparing fst) =<<
           sequence(map (\x -> collatz a x >>= (\v -> return (v,x))) [1..maxn])
