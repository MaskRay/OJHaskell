import Control.Monad
import Text.Printf

solve :: [String] -> [String] -> [(String, Int)]
solve es [] = [(e, 0) | e <- es]
solve es (q:qs) = [(e, f e) | e <- es, e /= q]
  where
    solve' = solve es qs
    f e = minimum [v + if e == e' then 0 else 1 | (e', v) <- solve']

main = do
  liftM read getLine >>= return . enumFromTo (1 :: Int) >>= mapM_
    (\cas -> do
        engines <- liftM read getLine >>= flip replicateM getLine
        queries <- liftM read getLine >>= flip replicateM getLine
        printf "Case #%d: %d\n" cas (minimum $ map snd $ solve engines queries))