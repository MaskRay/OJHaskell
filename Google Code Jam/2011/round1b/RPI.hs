import Control.Monad
import Control.Monad.Instances

main = do
  cases <- liftM read getLine
  forM_ [1..cases] $ \cc -> do
    n <- liftM read getLine
    schedule <- forM [1..n] (const getLine)
    let total = map (\x -> (fromIntegral $ length $ filter (/='.') $ schedule!!x)) [0..n-1]
        wins = map (\x -> (fromIntegral $ length $ filter (=='1') $ schedule!!x)) [0..n-1]
        wp = zipWith (/) wins total
        owp = [sum [case schedule!!x!!y of
                       '.' -> 0
                       '1' -> wins!!y / (total!!y-1)
                       '0' -> (wins!!y-1) / (total!!y-1)
                   | y <- [0..n-1]] / total!!x
              | x <- [0..n-1]]
        oowp = [sum [case schedule!!x!!y of
                        '.' -> 0
                        _ -> owp!!y
                    | y <- [0..n-1]] / total!!x
               | x <- [0..n-1]]
    putStrLn $ "Case #" ++ show cc ++ ":"
    forM_ [0..n-1] $ print . liftM3 (((+).).(+)) ((/4).(wp!!)) ((/2).(owp!!)) ((/4).(oowp!!))