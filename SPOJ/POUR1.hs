import Control.Monad

run :: [Int] -> IO ()
run [] = return ()
run (a:b:c:xs) = do
    print $ solve a b c `min` solve b a c
    run xs

solve :: Int -> Int -> Int -> Int
solve a b c = if c > max a b || c `mod` gcd a b /= 0 then -1 else f 0 0 0
    where f x y res
            | x == c || y == c = res
            | x == 0 = f a y (res+1)
            | y == b = f x 0 (res+1)
            | x+y <= b = f 0 (y+x) (res+1)
            | otherwise = f (x-(b-y)) b (res+1)

main = do
     content <- liftM (fmap read . tail . lines) getContents
     run content